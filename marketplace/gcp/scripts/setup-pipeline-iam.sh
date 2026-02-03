#!/bin/bash
# ============================================================================
# Pipeline IAM Setup Script
# Configures required IAM roles for the GCP Marketplace CI/CD Pipeline
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
REGION="${REGION:-us-central1}"

# Required roles for Cloud Build Service Account
declare -a BUILD_SA_ROLES=(
    "roles/cloudbuild.builds.builder"
    "roles/artifactregistry.writer"
    "roles/containeranalysis.notes.attacher"
    "roles/containeranalysis.notes.viewer"
    "roles/containeranalysis.occurrences.viewer"
    "roles/containeranalysis.occurrences.editor"
    "roles/storage.objectAdmin"
    "roles/iam.serviceAccountUser"
    "roles/logging.logWriter"
    "roles/monitoring.metricWriter"
    "roles/pubsub.publisher"
    "roles/secretmanager.secretViewer"
    "roles/workflows.invoker"
)

# Additional roles for deployment testing
declare -a DEPLOYMENT_ROLES=(
    "roles/container.developer"
    "roles/run.admin"
    "roles/compute.instanceAdmin"
    "roles/compute.networkAdmin"
)

# ============================================================================
# Functions
# ============================================================================

check_gcloud() {
    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found"
        log_info "Install from: https://cloud.google.com/sdk/docs/install"
        exit 1
    fi

    # Check if user is authenticated
    if ! gcloud auth list --filter="status:ACTIVE" --format="value(account)" | grep -q .; then
        log_error "Not authenticated with gcloud"
        log_info "Run: gcloud auth login"
        exit 1
    fi

    log_info "✓ gcloud CLI available and authenticated"
}

create_service_account() {
    log_info "Creating Cloud Build service account..."

    # Check if SA already exists
    if gcloud iam service-accounts describe "$BUILD_SA_EMAIL" \
        --project="$PROJECT_ID" &>/dev/null; then
        log_warn "Service account already exists: $BUILD_SA_EMAIL"
        return 0
    fi

    gcloud iam service-accounts create "$BUILD_SA_NAME" \
        --display-name="Cloud Build Marketplace SA" \
        --description="Service account for GCP Marketplace CI/CD pipeline" \
        --project="$PROJECT_ID"

    log_info "✓ Created service account: $BUILD_SA_EMAIL"
}

grant_build_roles() {
    log_info "Granting roles to Cloud Build SA..."

    for role in "${BUILD_SA_ROLES[@]}"; do
        log_info "  Granting: $role"
        gcloud projects add-iam-policy-binding "$PROJECT_ID" \
            --member="serviceAccount:$BUILD_SA_EMAIL" \
            --role="$role" \
            --condition=None \
            --project="$PROJECT_ID" 2>/dev/null || {
                log_warn "    Failed to grant (may already have): $role"
            }
    done

    log_info "✓ Build roles granted"
}

grant_deployment_roles() {
    log_info "Granting deployment test roles..."

    # Create a conditional binding for deployment testing
    # Only applies to resources with the "marketplace-test" tag

    local condition="resource.matchTag('${PROJECT_ID}/marketplace-test', 'true')"

    for role in "${DEPLOYMENT_ROLES[@]}"; do
        log_info "  Granting (conditional): $role"
        gcloud projects add-iam-policy-binding "$PROJECT_ID" \
            --member="serviceAccount:$BUILD_SA_EMAIL" \
            --role="$role" \
            --condition="title=Marketplace Test Resources,expression=$condition" \
            --project="$PROJECT_ID" 2>/dev/null || {
                log_warn "    Failed to grant (may already have): $role"
            }
    done

    log_info "✓ Deployment roles granted (conditional)"
}

enable_apis() {
    log_info "Enabling required APIs..."

    declare -a REQUIRED_APIS=(
        "cloudbuild.googleapis.com"
        "artifactregistry.googleapis.com"
        "containeranalysis.googleapis.com"
        "containerscanning.googleapis.com"
        "storage.googleapis.com"
        "pubsub.googleapis.com"
        "secretmanager.googleapis.com"
        "cloudkms.googleapis.com"
        "run.googleapis.com"
        "container.googleapis.com"
        "compute.googleapis.com"
        "monitoring.googleapis.com"
        "logging.googleapis.com"
    )

    for api in "${REQUIRED_APIS[@]}"; do
        log_info "  Enabling: $api"
        gcloud services enable "$api" --project="$PROJECT_ID" 2>/dev/null || {
            log_warn "    Failed to enable (may already be enabled): $api"
        }
    done

    log_info "✓ APIs enabled"
}

create_test_tag() {
    log_info "Creating test resource tag..."

    # Create tag for test resources
    gcloud resource-manager tags keys create marketplace-test \
        --project="$PROJECT_ID" 2>/dev/null || {
        log_warn "Tag key may already exist"
    }

    gcloud resource-manager tags values create true \
        --parent=projects/$PROJECT_ID \
        --short-name=true \
        --marketplace-test 2>/dev/null || {
        log_warn "Tag value may already exist"
    }

    log_info "✓ Test tag created"
}

configure_kms() {
    log_info "Configuring Cloud KMS for receipt signing..."

    local keyring_name="cloudbuild"
    local key_name="build-signing-key"

    # Create keyring
    gcloud kms keyrings create "$keyring_name" \
        --location global \
        --project="$PROJECT_ID" 2>/dev/null || {
        log_warn "KMS keyring may already exist"
    }

    # Create asymmetric signing key
    gcloud kms asymmetric-keys create "$key_name" \
        --keyring "$keyring_name" \
        --location global \
        --project="$PROJECT_ID" \
        --key-algorithm rsa-sign-pkcs1-2048-sha256 \
        --protection-level software 2>/dev/null || {
        log_warn "KMS key may already exist"
    }

    # Grant Cloud Build SA access to the key
    gcloud kms keys add-iam-policy-binding "$key_name" \
        --keyring "$keyring_name" \
        --location global \
        --project="$PROJECT_ID" \
        --member="serviceAccount:$BUILD_SA_EMAIL" \
        --role="roles/cloudkms.signerVerifier" 2>/dev/null || {
        log_warn "May already have KMS permissions"
    }

    log_info "✓ KMS configured"
}

create_bucket() {
    log_info "Creating evidence storage bucket..."

    local bucket_name="${PROJECT_ID}-marketplace-evidence"

    if gsutil ls -b "gs://$bucket_name" &>/dev/null; then
        log_warn "Bucket already exists: gs://$bucket_name"
        return 0
    fi

    gsutil mb -p "$PROJECT_ID" -l "$REGION" "gs://$bucket_name"

    # Set lifecycle policy (1 year retention)
    cat > /tmp/lifecycle.json << EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {
          "type": "Delete"
        },
        "condition": {
          "age": 365
        }
      }
    ]
  }
}
EOF

    gsutil lifecycle set /tmp/lifecycle.json "gs://$bucket_name"
    rm /tmp/lifecycle.json

    log_info "✓ Created bucket: gs://$bucket_name"
}

create_notification_topic() {
    log_info "Creating Pub/Sub topic for notifications..."

    local topic_name="build-notifications"

    if gcloud pubsub topics describe "$topic_name" --project="$PROJECT_ID" &>/dev/null; then
        log_warn "Topic already exists: $topic_name"
    else
        gcloud pubsub topics create "$topic_name" --project="$PROJECT_ID"
    fi

    # Grant Cloud Build SA permission to publish
    gcloud pubsub topics add-iam-policy-binding "$topic_name" \
        --project="$PROJECT_ID" \
        --member="serviceAccount:$BUILD_SA_EMAIL" \
        --role="roles/pubsub.publisher" 2>/dev/null || true

    log_info "✓ Pub/Sub topic configured"
}

print_summary() {
    echo ""
    echo "=========================================="
    echo "IAM Setup Summary"
    echo "=========================================="
    log_info "Project ID:        $PROJECT_ID"
    log_info "Service Account:   $BUILD_SA_EMAIL"
    log_info "Region:            $REGION"
    echo ""
    log_info "Next steps:"
    echo "  1. Create Slack webhook secret:"
    echo "     echo 'https://hooks.slack.com/services/YOUR/WEBHOOK/URL' | \\"
    echo "       gcloud secrets create slack-webhook-url --data-file=-"
    echo ""
    echo "  2. Grant secret access to build SA:"
    echo "     gcloud secrets add-iam-policy-binding slack-webhook-url \\"
    echo "       --member=serviceAccount:$BUILD_SA_EMAIL \\"
    echo "       --role=roles/secretmanager.secretAccessor"
    echo ""
    echo "  3. Create build trigger:"
    echo "     gcloud builds triggers create github \\"
    echo "       --name=marketplace-ci \\"
    echo "       --repo-name=erlmcp \\"
    echo "       --repo-owner=banyan-platform \\"
    echo "       --branch-pattern='^main\$' \\"
    echo "       --build-config=marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml"
    echo "=========================================="
}

# ============================================================================
# Main
# ============================================================================

main() {
    echo "=========================================="
    echo "GCP Marketplace Pipeline IAM Setup"
    echo "=========================================="

    check_gcloud

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    create_service_account
    grant_build_roles
    grant_deployment_roles
    enable_apis
    create_test_tag
    configure_kms
    create_bucket
    create_notification_topic
    print_summary

    log_info "IAM setup completed successfully!"
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
        --region)
            REGION="$2"
            shift 2
            ;;
        --sa-name)
            BUILD_SA_NAME="$2"
            BUILD_SA_EMAIL="${BUILD_SA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT_ID] [--region REGION] [--sa-name SA_NAME]"
            exit 1
            ;;
    esac
done

main "$@"
