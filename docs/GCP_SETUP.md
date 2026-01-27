# GCP Cloud Infrastructure Setup Guide

Complete guide for setting up Google Cloud Platform infrastructure for ErlMCP TAI Autonomic System.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [GCP Project Setup](#gcp-project-setup)
3. [Local Environment Configuration](#local-environment-configuration)
4. [Terraform Initialization](#terraform-initialization)
5. [Infrastructure Deployment](#infrastructure-deployment)
6. [Secrets Management](#secrets-management)
7. [GitHub Actions Integration](#github-actions-integration)
8. [Verification & Testing](#verification--testing)
9. [Troubleshooting](#troubleshooting)
10. [Monitoring & Maintenance](#monitoring--maintenance)

## Prerequisites

### Required Accounts
- **GCP Account**: Active Google Cloud Platform account with billing enabled
- **GitHub Account**: Repository access for CI/CD integration
- **Billing Account**: Active GCP billing account (required for resource provisioning)

### Required Software
- **gcloud CLI**: Google Cloud SDK v450+
- **Terraform**: v1.0 or higher
- **kubectl**: v1.28+ (for Cloud Run interactions)
- **Docker**: v24+ (for container image building)
- **git**: v2.40+
- **jq**: JSON processor (optional but recommended)

### Required Permissions
- GCP: Project Editor role (or equivalent custom role)
- GitHub: Admin access to repository settings

## GCP Project Setup

### Step 1: Create GCP Project

```bash
# Set project variables
export PROJECT_ID="taiea-v1"
export PROJECT_NAME="TAI Autonomic System v1"
export BILLING_ACCOUNT_ID="YOUR_BILLING_ACCOUNT_ID"

# Create the project
gcloud projects create $PROJECT_ID \
  --name="$PROJECT_NAME" \
  --set-as-default

# Link billing account
gcloud billing projects link $PROJECT_ID \
  --billing-account=$BILLING_ACCOUNT_ID

# Verify project creation
gcloud projects describe $PROJECT_ID
```

### Step 2: Set Default Configuration

```bash
# Configure gcloud defaults
gcloud config set project $PROJECT_ID
gcloud config set compute/region us-central1
gcloud config set compute/zone us-central1-a

# Verify configuration
gcloud config list
```

### Step 3: Enable Required APIs

```bash
# Enable core APIs
gcloud services enable \
  cloudresourcemanager.googleapis.com \
  iam.googleapis.com \
  compute.googleapis.com

# Terraform will enable additional APIs during apply
```

### Step 4: Create Service Account (Manual)

**Note**: Terraform creates this automatically, but you may create manually for initial setup:

```bash
# Create service account
gcloud iam service-accounts create erlmcp-deploy \
  --display-name="ErlMCP Deployment Service Account" \
  --description="Service account for CI/CD deployment to Cloud Run"

# Get the service account email
SA_EMAIL=$(gcloud iam service-accounts list \
  --filter="email:erlmcp-deploy@" \
  --format='value(email)')

echo "Service Account Email: $SA_EMAIL"
```

## Local Environment Configuration

### Step 1: Install gcloud CLI

**macOS (Homebrew)**:
```bash
brew install --cask google-cloud-sdk

# Initialize SDK
gcloud init
```

**Linux (Ubuntu/Debian)**:
```bash
curl https://sdk.cloud.google.com | bash
exec -l $SHELL
gcloud init
```

**Windows (Chocolatey)**:
```powershell
choco install google-cloud-sdk
gcloud init
```

### Step 2: Install Terraform

**macOS (Homebrew)**:
```bash
brew tap hashicorp/tap
brew install hashicorp/tap/terraform
```

**Linux (Ubuntu/Debian)**:
```bash
wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
sudo apt update && sudo apt install terraform
```

### Step 3: Authenticate with GCP

```bash
# Login with your Google account
gcloud auth login

# Set up Application Default Credentials (for local development)
gcloud auth application-default login

# Verify authentication
gcloud auth list
gcloud config get-value project
```

### Step 4: Create Project Working Directory

```bash
# Navigate to project
cd /Users/sac/erlmcp

# Verify GCP Terraform files exist
ls -la gcp/
# Should show: main.tf variables.tf outputs.tf project.tfvars README.md

# Create terraform working directory
mkdir -p .terraform/state
```

## Terraform Initialization

### Step 1: Initialize Terraform (Local Backend)

For initial setup, use local state:

```bash
cd /Users/sac/erlmcp/gcp

# Initialize with local backend
terraform init

# Verify initialization
terraform version
terraform validate
```

**Expected output**:
```
Terraform has been successfully configured!
```

### Step 2: Format Terraform Files

```bash
# Format all Terraform files
terraform fmt -recursive

# Check format compliance
terraform fmt -check -recursive
```

### Step 3: Validate Configuration

```bash
# Validate syntax and configuration
terraform validate
```

**Expected output**:
```
Success! The configuration is valid.
```

### Step 4: Plan Infrastructure

```bash
# Generate and review execution plan
terraform plan -var-file=project.tfvars -out=tfplan

# View detailed plan
terraform show tfplan

# Save plan summary
terraform show -json tfplan > tfplan.json
```

**Review the plan carefully** - look for:
- Service Account creation
- IAM role assignments
- Artifact Registry repository
- Secret Manager secrets
- Cloud KMS resources

## Infrastructure Deployment

### Step 1: Review Pre-Deployment Checklist

Before applying, verify:

```bash
# Verify project ID
gcloud config get-value project
# Expected: taiea-v1

# Verify authentication
gcloud auth list
# Expected: * (active account should be shown)

# Verify no existing resources conflict
gcloud resource-manager projects describe $PROJECT_ID
```

### Step 2: Apply Terraform Configuration

```bash
cd /Users/sac/erlmcp/gcp

# Apply the infrastructure (requires plan file)
terraform apply tfplan

# Or apply with auto-approve (for CI/CD)
terraform apply -var-file=project.tfvars -auto-approve
```

**Expected output**:
```
Apply complete! Resources added: 18, changed: 0, destroyed: 0.
```

### Step 3: Retrieve and Save Outputs

```bash
# Display all outputs
terraform output

# Get specific outputs
terraform output service_account_email
terraform output artifact_registry_repo_url

# Save outputs to file
terraform output -json > infrastructure_outputs.json

# Export as environment variables
export GCP_SA_EMAIL=$(terraform output -raw service_account_email)
export GCP_REGISTRY=$(terraform output -raw artifact_registry_repo_url)
```

### Step 4: Verify Resource Creation

```bash
# List created resources
gcloud resource-manager search-resources --query='project_id=taiea-v1'

# Check service account
gcloud iam service-accounts list --filter="email:erlmcp-deploy@"

# Check Artifact Registry
gcloud artifacts repositories list --location=us-central1

# Check Secret Manager secrets
gcloud secrets list

# Check KMS keys
gcloud kms keyrings list --location=us-central1
```

## Secrets Management

### Step 1: Prepare Secrets

Create or gather the following sensitive values:

```bash
# 1. Database Connection String
DATABASE_URL="postgresql://user:password@host:5432/erlmcp"

# 2. API Keys (JSON format)
API_KEYS='{"openai": "sk-...", "anthropic": "sk-..."}'

# 3. Deployment Credentials (base64 encoded if needed)
DEPLOYMENT_CREDS='{"username": "user", "password": "pass"}'
```

### Step 2: Store Secrets in Secret Manager

```bash
# Store database URL
echo -n "$DATABASE_URL" | gcloud secrets versions add erlmcp-database-url --data-file=-

# Store API keys
echo -n "$API_KEYS" | gcloud secrets versions add erlmcp-api-keys --data-file=-

# Store deployment credentials
echo -n "$DEPLOYMENT_CREDS" | gcloud secrets versions add erlmcp-deployment-creds --data-file=-

# Verify secrets stored
gcloud secrets list
gcloud secrets versions list erlmcp-database-url
```

### Step 3: Verify Secret Access

```bash
# Test service account access to secrets
SA_EMAIL=$(terraform output -raw service_account_email)

# Grant permissions (Terraform does this automatically)
gcloud secrets add-iam-policy-binding erlmcp-database-url \
  --member="serviceAccount:$SA_EMAIL" \
  --role="roles/secretmanager.secretAccessor"

# Verify access
gcloud secrets get-iam-policy erlmcp-database-url
```

### Step 4: Retrieve Secrets in Application

```bash
# From Cloud Run container
gcloud secrets versions access latest --secret="erlmcp-database-url"

# From local environment (for testing)
export DATABASE_URL=$(gcloud secrets versions access latest --secret="erlmcp-database-url")
```

## GitHub Actions Integration

### Step 1: Set Up Workload Identity Federation

```bash
# Enable required APIs
gcloud services enable iamcredentials.googleapis.com cloudidentity.googleapis.com

# Create Workload Identity Pool
gcloud iam workload-identity-pools create "github-pool" \
  --project="$PROJECT_ID" \
  --location="global" \
  --display-name="GitHub Pool"

# Get Workload Identity Pool resource name
WORKLOAD_IDENTITY_POOL_ID=$(gcloud iam workload-identity-pools describe \
  "github-pool" --project="$PROJECT_ID" --location="global" \
  --format='value(name)')

# Create Workload Identity Provider
gcloud iam workload-identity-pools providers create-oidc "github-provider" \
  --project="$PROJECT_ID" \
  --location="global" \
  --workload-identity-pool="github-pool" \
  --display-name="GitHub Provider" \
  --attribute-mapping="google.subject=assertion.sub,assertion.aud=assertion.aud,assertion.repository=assertion.repository" \
  --issuer-uri="https://token.actions.githubusercontent.com"

# Get the provider resource name
WORKLOAD_IDENTITY_PROVIDER_ID=$(gcloud iam workload-identity-pools providers describe \
  "github-provider" --project="$PROJECT_ID" --location="global" \
  --workload-identity-pool="github-pool" --format='value(name)')

echo "Workload Identity Provider: $WORKLOAD_IDENTITY_PROVIDER_ID"
```

### Step 2: Configure Service Account for GitHub

```bash
# Get service account email
SA_EMAIL=$(terraform output -raw service_account_email)

# Allow GitHub to impersonate the service account
gcloud iam service-accounts add-iam-policy-binding "$SA_EMAIL" \
  --project="$PROJECT_ID" \
  --role="roles/iam.workloadIdentityUser" \
  --member="principalSet://iam.googleapis.com/$WORKLOAD_IDENTITY_PROVIDER_ID/attribute.repository/seanchatmangpt/erlmcp"

# Verify binding
gcloud iam service-accounts get-iam-policy "$SA_EMAIL"
```

### Step 3: Configure GitHub Repository Secrets

Add these secrets to your GitHub repository (Settings → Secrets and variables → Actions):

```bash
# Copy these values to GitHub Secrets

# 1. GCP_PROJECT_ID
GCP_PROJECT_ID="taiea-v1"

# 2. GCP_SERVICE_ACCOUNT_EMAIL
GCP_SERVICE_ACCOUNT_EMAIL=$(terraform output -raw service_account_email)

# 3. GCP_WORKLOAD_IDENTITY_PROVIDER (from Step 1)
GCP_WORKLOAD_IDENTITY_PROVIDER="$WORKLOAD_IDENTITY_PROVIDER_ID"
```

**GitHub Secrets to add**:
| Secret Name | Value | Example |
|-------------|-------|---------|
| GCP_PROJECT_ID | Project ID | taiea-v1 |
| GCP_SERVICE_ACCOUNT_EMAIL | Service account email | erlmcp-deploy@taiea-v1.iam.gserviceaccount.com |
| GCP_WORKLOAD_IDENTITY_PROVIDER | WIF Provider resource name | projects/123456/locations/global/workloadIdentityPools/github-pool/providers/github-provider |
| SLACK_WEBHOOK_URL | Slack notification webhook | https://hooks.slack.com/services/... |

### Step 4: Test GitHub Actions Integration

```bash
# Trigger workflow manually
gh workflow run gcp-deploy.yml \
  --ref main \
  --field environment=production

# Monitor workflow execution
gh run watch

# View workflow logs
gh run view [RUN_ID] --log
```

## Verification & Testing

### Step 1: Verify All Resources Created

```bash
# Check service account
gcloud iam service-accounts describe erlmcp-deploy@$PROJECT_ID.iam.gserviceaccount.com

# Check Artifact Registry
gcloud artifacts repositories describe erlmcp-tai-repo \
  --location=us-central1

# Check Secret Manager
gcloud secrets list

# Check Cloud KMS
gcloud kms keyrings list --location=us-central1
gcloud kms keys list --location=us-central1 --keyring=erlmcp-tai-keyring
```

### Step 2: Test IAM Permissions

```bash
# List service account roles
gcloud projects get-iam-policy $PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:erlmcp-deploy@"

# Test Cloud Run Admin permission
gcloud run services list --region=us-central1
```

### Step 3: Test Secret Access

```bash
# Impersonate service account and test secret access
gcloud secrets versions access latest \
  --secret="erlmcp-database-url" \
  --impersonate-service-account=erlmcp-deploy@$PROJECT_ID.iam.gserviceaccount.com
```

### Step 4: Create Test Deployment

```bash
# Build test Docker image
docker build -t erlmcp-test .

# Tag for Artifact Registry
REGISTRY_URL=$(terraform output -raw artifact_registry_repo_url)
docker tag erlmcp-test:latest $REGISTRY_URL/erlmcp-test:latest

# This will be tested in Agent 8 (Cloud Run Deployment)
```

## Troubleshooting

### Issue: "The [X] API is not enabled for project"

**Solution**:
```bash
# Enable the missing API
gcloud services enable [API_NAME]

# Or let Terraform enable it
terraform apply -var-file=project.tfvars
```

### Issue: "Permission denied" during terraform apply

**Solution**:
```bash
# Verify authentication
gcloud auth login
gcloud auth application-default login

# Verify project set
gcloud config set project taiea-v1

# Verify IAM permissions
gcloud projects get-iam-policy taiea-v1
```

### Issue: Terraform state conflict

**Solution**:
```bash
# Check state lock
terraform state list

# Force unlock if necessary (use with caution)
terraform force-unlock [LOCK_ID]

# Refresh state
terraform refresh -var-file=project.tfvars
```

### Issue: Service account not found

**Solution**:
```bash
# Verify service account exists
gcloud iam service-accounts list

# If missing, create it
gcloud iam service-accounts create erlmcp-deploy \
  --display-name="ErlMCP Deployment Service Account"

# Or reapply Terraform
terraform apply -target=google_service_account.erlmcp_deploy
```

### Issue: Secret Manager permission denied

**Solution**:
```bash
# Grant service account secret accessor role
SA_EMAIL=$(terraform output -raw service_account_email)
gcloud secrets add-iam-policy-binding [SECRET_NAME] \
  --member="serviceAccount:$SA_EMAIL" \
  --role="roles/secretmanager.secretAccessor"
```

## Monitoring & Maintenance

### Enable Cloud Logging

```bash
# View recent logs
gcloud logging read "resource.type=service_account" \
  --limit 50 --format json

# Create log sink for Terraform operations
gcloud logging sinks create terraform-logs \
  logging.googleapis.com/terraform-logs \
  --log-filter='resource.type="service_account"'
```

### Monitor Service Account Usage

```bash
# Get service account activity
gcloud logging read "protoPayload.authenticationInfo.principalEmail=erlmcp-deploy@$PROJECT_ID.iam.gserviceaccount.com" \
  --limit=50 --format=table
```

### Backup Terraform State

```bash
# Manual backup
cp terraform.tfstate terraform.tfstate.backup

# Upload to GCS (recommended)
gsutil cp terraform.tfstate \
  gs://taiea-v1-terraform-state/terraform.tfstate
```

### Schedule Maintenance

Regular maintenance tasks:
- Weekly: Review IAM policies and service account permissions
- Monthly: Check for unused resources and cost optimization
- Quarterly: Review KMS key rotation and secret expiration
- Annually: Full infrastructure audit and compliance review

## Next Steps

1. **Agent 7**: Terraform initialization and plan validation
2. **Agent 8**: Cloud Run service deployment
3. **Agent 9**: Monitoring and alerting setup
4. **Agent 10**: Database and migrations
5. **Agent 11**: Load balancing and DNS

## Resources

- [GCP Documentation](https://cloud.google.com/docs)
- [Terraform GCP Provider](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Cloud Run Guide](https://cloud.google.com/run/docs)
- [Secret Manager Guide](https://cloud.google.com/secret-manager/docs)
- [Workload Identity Federation](https://cloud.google.com/iam/docs/workload-identity-federation)

---

**Last Updated**: January 26, 2026
**Agent**: GCP Cloud Infrastructure Setup (Agent 6/20)
**Status**: Infrastructure configuration files created and documented
