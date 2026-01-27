# GCP Infrastructure Configuration for ErlMCP TAI

This directory contains Terraform configuration for setting up Google Cloud Platform infrastructure for the ErlMCP Autonomic AI system.

## Architecture Overview

The configuration provisions:
- **Service Account**: Dedicated service account for CI/CD deployments with least-privilege IAM roles
- **Artifact Registry**: Docker container registry for storing built images
- **Cloud KMS**: Key management for encrypting secrets
- **Secret Manager**: Secure storage for sensitive configuration (database URLs, API keys, credentials)
- **Cloud Storage**: Bucket for Terraform state with versioning and access controls
- **Enabled APIs**: All required GCP services for the deployment

## File Structure

```
gcp/
├── main.tf              # Main infrastructure configuration
├── variables.tf         # Variable definitions
├── outputs.tf           # Output values
├── project.tfvars       # Project-specific variables
└── README.md            # This file
```

## Prerequisites

### Required Tools
- **gcloud CLI**: Google Cloud SDK (https://cloud.google.com/sdk/docs/install)
- **Terraform**: v1.0+ (https://www.terraform.io/downloads.html)
- **Billing Account**: Active GCP billing account

### Setup Steps

1. **Install gcloud CLI**:
   ```bash
   # macOS using Homebrew
   brew install --cask google-cloud-sdk

   # Or download from: https://cloud.google.com/sdk/docs/install
   ```

2. **Authenticate with GCP**:
   ```bash
   gcloud auth login
   gcloud config set project taiea-v1
   ```

3. **Verify Billing Account**:
   ```bash
   gcloud billing accounts list
   ```

4. **Install Terraform**:
   ```bash
   # macOS using Homebrew
   brew tap hashicorp/tap
   brew install hashicorp/tap/terraform

   # Or download from: https://www.terraform.io/downloads.html
   ```

5. **Verify installations**:
   ```bash
   gcloud --version
   terraform --version
   ```

## Configuration

### Update project.tfvars

Edit `/Users/sac/erlmcp/gcp/project.tfvars` to customize for your environment:

```hcl
project_id = "taiea-v1"              # Your GCP project ID
region      = "us-central1"          # GCP region
environment = "production"           # Environment name
```

### Additional Variables (optional in project.tfvars)

```hcl
billing_account = "XXXXXX-XXXXXX-XXXXXX"  # GCP billing account ID
app_name        = "erlmcp-tai"             # Application name
service_account_name = "erlmcp-deploy"    # Service account name
```

## Terraform Workflow

### 1. Initialize Terraform

```bash
cd /Users/sac/erlmcp/gcp

# First time initialization (local state)
terraform init

# With remote state backend (after gcs bucket exists)
terraform init \
  -backend-config='bucket=taiea-v1-terraform-state' \
  -backend-config='prefix=prod'
```

**What this does**:
- Downloads provider plugins (google, google-beta)
- Initializes local or remote state
- Validates Terraform configuration

### 2. Validate Configuration

```bash
terraform validate
```

**What this does**:
- Checks syntax and configuration validity
- Verifies provider definitions
- Reports any errors

### 3. Plan Infrastructure Changes

```bash
terraform plan -var-file=project.tfvars -out=tfplan
```

**What this does**:
- Displays what resources will be created
- Shows IAM assignments
- Estimates resource costs

### 4. Apply Configuration

```bash
terraform apply tfplan
```

**What this does**:
- Creates all resources defined in main.tf
- Configures IAM roles
- Outputs service account and resource IDs

### 5. View Outputs

```bash
terraform output
```

**Key outputs**:
- `service_account_email`: Email address of deployment service account
- `artifact_registry_repo_url`: Docker registry URL
- `kms_keyring_id`: Cloud KMS key ring
- `terraform_state_bucket_name`: GCS bucket for state

## Secret Management Setup

After `terraform apply`, populate Secret Manager with sensitive values:

### 1. Database URL Secret

```bash
gcloud secrets versions add erlmcp-database-url \
  --data-file=- <<< "your-database-connection-string"
```

### 2. API Keys Secret

```bash
gcloud secrets versions add erlmcp-api-keys \
  --data-file=- <<< "your-api-keys-json"
```

### 3. Deployment Credentials

```bash
gcloud secrets versions add erlmcp-deployment-creds \
  --data-file=- <<< "your-deployment-credentials"
```

## GitHub Actions Integration

The configuration enables GitHub Actions deployment through:

1. **Service Account Key**: Used by GitHub Actions to authenticate
2. **IAM Roles**: Service account has Cloud Run Admin, Artifact Registry Writer, Secret Manager Accessor
3. **CI/CD Pipeline**: `.github/workflows/gcp-deploy.yml` handles authentication and deployment

### Setting Up GitHub Secrets

1. Generate service account key:
   ```bash
   gcloud iam service-accounts keys create key.json \
     --iam-account=erlmcp-deploy@taiea-v1.iam.gserviceaccount.com
   ```

2. Add to GitHub repository secrets:
   - `GCP_PROJECT_ID`: taiea-v1
   - `GCP_SERVICE_ACCOUNT_EMAIL`: erlmcp-deploy@taiea-v1.iam.gserviceaccount.com
   - `GCP_SERVICE_ACCOUNT_KEY`: Contents of key.json (base64 encoded)

3. Store the key file securely (don't commit to repository)

## Resource Details

### Service Account

- **Name**: erlmcp-deploy
- **Email**: erlmcp-deploy@[PROJECT_ID].iam.gserviceaccount.com
- **Roles**:
  - `roles/run.admin` - Deploy and manage Cloud Run services
  - `roles/iam.serviceAccountUser` - Run Cloud Run services as this account
  - `roles/artifactregistry.writer` - Push images to Artifact Registry
  - `roles/secretmanager.secretAccessor` - Read secrets from Secret Manager
  - `roles/cloudkms.cryptoKeyEncrypterDecrypter` - Encrypt/decrypt with KMS
  - `roles/cloudbuild.builds.editor` - Build and manage container images

### Artifact Registry

- **Format**: Docker
- **Location**: [REGION]-docker.pkg.dev
- **URL**: `[REGION]-docker.pkg.dev/[PROJECT_ID]/erlmcp-tai-repo`
- **Credentials**: Service account with writer access

### Cloud KMS

- **Key Ring**: erlmcp-tai-keyring
- **Crypto Key**: erlmcp-tai-key
- **Rotation**: 90 days (automatic)
- **Purpose**: ENCRYPT_DECRYPT for secret encryption

### Secrets Manager

Three secrets are configured:
1. **erlmcp-database-url** - Database connection string
2. **erlmcp-api-keys** - API authentication keys
3. **erlmcp-deployment-creds** - Deployment credentials

## Monitoring & Logging

The configuration enables these APIs for observability:
- Cloud Logging (logging.googleapis.com)
- Cloud Monitoring (monitoring.googleapis.com)
- Cloud Trace (cloudtrace.googleapis.com)

Agent 9 will configure dashboards and alerting.

## Common Operations

### Destroy Infrastructure (⚠️ Careful!)

```bash
terraform destroy -var-file=project.tfvars
```

**Warning**: This will delete all GCP resources created by this configuration.

### Update Service Account IAM Role

```bash
terraform apply -var-file=project.tfvars -target=google_project_iam_member.[role_name]
```

### Rotate KMS Key

Manual rotation:
```bash
gcloud kms keys versions create \
  --location=us-central1 \
  --keyring=erlmcp-tai-keyring \
  --key=erlmcp-tai-key
```

Automatic rotation is configured for 90 days.

### Retrieve Outputs

```bash
# Get service account email
terraform output service_account_email

# Get Artifact Registry URL
terraform output artifact_registry_repo_url

# Get all outputs as JSON
terraform output -json
```

## Troubleshooting

### Provider Configuration Error

**Error**: `Error: Failed to configure Google Cloud credentials`

**Solution**:
```bash
gcloud auth application-default login
gcloud config set project taiea-v1
```

### API Not Enabled

**Error**: `The [X] API is not enabled for project`

**Solution**: This is automatically handled by terraform. Run `terraform apply` again.

### Billing Account Required

**Error**: `Billing account not configured`

**Solution**:
```bash
gcloud billing accounts list
gcloud billing project-link --billing-account=BILLING_ACCOUNT_ID taiea-v1
```

### State Lock Issues

**Error**: `Error acquiring the state lock`

**Solution**:
```bash
terraform force-unlock [LOCK_ID]
```

## Next Steps (Agents 7+)

1. **Agent 7**: Terraform initialization and plan review
2. **Agent 8**: Deploy Cloud Run services
3. **Agent 9**: Configure monitoring and alerting
4. **Agent 10**: Set up database and migrations
5. **Agent 11**: Configure load balancing and DNS

## References

- [GCP Terraform Provider Docs](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Secret Manager Documentation](https://cloud.google.com/secret-manager/docs)
- [Artifact Registry Documentation](https://cloud.google.com/artifact-registry/docs)
- [Cloud KMS Documentation](https://cloud.google.com/kms/docs)

## Support

For issues or questions:
1. Check GCP console for resource creation status
2. Review Terraform logs: `TF_LOG=DEBUG terraform plan`
3. Check gcloud configuration: `gcloud config list`
4. Review service account permissions in GCP IAM console
