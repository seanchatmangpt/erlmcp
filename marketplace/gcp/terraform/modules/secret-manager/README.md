# Google Secret Manager Module - V3 Security Architecture

Enterprise-grade secret management for erlmcp with zero-trust principles, CMEK encryption, automatic rotation, and least-privilege access control.

## Features

### Security Enhancements

- **Customer-Managed Encryption Keys (CMEK)**: All critical secrets encrypted with KMS keys under your control
- **Automatic Secret Rotation**: Configurable rotation periods (30/90/365 days) with Pub/Sub notifications
- **Least-Privilege IAM**: Granular access control by workload type with IAM conditions
- **Comprehensive Audit Logging**: All secret operations logged for compliance and security monitoring
- **Multi-Region Replication**: User-managed replication for critical secrets ensures HA/DR
- **Secret Version Management**: Version aliases and lifecycle management
- **Compliance Annotations**: Built-in annotations for PCI-DSS, HIPAA, GDPR, SOC 2
- **VPC Service Controls**: Optional VPC-SC support for additional network isolation

### Secret Classifications

#### Critical Secrets (Tier 1)
- **erlang_cookie**: Erlang cluster authentication (30-day rotation)
- **tls_key**: TLS private key (event-driven rotation)
- **jwt_private_key**: JWT signing key (365-day rotation)
- **backup_key**: Backup encryption key (365-day rotation)

#### High Secrets (Tier 2)
- **db_password**: PostgreSQL authentication (90-day rotation)
- **redis_password**: Redis authentication (90-day rotation)
- **grafana_password**: Grafana authentication (90-day rotation)
- **ca_bundle**: CA certificate bundle (event-driven)

#### Medium Secrets (Tier 3)
- **jwt_public_key**: JWT verification key (365-day rotation)
- **tls_cert**: TLS certificate (event-driven)
- **otel_ca_cert**: OpenTelemetry CA (event-driven)

## Usage

### Basic Usage (with CMEK and Rotation)

```hcl
module "secret_manager" {
  source = "./modules/secret-manager"

  project_id = "my-gcp-project"

  # Enable enterprise security features
  enable_cmek                   = true
  enable_rotation               = true
  enable_rotation_notifications = true
  enable_audit_logging          = true

  # KMS configuration
  kms_location = "us-central1"

  # Multi-region replication for HA/DR
  secret_replication_locations = ["us-central1", "us-east1", "us-west1"]

  # Rotation periods by classification
  rotation_periods = {
    critical = "2592000s"   # 30 days
    high     = "7776000s"   # 90 days
    crypto   = "31536000s"  # 365 days
  }

  # Least-privilege IAM bindings by workload type
  cluster_service_accounts = [
    "serviceAccount:erlmcp-node-1@my-project.iam.gserviceaccount.com",
    "serviceAccount:erlmcp-node-2@my-project.iam.gserviceaccount.com",
    "serviceAccount:erlmcp-node-3@my-project.iam.gserviceaccount.com",
  ]

  database_service_accounts = [
    "serviceAccount:erlmcp-api@my-project.iam.gserviceaccount.com",
  ]

  cache_service_accounts = [
    "serviceAccount:erlmcp-api@my-project.iam.gserviceaccount.com",
  ]

  tls_service_accounts = [
    "serviceAccount:erlmcp-loadbalancer@my-project.iam.gserviceaccount.com",
  ]

  auth_service_accounts = [
    "serviceAccount:erlmcp-auth@my-project.iam.gserviceaccount.com",
  ]

  app_service_accounts = [
    "serviceAccount:erlmcp-api@my-project.iam.gserviceaccount.com",
    "serviceAccount:erlmcp-worker@my-project.iam.gserviceaccount.com",
  ]

  monitoring_service_accounts = [
    "serviceAccount:erlmcp-monitoring@my-project.iam.gserviceaccount.com",
  ]

  backup_service_accounts = [
    "serviceAccount:erlmcp-backup@my-project.iam.gserviceaccount.com",
  ]

  observability_service_accounts = [
    "serviceAccount:erlmcp-otel-collector@my-project.iam.gserviceaccount.com",
  ]

  # Security and audit teams (metadata-only access)
  secret_viewers = [
    "group:security-team@example.com",
    "group:audit-team@example.com",
  ]

  # Security team administrators
  secret_administrators = [
    "group:security-admins@example.com",
  ]

  # Labels for organization and compliance
  labels = {
    app         = "erlmcp"
    environment = "production"
    managed-by  = "terraform"
    compliance  = "pci-dss-hipaa-gdpr"
  }
}
```

### Minimal Usage (Development)

```hcl
module "secret_manager" {
  source = "./modules/secret-manager"

  project_id = "my-dev-project"

  # Disable CMEK for development (cost savings)
  enable_cmek    = false
  enable_rotation = false

  # Minimal IAM for development
  app_service_accounts = [
    "serviceAccount:dev-service@my-dev-project.iam.gserviceaccount.com",
  ]

  labels = {
    app         = "erlmcp"
    environment = "development"
  }
}
```

### With Secret Values (Initial Setup)

```hcl
module "secret_manager" {
  source = "./modules/secret-manager"

  project_id = "my-gcp-project"

  # Enable initial secret version creation
  create_secret_versions = true

  # Provide secret values (use encrypted tfvars in production!)
  secrets = {
    erlang_cookie    = ""  # Auto-generated if empty
    db_password      = ""  # Auto-generated if empty
    redis_password   = ""  # Auto-generated if empty
    tls_cert         = file("${path.module}/certs/tls.crt")
    tls_key          = file("${path.module}/certs/tls.key")
    ca_bundle        = file("${path.module}/certs/ca-bundle.crt")
    jwt_private_key  = file("${path.module}/keys/jwt-private.pem")
    jwt_public_key   = file("${path.module}/keys/jwt-public.pem")
    grafana_password = ""  # Auto-generated if empty
    backup_key       = ""  # Auto-generated if empty
    otel_ca_cert     = file("${path.module}/certs/otel-ca.crt")
  }

  # ... rest of configuration
}
```

## Outputs

### Secret References

```hcl
# Use secret IDs in other modules
resource "google_cloud_run_service" "api" {
  # ...

  template {
    spec {
      containers {
        env {
          name = "DB_PASSWORD"
          value_source {
            secret_key_ref {
              name = module.secret_manager.secret_names.db_password
              key  = "latest"
            }
          }
        }
      }
    }
  }
}

# GKE Secret Store CSI Driver configuration
resource "kubernetes_manifest" "secret_provider_class" {
  manifest = {
    apiVersion = "secrets-store.csi.x-k8s.io/v1"
    kind       = "SecretProviderClass"
    metadata = {
      name      = "erlmcp-secrets"
      namespace = "default"
    }
    spec = {
      provider = "gcp"
      parameters = {
        secrets = module.secret_manager.gke_secret_provider_class_spec.parameters.secrets
      }
    }
  }
}
```

### Accessing Secret Metadata

```hcl
output "rotation_config" {
  value = module.secret_manager.rotation_config
}

output "security_summary" {
  value = module.secret_manager.security_metadata
}

output "encryption_status" {
  value = module.secret_manager.encryption_config
}
```

## Secret Generation (Docker-Only)

Per erlmcp Constitution, ALL secret generation MUST occur in Docker containers:

```bash
# Generate passwords and keys
docker run --rm alpine/openssl rand -base64 48 > erlang-cookie.txt
docker run --rm alpine/openssl rand -base64 32 > db-password.txt
docker run --rm alpine/openssl rand -base64 32 > redis-password.txt
docker run --rm alpine/openssl rand -base64 32 > grafana-password.txt
docker run --rm alpine/openssl rand -base64 64 > backup-key.txt

# Generate JWT RSA-4096 key pair
docker run --rm -v $(pwd)/keys:/keys alpine/openssl genrsa -out /keys/jwt-private.pem 4096
docker run --rm -v $(pwd)/keys:/keys alpine/openssl rsa -in /keys/jwt-private.pem -pubout -out /keys/jwt-public.pem

# Generate TLS certificate (production: use Let's Encrypt or CA)
docker run --rm -v $(pwd)/certs:/certs alpine/openssl req -x509 -newkey rsa:4096 \
  -keyout /certs/tls.key -out /certs/tls.crt -days 365 -nodes \
  -subj "/CN=erlmcp.example.com/O=Example Inc/C=US"
```

## Rotation Strategy

### Automatic Rotation

The module implements automatic rotation with Pub/Sub notifications:

```
┌─────────────────┐     rotation      ┌──────────────────┐
│ Secret Manager  │─────trigger──────>│   Pub/Sub Topic  │
└─────────────────┘                   └──────────────────┘
                                              │
                                              v
                                      ┌──────────────────┐
                                      │  Cloud Function  │
                                      │  /Cloud Run Job  │
                                      └──────────────────┘
                                              │
                         ┌────────────────────┼────────────────────┐
                         v                    v                    v
                   Generate New        Update Secret        Trigger App
                   Credentials         Version              Restart
```

### Rotation Frequencies

- **Critical Secrets**: 30 days (erlang_cookie)
- **High Secrets**: 90 days (db_password, redis_password, grafana_password)
- **Crypto Keys**: 365 days (jwt_private_key, backup_key)
- **Event-Driven**: As needed (tls_cert, ca_bundle, otel_ca_cert)

### Implementing Rotation Handler

```bash
# Deploy rotation handler as Cloud Run job
docker compose -f docker-compose.rotation.yml run --rm rotation-handler \
  --secret-id=erlmcp-db-password \
  --action=rotate
```

## Security Best Practices

### 1. Never Commit Secrets

```gitignore
# .gitignore
*.txt
*.pem
*.key
*.crt
terraform.tfvars
secrets.tfvars
!example.tfvars
```

### 2. Encrypt Terraform Variables

Use SOPS or similar:

```bash
# Encrypt sensitive tfvars
docker run --rm -v $(pwd):/workspace mozilla/sops --encrypt \
  --gcp-kms projects/PROJECT/locations/LOCATION/keyRings/RING/cryptoKeys/KEY \
  /workspace/secrets.tfvars > /workspace/secrets.tfvars.enc
```

### 3. Least-Privilege IAM

Each workload type gets ONLY the secrets it needs:

- Cluster nodes: `erlang_cookie` only
- Database services: `db_password` only
- Auth services: `jwt_private_key` only
- Backup services: `backup_key` only

### 4. Monitor and Alert

Set up Cloud Monitoring alerts:

```hcl
resource "google_monitoring_alert_policy" "secret_access_anomaly" {
  display_name = "Unusual Secret Access Pattern"
  combiner     = "OR"

  conditions {
    display_name = "High secret access rate"

    condition_threshold {
      filter          = "resource.type=\"secret_manager_secret\" AND metric.type=\"secretmanager.googleapis.com/secret/access_count\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 100
      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }
}
```

## Compliance and Audit

### PCI-DSS Compliance

- ✅ Requirement 3.4: CMEK encryption at rest
- ✅ Requirement 3.5: Key management (KMS with 90-day rotation)
- ✅ Requirement 3.6: Secret rotation policies
- ✅ Requirement 8.2: Strong password generation (32+ chars)
- ✅ Requirement 10.2: Comprehensive audit logging

### HIPAA Compliance

- ✅ 164.312(a)(2)(iv): CMEK encryption
- ✅ 164.312(d): Integrity controls (versioning)
- ✅ 164.308(a)(4): Access controls (least-privilege IAM)

### GDPR Compliance

- ✅ Article 32: Security of processing (encryption, access controls)
- ✅ Article 25: Data protection by design (zero-trust architecture)

### Audit Reports

```bash
# Generate audit report
docker compose run --rm audit-tools \
  gcloud logging read 'resource.type="secretmanager.googleapis.com/Secret"' \
  --project=PROJECT_ID \
  --format=json > audit-report.json
```

## Disaster Recovery

### Multi-Region Replication

Critical secrets are replicated across multiple regions:

```
us-central1 (primary)
    ├─> us-east1 (replica)
    └─> us-west1 (replica)
```

**RTO**: < 15 minutes
**RPO**: 0 (real-time replication)

### Secret Compromise Response

1. Immediately rotate compromised secret
2. Audit all access logs
3. Revoke compromised credentials
4. Update dependent systems
5. Document incident

```bash
# Emergency rotation
docker compose run --rm rotation-handler \
  --secret-id=COMPROMISED_SECRET \
  --action=emergency-rotate \
  --reason="security-incident"
```

## Cost Optimization

### Pricing Estimates (per month)

```
11 secrets × $0.06 = $0.66
+ 2 replicas × 5 critical secrets × $0.06 = $0.60
+ ~10,000 access operations × $0.03/10k = $0.03
+ KMS key × $1.00 = $1.00
+ KMS operations × $0.03/10k = $0.03
────────────────────────────────────────────
Total: ~$2.32/month
```

### Cost Optimization Tips

1. Disable old secret versions after grace period
2. Use automatic replication for non-critical secrets
3. Cache secrets in application memory
4. Disable rotation in development environments

## Troubleshooting

### Secret Access Denied

```bash
# Check IAM bindings
docker compose run --rm gcloud-tools \
  gcloud secrets get-iam-policy erlmcp-db-password --project=PROJECT_ID

# Grant access
docker compose run --rm gcloud-tools \
  gcloud secrets add-iam-policy-binding erlmcp-db-password \
  --member=serviceAccount:SA@PROJECT.iam.gserviceaccount.com \
  --role=roles/secretmanager.secretAccessor \
  --project=PROJECT_ID
```

### Rotation Failures

```bash
# Check rotation logs
docker compose run --rm gcloud-tools \
  gcloud logging read 'resource.type="cloud_run_job" AND labels.job_name="secret-rotation"' \
  --project=PROJECT_ID \
  --limit=50
```

### KMS Key Access Issues

```bash
# Grant Secret Manager access to KMS key
docker compose run --rm gcloud-tools \
  gcloud kms keys add-iam-policy-binding SECRET_KEY \
  --keyring=KEYRING --location=LOCATION \
  --member=serviceAccount:service-PROJECT_NUMBER@gcp-sa-secretmanager.iam.gserviceaccount.com \
  --role=roles/cloudkms.cryptoKeyEncrypterDecrypter \
  --project=PROJECT_ID
```

## Migration from Existing Secrets

```hcl
# Import existing secrets
import {
  to = module.secret_manager.google_secret_manager_secret.db_password
  id = "projects/PROJECT_ID/secrets/erlmcp-db-password"
}

# Then apply to add enhanced security features
terraform apply
```

## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.5.0 |
| google | >= 5.0.0 |
| google-beta | >= 5.0.0 |
| random | >= 3.0.0 |

## Providers

| Name | Version |
|------|---------|
| google | >= 5.0.0 |
| google-beta | >= 5.0.0 |
| random | >= 3.0.0 |

## Resources Created

- 11 × `google_secret_manager_secret`
- 11 × `google_secret_manager_secret_version` (optional)
- 1 × `google_kms_key_ring` (if CMEK enabled)
- 1 × `google_kms_crypto_key` (if CMEK enabled)
- 1 × `google_pubsub_topic` (if rotation notifications enabled)
- 55+ × `google_secret_manager_secret_iam_member` (varies by configuration)
- 1 × `google_project_iam_audit_config` (if audit logging enabled)

## Support

For issues, questions, or contributions, please refer to the main erlmcp repository.

## License

Copyright 2024-2026 erlmcp contributors. See LICENSE file for details.
