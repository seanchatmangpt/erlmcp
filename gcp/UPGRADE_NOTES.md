# GCP Terraform Configuration - Upgrade Notes

**Date:** 2026-02-06
**Terraform Version:** >= 1.5.0
**Google Provider Version:** ~> 6.0

## Summary of Changes

This upgrade brings all GCP Terraform configurations up to date with the latest provider versions, best practices, and security improvements as of February 2026.

## Major Updates

### 1. Provider Versions
- **Terraform:** Upgraded from >= 1.0 to >= 1.5.0
- **Google Provider:** Upgraded from ~> 5.0 to ~> 6.0
- **Google Beta Provider:** Upgraded from ~> 5.0 to ~> 6.0
- **Added:** Random provider ~> 3.6 for secure password generation

### 2. Security Enhancements

#### Encryption
- **Customer-Managed Encryption Keys (CMEK):** Implemented for all production resources
- **HSM Protection:** Production keys use Cloud HSM for hardware-level security
- **Automatic Key Rotation:** 90-day rotation for all KMS keys
- **Secret Rotation:** Automatic rotation for Secret Manager secrets

#### Access Control
- **Workload Identity:** Enabled for GKE clusters
- **Binary Authorization:** Enforced for production container images
- **Private Clusters:** GKE clusters use private nodes by default
- **VPC Service Controls:** Support for data exfiltration prevention

#### Audit and Compliance
- **Comprehensive Audit Logging:** Security events logged to dedicated bucket
- **Log Encryption:** All logs encrypted with CMEK
- **Compliance Framework:** Configurable compliance (SOX, HIPAA, PCI-DSS, GDPR, ISO27001)
- **Budget Alerts:** Multi-threshold budget monitoring

### 3. GKE Improvements

- **PostgreSQL 16:** Upgraded from PostgreSQL 14
- **Latest Redis:** Updated to Redis 7.2
- **GKE Release Channels:** Automatic updates via release channels
- **Managed Prometheus:** Built-in monitoring for production
- **Security Posture:** Enterprise-level security scanning
- **Shielded Nodes:** Enabled for all environments
- **Network Policies:** Enforced for workload isolation

### 4. Cloud SQL Enhancements

- **Point-in-Time Recovery:** Enabled for all environments
- **Regional Availability:** High availability for staging/production
- **Query Insights:** Advanced performance monitoring
- **SSL Required:** TLS encryption enforced
- **Private IP Only:** No public internet access in production
- **Enhanced Logging:** Detailed connection and query logging

### 5. Storage and Artifact Management

- **Immutable Tags:** Production artifacts cannot be overwritten
- **Cleanup Policies:** Automatic removal of old/untagged images
- **Soft Delete:** 7-day recovery window for deleted state
- **Public Access Prevention:** Enforced for all buckets
- **Lifecycle Rules:** Automatic archival and deletion policies

### 6. Monitoring and Observability

- **Multi-Region Health Checks:** Global availability monitoring
- **Security Log Sink:** Dedicated logging for security events
- **Notification Channels:** Email alerts for critical events
- **Budget Monitoring:** Cost tracking with multiple thresholds
- **Cloud Operations Integration:** Comprehensive metrics and traces

## New Variables

### Core Configuration
- `artifact_retention_count` - Number of artifact versions to retain (default: 10)
- `alert_email_addresses` - List of emails for alerts (default: [])
- `monthly_budget_limit` - Budget limit in USD (default: 1000)

### Logging
- `log_retention_days` - General log retention (default: 30)
- `security_log_retention_days` - Security log retention (default: 365)

### Security Features
- `enable_workload_identity` - Enable Workload Identity (default: true)
- `enable_binary_authorization` - Container image validation (default: true)
- `enable_vulnerability_scanning` - Scan images for vulnerabilities (default: true)
- `enable_ddos_protection` - Cloud Armor DDoS protection (default: false)
- `enable_audit_logs` - Comprehensive audit logging (default: true)

### Compliance
- `compliance_framework` - Compliance standard (default: "sox")
- `enable_organization_policies` - Enhanced security policies (default: false)
- `enable_vpc_service_controls` - Data exfiltration prevention (default: false)

### Advanced Options
- `enable_cross_region_backup` - Multi-region backups (default: false)
- `enable_data_residency` - Regional data controls (default: false)
- `terraform_state_encryption` - CMEK for state (default: true)
- `additional_labels` - Custom resource labels (default: {})

## Migration Path

### Prerequisites
1. Terraform >= 1.5.0 installed
2. gcloud CLI authenticated
3. Required GCP APIs enabled
4. Billing account configured (for budget alerts)

### Step 1: Backup Current State
```bash
# Download current state
terraform state pull > backup-$(date +%Y%m%d-%H%M%S).tfstate

# Backup configuration files
tar -czf terraform-backup-$(date +%Y%m%d).tar.gz *.tf *.tfvars
```

### Step 2: Update Terraform and Providers
```bash
# Initialize with new provider versions
terraform init -upgrade

# Review changes
terraform plan -var-file=terraform.production.tfvars
```

### Step 3: Apply Changes Incrementally
```bash
# For production, use targeted applies to minimize risk
terraform apply -target=google_kms_key_ring.erlmcp -var-file=terraform.production.tfvars
terraform apply -target=google_kms_crypto_key.erlmcp_app -var-file=terraform.production.tfvars

# Then apply all changes
terraform apply -var-file=terraform.production.tfvars
```

### Step 4: Verify Resources
```bash
# Check GKE cluster
gcloud container clusters describe erlmcp-prod-cluster --region=us-central1

# Check Cloud SQL
gcloud sql instances describe erlmcp-prod-db

# Check secrets
gcloud secrets list

# Check KMS keys
gcloud kms keys list --location=us-central1 --keyring=erlmcp-prod-keyring
```

## Breaking Changes

### Provider API Changes
- Some resource attributes may have changed between v5 and v6
- Default labels now applied at provider level
- Some deprecated fields removed

### Resource Name Changes
- KMS keys now include environment suffix
- Secrets now include environment suffix
- VPC subnets now have secondary ranges for GKE

### New Required Variables
The following variables are now used in main.tf and require values:
- `alert_email_addresses` (can be empty list)
- `monthly_budget_limit`
- `log_retention_days`
- `security_log_retention_days`
- `artifact_retention_count`

## Environment-Specific Changes

### Development
- Uses preemptible/spot nodes for cost savings
- Minimal logging retention (7 days)
- No binary authorization
- SOFTWARE-level encryption (not HSM)

### Staging
- Production-like configuration
- Regional Cloud SQL availability
- 30-day log retention
- Binary authorization enabled
- CMEK encryption with SOFTWARE keys

### Production
- Multi-zone GKE cluster
- Regional Cloud SQL with PITR
- 90-day log retention, 365-day security logs
- Binary authorization enforced
- CMEK encryption with HSM keys
- VPC Service Controls support
- Organization policies support

## Post-Upgrade Tasks

### 1. Populate Secrets
```bash
# Database password
echo -n "YOUR_DB_PASSWORD" | gcloud secrets versions add erlmcp-production-db-password --data-file=-

# API keys
echo -n "YOUR_API_KEYS" | gcloud secrets versions add erlmcp-api-keys-production --data-file=-

# Deployment credentials
echo -n "YOUR_DEPLOY_CREDS" | gcloud secrets versions add erlmcp-deployment-creds-production --data-file=-
```

### 2. Configure GitHub Secrets
```bash
# Get service account key
terraform output -raw service_account_key_private | base64 -d > sa-key.json

# Add to GitHub as GCP_SA_KEY
gh secret set GCP_SA_KEY < sa-key.json

# Clean up local key
rm sa-key.json
```

### 3. Enable Backend
After first successful apply, uncomment backend configuration in main.tf:
```hcl
backend "gcs" {
  bucket = "PROJECT_ID-terraform-state"
  prefix = "base/terraform.tfstate"
}
```

Then migrate state:
```bash
terraform init -migrate-state
```

### 4. Review and Update Email Addresses
Update `alert_email_addresses` in tfvars files with actual team emails.

### 5. Configure Budget Thresholds
Review and adjust `monthly_budget_limit` based on actual usage patterns.

## Verification Checklist

- [ ] All APIs enabled successfully
- [ ] KMS keys created with proper rotation
- [ ] Secrets created and accessible
- [ ] GKE cluster accessible via kubectl
- [ ] Cloud SQL reachable from GKE
- [ ] Redis cache accessible
- [ ] Artifact Registry ready for pushes
- [ ] Budget alerts configured
- [ ] Monitoring channels receiving alerts
- [ ] Terraform state in GCS bucket
- [ ] Service accounts have correct permissions

## Rollback Procedure

If issues occur:

1. **Restore from backup:**
   ```bash
   terraform state push backup-TIMESTAMP.tfstate
   ```

2. **Downgrade providers:**
   ```bash
   # Restore old configuration files
   tar -xzf terraform-backup-DATE.tar.gz

   # Reinitialize
   terraform init -upgrade=false
   ```

3. **Reapply old configuration:**
   ```bash
   terraform apply -var-file=terraform.production.tfvars
   ```

## Support and Documentation

### Official Documentation
- [Terraform GCP Provider v6.x](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [GCP Best Practices](https://cloud.google.com/docs/enterprise/best-practices-for-enterprise-organizations)
- [GKE Security Hardening](https://cloud.google.com/kubernetes-engine/docs/how-to/hardening-your-cluster)

### Internal Resources
- `/home/user/erlmcp/gcp/README.md` - General GCP documentation
- `/home/user/erlmcp/gcp/main.tf` - Base infrastructure
- `/home/user/erlmcp/gcp/environments.tf` - Multi-environment setup

## Cost Implications

### Expected Cost Changes
- **KMS HSM keys (production):** ~$1/key/month + operations
- **Regional Cloud SQL:** 2x zonal cost
- **Redis HA:** ~2x basic tier cost
- **Enhanced logging:** Storage and ingestion costs
- **Multi-region backups:** Additional storage in replica regions

### Cost Optimization Tips
1. Use preemptible nodes in dev
2. Right-size machine types based on actual usage
3. Enable autoscaling to scale down during off-hours
4. Set appropriate log retention periods
5. Use budget alerts to catch unexpected costs early

## Security Considerations

### Immediate Actions Required
1. Rotate all secrets stored in Secret Manager
2. Review IAM bindings and remove unnecessary permissions
3. Update master authorized networks to restrict GKE access
4. Configure VPC Service Controls for production
5. Enable organization policies if applicable

### Ongoing Security Tasks
- Review audit logs monthly
- Monitor vulnerability scan results
- Update base images regularly
- Rotate KMS keys per schedule
- Review and update IAM permissions quarterly

## Contact

For questions or issues:
- Platform Team: platform@example.com
- DevOps: devops@example.com
- Security: security@example.com
