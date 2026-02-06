# Secret Manager Module - Security Checklist

## Pre-Deployment Security Checklist

### ✓ Infrastructure Preparation

- [ ] Enable Secret Manager API in target GCP project
- [ ] Enable Cloud KMS API for CMEK encryption
- [ ] Set up audit log sink for compliance tracking
- [ ] Configure VPC Service Controls (if required)
- [ ] Create dedicated service accounts per workload type
- [ ] Document service account purpose and access scope

### ✓ Encryption Configuration

- [ ] Enable CMEK for production environments
- [ ] Set KMS key rotation period to 90 days or less
- [ ] Verify KMS key ring location matches primary region
- [ ] Grant Secret Manager service account access to KMS keys
- [ ] Document encryption key recovery procedures

### ✓ Secret Classification

- [ ] Classify all secrets by sensitivity (Critical/High/Medium)
- [ ] Define rotation periods per classification
- [ ] Document data residency requirements
- [ ] Identify multi-region replication needs
- [ ] Map secrets to compliance frameworks (PCI-DSS, HIPAA, GDPR)

### ✓ IAM Configuration

- [ ] Apply least-privilege principle to all service accounts
- [ ] Use IAM conditions to restrict access scope
- [ ] Separate service accounts by workload type
- [ ] Grant secretAccessor role only to specific secrets
- [ ] Avoid project-level secret permissions
- [ ] Enable IAM audit logging
- [ ] Review and document all IAM bindings

### ✓ Rotation Strategy

- [ ] Define rotation frequency per secret classification
- [ ] Set up Pub/Sub topic for rotation notifications
- [ ] Implement rotation handler (Cloud Function/Cloud Run)
- [ ] Test rotation process in non-production
- [ ] Document rotation procedures and rollback steps
- [ ] Configure alerts for rotation failures

### ✓ Replication and HA

- [ ] Enable user-managed replication for critical secrets
- [ ] Select appropriate replica locations (latency vs. compliance)
- [ ] Test failover to replica regions
- [ ] Document disaster recovery procedures
- [ ] Verify RTO (< 15 minutes) and RPO (0) targets

### ✓ Monitoring and Alerting

- [ ] Enable audit logging for all secret operations
- [ ] Set up alerts for failed secret access attempts
- [ ] Monitor secret access patterns for anomalies
- [ ] Track rotation status and upcoming rotations
- [ ] Alert on IAM policy changes
- [ ] Dashboard for secret age and staleness

### ✓ Compliance and Audit

- [ ] Enable DATA_READ and DATA_WRITE audit logs
- [ ] Configure log retention per compliance requirements
- [ ] Set up automated compliance reports
- [ ] Document data classification and handling
- [ ] Maintain access request and approval records
- [ ] Schedule regular security audits

## Deployment Security Checklist

### ✓ Terraform Configuration

- [ ] Use encrypted tfvars files (SOPS, Vault, etc.)
- [ ] Never commit secrets to version control
- [ ] Add *.txt, *.pem, *.key to .gitignore
- [ ] Use remote state with encryption
- [ ] Enable state locking
- [ ] Use separate workspaces per environment

### ✓ Secret Generation (Docker-Only)

- [ ] Generate all secrets using Docker containers (never host)
- [ ] Use cryptographically secure random generators
- [ ] Minimum password length: 32 characters
- [ ] RSA keys: 4096 bits minimum
- [ ] Validate secret strength before upload
- [ ] Securely delete temporary files after upload

### ✓ Initial Secret Provisioning

- [ ] Create secrets via Terraform or gcloud in Docker
- [ ] Verify secret values are not logged
- [ ] Test secret access from each workload
- [ ] Disable or delete test/temporary secret versions
- [ ] Document secret provisioning procedure

### ✓ Version Management

- [ ] Keep maximum 2-3 enabled versions per secret
- [ ] Disable old versions after grace period (7-30 days)
- [ ] Destroy disabled versions after retention period
- [ ] Document version lifecycle policy
- [ ] Test rollback to previous version

## Post-Deployment Security Checklist

### ✓ Access Validation

- [ ] Verify each service account can access only required secrets
- [ ] Test that unauthorized access is denied
- [ ] Validate IAM conditions are enforced
- [ ] Review audit logs for unexpected access
- [ ] Confirm no public access (allUsers, allAuthenticatedUsers)

### ✓ Encryption Verification

- [ ] Verify CMEK encryption for critical secrets
- [ ] Check KMS key rotation is active
- [ ] Validate encryption at rest
- [ ] Test key access revocation procedures
- [ ] Document encryption verification results

### ✓ Rotation Testing

- [ ] Trigger manual rotation for one secret
- [ ] Verify Pub/Sub notification delivery
- [ ] Test application handles new secret version
- [ ] Verify old version is disabled after grace period
- [ ] Document rotation test results

### ✓ Monitoring Validation

- [ ] Verify audit logs are being generated
- [ ] Test alert triggers (failed access, policy changes)
- [ ] Validate log export to SIEM/monitoring system
- [ ] Check dashboard displays current metrics
- [ ] Test incident response procedures

### ✓ Disaster Recovery Testing

- [ ] Test secret access during regional outage
- [ ] Verify failover to replica region
- [ ] Measure actual RTO and RPO
- [ ] Test secret restoration from backups
- [ ] Document DR test results

## Ongoing Security Maintenance

### Daily

- [ ] Monitor audit logs for anomalies
- [ ] Review failed access attempts
- [ ] Check for IAM policy changes

### Weekly

- [ ] Review secret access patterns
- [ ] Check rotation status
- [ ] Validate active secret versions
- [ ] Review security alerts

### Monthly

- [ ] Audit IAM bindings for least-privilege
- [ ] Review and update secret classifications
- [ ] Test rotation procedures
- [ ] Update documentation
- [ ] Review compliance reports

### Quarterly

- [ ] Conduct security audit
- [ ] Review and update rotation policies
- [ ] Test disaster recovery procedures
- [ ] Update threat model
- [ ] Review and update documentation
- [ ] Compliance audit and remediation

### Annually

- [ ] Comprehensive security review
- [ ] Update cryptographic standards (key sizes, algorithms)
- [ ] Review and update IAM policies
- [ ] Disaster recovery full-scale test
- [ ] Third-party security audit
- [ ] Update compliance certifications

## Incident Response Checklist

### Secret Compromise Detected

1. [ ] **Immediately** revoke compromised secret
2. [ ] Audit all access logs for the secret
3. [ ] Identify affected systems and workloads
4. [ ] Generate new secret value
5. [ ] Update all dependent systems
6. [ ] Monitor for unauthorized access attempts
7. [ ] Document incident timeline and impact
8. [ ] Update security controls to prevent recurrence
9. [ ] Notify compliance and legal teams (if required)
10. [ ] Conduct post-incident review

### Unauthorized Access Attempt

1. [ ] Identify source of access attempt
2. [ ] Review IAM permissions for the principal
3. [ ] Check for compromised service accounts
4. [ ] Analyze audit logs for pattern
5. [ ] Block suspicious IP addresses (if applicable)
6. [ ] Rotate potentially compromised secrets
7. [ ] Document incident and remediation
8. [ ] Update security policies

### Rotation Failure

1. [ ] Check rotation handler logs
2. [ ] Verify Pub/Sub message delivery
3. [ ] Test secret generation mechanism
4. [ ] Validate IAM permissions for rotation
5. [ ] Manually trigger rotation if needed
6. [ ] Update dependent systems
7. [ ] Document failure and resolution
8. [ ] Update monitoring alerts

## Security Validation Commands (Docker-Only)

### Run Security Validation Script

```bash
docker compose run --rm security-validator \
  --project-id=PROJECT_ID \
  --check-all
```

### Manual Validation Commands

```bash
# Check CMEK encryption
docker compose run --rm gcloud-tools \
  gcloud secrets describe SECRET_ID --project=PROJECT_ID --format="json" | \
  jq '.replication.userManaged.replicas[].customerManagedEncryption'

# Verify rotation configuration
docker compose run --rm gcloud-tools \
  gcloud secrets describe SECRET_ID --project=PROJECT_ID --format="json" | \
  jq '.rotation'

# Check IAM bindings
docker compose run --rm gcloud-tools \
  gcloud secrets get-iam-policy SECRET_ID --project=PROJECT_ID

# Audit recent secret access
docker compose run --rm gcloud-tools \
  gcloud logging read 'resource.type="secretmanager.googleapis.com/Secret"' \
  --project=PROJECT_ID --limit=100 --format=json

# Verify secret versions
docker compose run --rm gcloud-tools \
  gcloud secrets versions list SECRET_ID --project=PROJECT_ID
```

## Compliance Mappings

### PCI-DSS v4.0

| Requirement | Control | Implementation |
|-------------|---------|----------------|
| 3.4 | Encryption at rest | CMEK with Cloud KMS |
| 3.5 | Key management | KMS with 90-day rotation |
| 3.6 | Key rotation | Automatic secret rotation |
| 8.2 | Strong authentication | 32+ char passwords, RSA-4096 |
| 10.2 | Audit trails | Comprehensive audit logging |

### HIPAA

| Requirement | Control | Implementation |
|-------------|---------|----------------|
| 164.312(a)(2)(iv) | Encryption | CMEK encryption |
| 164.312(d) | Integrity controls | Version management |
| 164.308(a)(4) | Access controls | Least-privilege IAM |

### GDPR

| Article | Requirement | Implementation |
|---------|-------------|----------------|
| 32 | Security of processing | Encryption, access controls, audit logs |
| 25 | Data protection by design | Zero-trust architecture |
| 30 | Records of processing | Comprehensive audit logging |

### SOC 2 Type II

| Trust Service Criteria | Control | Implementation |
|------------------------|---------|----------------|
| CC6.1 | Logical access | Least-privilege IAM with conditions |
| CC6.6 | Encryption management | CMEK with KMS |
| CC7.2 | System monitoring | Audit logs, alerts, dashboards |

## Security Best Practices Summary

### Encryption
- **Always** use CMEK for Critical and High secrets in production
- Rotate KMS keys every 90 days
- Use separate KMS keys per environment

### Access Control
- **Never** grant project-level secret permissions
- Use service accounts, not user accounts, for workload access
- Apply IAM conditions to restrict access scope
- Regularly audit and review IAM bindings

### Secret Management
- **Never** commit secrets to version control
- Use encrypted tfvars or external secret management
- Maintain maximum 2-3 enabled versions per secret
- Disable old versions after grace period

### Rotation
- Automate rotation for all non-certificate secrets
- Test rotation in staging before production
- Monitor rotation status and failures
- Document rollback procedures

### Monitoring
- Enable comprehensive audit logging
- Set up alerts for anomalies and failures
- Regular review of access patterns
- Dashboard for visibility

### Compliance
- Document security controls and procedures
- Conduct regular security audits
- Maintain compliance evidence
- Update policies as requirements change

## Support and Resources

- **Module Documentation**: README.md
- **Security Validation**: validate-security.sh
- **Example Configuration**: examples.tfvars
- **GCP Security Best Practices**: https://cloud.google.com/secret-manager/docs/best-practices
- **Compliance Guidance**: https://cloud.google.com/security/compliance

## Emergency Contacts

Define and document:
- Security team contact
- On-call rotation
- Incident response team
- Compliance officer
- Legal team (for breaches)
