# ============================================================================
# Secret Manager Secret Definitions - V3 Security Architecture
# Enterprise-Grade Secret Management with Zero-Trust Principles
# ============================================================================
#
# This file provides comprehensive documentation for the 11 required secrets
# in the erlmcp system. All secrets follow defense-in-depth security practices.
#
# Security Features:
# - CMEK encryption at rest
# - Automatic rotation policies
# - Least-privilege IAM access
# - Comprehensive audit logging
# - Multi-region replication for HA
# - Version management and aliases
# - Compliance annotations (PCI-DSS, HIPAA, GDPR)
#
# ============================================================================

# ============================================================================
# Secret Inventory and Classifications
# ============================================================================

# CRITICAL SECRETS (Tier 1) - 30-day rotation:
# 1. erlmcp-erlang-cookie   - Erlang distributed system authentication
#    - Classification: CRITICAL
#    - Data Class: cluster-auth
#    - Rotation: 30 days (automatic)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Erlang cluster nodes only

# HIGH SECRETS (Tier 2) - 90-day rotation:
# 2. erlmcp-db-password     - PostgreSQL database authentication
#    - Classification: HIGH
#    - Data Class: database-auth
#    - Rotation: 90 days (automatic)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Database service accounts only
#    - Compliance: PCI-DSS, HIPAA

# 3. erlmcp-redis-password  - Redis cache authentication
#    - Classification: HIGH
#    - Data Class: cache-auth
#    - Rotation: 90 days (automatic)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Cache service accounts only
#    - Compliance: PCI-DSS

# CRITICAL SECRETS (Tier 1) - Event-driven rotation:
# 4. erlmcp-tls-cert        - TLS certificate (X.509)
#    - Classification: CRITICAL
#    - Data Class: tls-cert
#    - Rotation: Based on certificate validity (event-driven)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Load balancers, API gateways
#    - Format: PEM-encoded X.509 certificate

# 5. erlmcp-tls-key         - TLS private key (RSA/ECDSA)
#    - Classification: CRITICAL (Highest)
#    - Data Class: private-key
#    - Rotation: Based on certificate validity (event-driven)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: TLS termination points only (highly restricted)
#    - Audit: Mandatory access logging
#    - Compliance: PCI-DSS, HIPAA
#    - Format: PEM-encoded private key

# 6. erlmcp-ca-bundle       - CA certificate bundle
#    - Classification: HIGH
#    - Data Class: ca-cert
#    - Rotation: Event-driven (CA updates)
#    - Replication: Automatic
#    - Access: All application services
#    - Format: PEM-encoded CA certificates

# 7. erlmcp-jwt-private-key - JWT signing key (RS256)
#    - Classification: CRITICAL
#    - Data Class: jwt-signing-key
#    - Rotation: 365 days (automatic)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Authentication services only
#    - Audit: Mandatory access logging
#    - Compliance: PCI-DSS
#    - Algorithm: RSA-4096, RS256
#    - Format: PEM-encoded RSA private key

# 8. erlmcp-jwt-public-key  - JWT verification key (RS256)
#    - Classification: MEDIUM
#    - Data Class: jwt-public-key
#    - Rotation: 365 days (follows private key)
#    - Replication: Automatic
#    - Access: All services that verify JWTs
#    - Algorithm: RSA-4096, RS256
#    - Format: PEM-encoded RSA public key

# 9. erlmcp-grafana-password - Grafana admin authentication
#    - Classification: HIGH
#    - Data Class: monitoring-auth
#    - Rotation: 90 days (automatic)
#    - Replication: Automatic
#    - Access: Monitoring service accounts only

# 10. erlmcp-backup-key     - Backup encryption key (AES-256)
#    - Classification: CRITICAL
#    - Data Class: backup-encryption
#    - Rotation: 365 days (automatic)
#    - Replication: Multi-region (user-managed)
#    - Encryption: CMEK required
#    - Access: Backup services only (highly restricted)
#    - Audit: Mandatory access logging
#    - Compliance: PCI-DSS, HIPAA, GDPR
#    - Format: Base64-encoded 256-bit key

# 11. erlmcp-otel-ca-cert   - OpenTelemetry CA certificate
#    - Classification: HIGH
#    - Data Class: observability-cert
#    - Rotation: Event-driven
#    - Replication: Automatic
#    - Access: OpenTelemetry collectors
#    - Format: PEM-encoded X.509 certificate

# ============================================================================
# Secret Generation Commands (DOCKER-ONLY per CLAUDE.md)
# ============================================================================

# ALL secret generation MUST occur within Docker containers.
# Host execution is FORBIDDEN per erlmcp Constitution.

# Generate secrets using OpenSSL container:
# docker run --rm alpine/openssl rand -base64 48 > erlang-cookie.txt
# docker run --rm alpine/openssl rand -base64 32 > db-password.txt
# docker run --rm alpine/openssl rand -base64 32 > redis-password.txt
# docker run --rm alpine/openssl rand -base64 32 > grafana-password.txt
# docker run --rm alpine/openssl rand -base64 64 > backup-key.txt

# Generate JWT RSA-4096 key pair:
# docker run --rm -v $(pwd):/keys alpine/openssl genrsa -out /keys/jwt-private.pem 4096
# docker run --rm -v $(pwd):/keys alpine/openssl rsa -in /keys/jwt-private.pem -pubout -out /keys/jwt-public.pem

# Generate TLS certificate (self-signed for dev):
# docker run --rm -v $(pwd):/certs alpine/openssl req -x509 -newkey rsa:4096 \
#   -keyout /certs/tls.key -out /certs/tls.crt -days 365 -nodes \
#   -subj "/CN=erlmcp.local/O=erlmcp/C=US"

# ============================================================================
# Secret Upload Commands (via Docker/gcloud)
# ============================================================================

# Upload secrets via gcloud container:
# docker run --rm -v ~/.config/gcloud:/root/.config/gcloud -v $(pwd):/secrets \
#   google/cloud-sdk:alpine gcloud secrets create erlmcp-erlang-cookie \
#   --data-file=/secrets/erlang-cookie.txt --project=PROJECT_ID

# OR use terraform to manage secret versions (recommended):
# terraform apply -var-file=secrets.tfvars

# ============================================================================
# Secret Access Patterns by Platform
# ============================================================================

# 1. GKE with Workload Identity (Recommended):
#    a. Create Kubernetes ServiceAccount
#    b. Annotate with GCP Service Account: iam.gke.io/gcp-service-account=GSA@PROJECT.iam.gserviceaccount.com
#    c. Grant secretAccessor role to GCP Service Account
#    d. Mount secrets via Secret Store CSI Driver:
#       apiVersion: secrets-store.csi.x-k8s.io/v1
#       kind: SecretProviderClass
#       metadata:
#         name: erlmcp-secrets
#       spec:
#         provider: gcp
#         parameters:
#           secrets: |
#             - resourceName: "projects/PROJECT_ID/secrets/erlmcp-erlang-cookie/versions/latest"
#               path: "erlang-cookie"

# 2. Cloud Run:
#    a. Grant secretAccessor role to Cloud Run Service Account
#    b. Reference in service definition:
#       env:
#       - name: ERLANG_COOKIE
#         valueFrom:
#           secretKeyRef:
#             name: erlmcp-erlang-cookie
#             key: latest

# 3. Compute Engine / GCE:
#    a. Grant secretAccessor role to VM Service Account
#    b. Fetch in startup script:
#       gcloud secrets versions access latest --secret=erlmcp-erlang-cookie > /etc/erlmcp/cookie

# 4. Cloud Build:
#    a. Grant secretAccessor role to Cloud Build Service Account
#    b. Use availableSecrets in cloudbuild.yaml:
#       availableSecrets:
#         secretManager:
#         - versionName: projects/PROJECT_ID/secrets/erlmcp-erlang-cookie/versions/latest
#           env: ERLANG_COOKIE

# ============================================================================
# Secret Rotation Strategy and Process
# ============================================================================

# AUTOMATIC ROTATION (via Terraform + CI/CD):

# Critical Secrets (30-day rotation):
# - erlmcp-erlang-cookie
#   Process:
#   1. Rotation triggered automatically by Secret Manager
#   2. Pub/Sub notification sent to rotation topic
#   3. Cloud Function/Cloud Run job generates new cookie
#   4. Updates secret version
#   5. Triggers rolling restart of Erlang cluster
#   6. Monitors cluster health
#   7. Disables old version after grace period

# High Secrets (90-day rotation):
# - erlmcp-db-password, erlmcp-redis-password, erlmcp-grafana-password
#   Process:
#   1. Rotation triggered automatically
#   2. Pub/Sub notification sent
#   3. Rotation service:
#      a. Creates new password
#      b. Updates database/service credential
#      c. Updates Secret Manager version
#      d. Triggers application restart
#      e. Verifies connectivity
#      f. Disables old version

# Crypto Secrets (365-day rotation):
# - erlmcp-jwt-private-key, erlmcp-backup-key
#   Process:
#   1. Rotation triggered automatically
#   2. Generate new key pair
#   3. Update secret versions
#   4. Deploy new keys alongside old (dual-key period)
#   5. Gradually shift traffic to new keys
#   6. Disable old keys after grace period

# EVENT-DRIVEN ROTATION:
# - TLS certificates: Rotate before expiration (90-day notice)
# - CA bundles: Rotate when CA updates occur
# - OpenTelemetry certs: Rotate on infrastructure changes

# ROTATION TESTING:
# All rotation processes MUST be tested in staging before production.
# Use terraform workspace for environment isolation.

# ============================================================================
# Security Best Practices
# ============================================================================

# 1. NEVER commit secrets to version control
#    - Use .gitignore for *.txt, *.pem, *.key files
#    - Use terraform.tfvars.encrypted with SOPS or similar

# 2. NEVER log secret values
#    - All secret variables marked as sensitive=true
#    - Application code must not log secret contents

# 3. ALWAYS use least-privilege IAM
#    - Grant secretAccessor only to specific secrets needed
#    - Use IAM conditions to restrict access further

# 4. ALWAYS enable audit logging
#    - Monitor all secret access via Cloud Logging
#    - Alert on suspicious access patterns

# 5. ALWAYS use CMEK for sensitive secrets
#    - Critical and High secrets require CMEK
#    - Medium secrets may use Google-managed keys

# 6. ALWAYS implement secret rotation
#    - Critical: 30 days
#    - High: 90 days
#    - Crypto: 365 days
#    - Event-driven: As needed

# 7. ALWAYS use multi-region replication for critical secrets
#    - Ensures availability during regional outages
#    - Required for disaster recovery

# 8. ALWAYS test rotation in non-production first
#    - Validate rotation process in staging
#    - Verify application handles rotation gracefully

# 9. ALWAYS maintain version history
#    - Keep at least 2 previous versions
#    - Enables rollback in case of issues

# 10. ALWAYS validate secret format and strength
#     - Passwords: Min 32 chars, high entropy
#     - Keys: RSA-4096 or ECDSA P-384 minimum
#     - Tokens: Min 64 chars, cryptographically random

# ============================================================================
# Compliance and Audit Requirements
# ============================================================================

# PCI-DSS Requirements:
# - Requirement 3.4: Encryption of cardholder data
# - Requirement 3.5: Key management procedures
# - Requirement 3.6: Key rotation
# - Requirement 8.2: Strong authentication
# - Requirement 10.2: Audit trails

# HIPAA Requirements:
# - 164.312(a)(2)(iv): Encryption and decryption
# - 164.312(d): Integrity controls
# - 164.308(a)(4): Access controls

# GDPR Requirements:
# - Article 32: Security of processing
# - Article 25: Data protection by design

# SOC 2 Type II:
# - CC6.1: Logical access controls
# - CC6.6: Encryption management
# - CC7.2: System monitoring

# ============================================================================
# Disaster Recovery and Business Continuity
# ============================================================================

# Backup Strategy:
# - All secrets automatically backed up by Secret Manager
# - Multi-region replication ensures availability
# - Version history maintained for rollback

# Recovery Procedures:
# 1. Secret compromise detected
# 2. Immediately rotate compromised secret
# 3. Audit all access logs
# 4. Revoke compromised credentials
# 5. Update all dependent systems
# 6. Document incident in compliance log

# Recovery Time Objective (RTO): < 15 minutes
# Recovery Point Objective (RPO): 0 (real-time replication)

# ============================================================================
# Monitoring and Alerting
# ============================================================================

# Alert on:
# - Failed secret access attempts (potential breach)
# - Secret access from unexpected service accounts
# - Secret access from unusual locations
# - Secret version disabled/deleted
# - Rotation failures
# - CMEK key usage anomalies

# Dashboards:
# - Secret access frequency by service account
# - Rotation status and upcoming rotations
# - Secret age and staleness
# - Audit log query rate
# - IAM permission changes

# ============================================================================
# Cost Optimization
# ============================================================================

# Secret Manager Pricing (as of 2024):
# - Active secret versions: $0.06 per secret per month
# - Access operations: $0.03 per 10,000 operations
# - Replication: Additional $0.06 per replica region

# Cost Optimization Tips:
# - Disable old secret versions after grace period
# - Use automatic replication for non-critical secrets
# - Cache secrets in application memory (refresh periodically)
# - Batch secret access operations where possible

# ============================================================================

