# ============================================================================
# Secret Manager Secret Definitions
# This file defines the 11 required secrets for erlmcp
# ============================================================================

# Required Secrets List:
# 1. erlmcp-erlang-cookie  - Erlang distribution cookie (64 chars, base64)
# 2. erlmcp-db-password    - PostgreSQL database password
# 3. erlmcp-redis-password  - Redis cache password
# 4. erlmcp-tls-cert       - TLS certificate (PEM format)
# 5. erlmcp-tls-key        - TLS private key (PEM format)
# 6. erlmcp-ca-bundle      - CA certificate bundle
# 7. erlmcp-jwt-private-key - JWT signing key (RS256)
# 8. erlmcp-jwt-public-key  - JWT verification key (RS256)
# 9. erlmcp-grafana-password - Grafana admin password
# 10. erlmcp-backup-key     - Backup encryption key
# 11. erlmcp-otel-ca-cert   - OpenTelemetry CA certificate

# ============================================================================
# Secret Creation Commands (for manual setup)
# ============================================================================

# Generate secure values:
# openssl rand -base64 48 | tr -d '\n' > erlang-cookie.txt
# openssl rand -base64 32 | tr -d '\n' > db-password.txt
# openssl rand -base64 32 | tr -d '\n' > redis-password.txt
# openssl rand -base64 32 | tr -d '\n' > grafana-password.txt
# openssl rand -base64 64 | tr -d '\n' > backup-key.txt

# Generate JWT key pair:
# openssl genrsa -out jwt-private.pem 4096
# openssl rsa -in jwt-private.pem -pubout -out jwt-public.pem

# Create secrets:
# gcloud secrets create erlmcp-erlang-cookie --data-file=erlang-cookie.txt
# gcloud secrets create erlmcp-db-password --data-file=db-password.txt
# gcloud secrets create erlmcp-redis-password --data-file=redis-password.txt
# gcloud secrets create erlmcp-tls-cert --data-file=tls.crt
# gcloud secrets create erlmcp-tls-key --data-file=tls.key
# gcloud secrets create erlmcp-ca-bundle --data-file=ca-bundle.crt
# gcloud secrets create erlmcp-jwt-private-key --data-file=jwt-private.pem
# gcloud secrets create erlmcp-jwt-public-key --data-file=jwt-public.pem
# gcloud secrets create erlmcp-grafana-password --data-file=grafana-password.txt
# gcloud secrets create erlmcp-backup-key --data-file=backup-key.txt
# gcloud secrets create erlmcp-otel-ca-cert --data-file=otel-ca.crt

# ============================================================================
# Secret Access Patterns
# ============================================================================

# GKE Workload Identity:
# - Annotate Kubernetes ServiceAccount with GCP Service Account
# - Grant secretAccessor role to GCP Service Account

# Cloud Run:
# - Grant secretAccessor role to Cloud Run Service Account
# - Reference secrets in container definition

# Compute Engine:
# - Grant secretAccessor role to VM Service Account
# - Use startup script to fetch and inject secrets

# ============================================================================
# Secret Rotation Strategy
# ============================================================================

# Rotation Plan:
# 1. High frequency (30 days): erlmcp-erlang-cookie
# 2. Medium frequency (90 days): db-password, redis-password, grafana-password
# 3. Low frequency (365 days): jwt keys, backup keys
# 4. Manual event-driven: tls certs, ca bundles, otel certs

# Rotation Process:
# 1. Create new secret version
# 2. Deploy new version (canary)
# 3. Verify application health
# 4. Rollout to all instances
# 5. Disable old version
# 6. Delete old version after retention period
