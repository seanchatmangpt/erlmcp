#!/bin/bash
# Vault Setup Script for erlmcp v3
# Usage: ./setup-vault.sh [environment]
#
# Environment: development, staging, production

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV="${1:-production}"
VAULT_ADDR="${VAULT_ADDR:-https://vault.vault.svc.cluster.local:8200}"
VAULT_NAMESPACE="${VAULT_NAMESPACE:-erlmcp-system}"

# Function to log messages
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if vault is accessible
check_vault() {
    log_info "Checking Vault connectivity..."
    if ! vault status &>/dev/null; then
        log_error "Cannot connect to Vault at ${VAULT_ADDR}"
        log_error "Please ensure VAULT_ADDR and VAULT_TOKEN are set"
        exit 1
    fi
    log_info "Vault is accessible"
}

# Function to enable Vault secrets engines
enable_secrets_engines() {
    log_info "Enabling Vault secrets engines..."

    # KV v2 secrets engine
    vault secrets enable -path=erlmcp kv-v2 2>/dev/null || log_warn "erlmcp secrets engine already enabled"

    # Database secrets engine
    vault secrets enable -path=database database 2>/dev/null || log_warn "database secrets engine already enabled"

    # PKI secrets engine for internal TLS
    vault secrets enable -path=pki-internal pki 2>/dev/null || log_warn "pki-internal already enabled"
    vault secrets tune -max-lease-ttl=8760h pki-internal

    # PKI for external certificates
    vault secrets enable -path=pki pki 2>/dev/null || log_warn "pki already enabled"
    vault secrets tune -max-lease-ttl=8760h pki

    # Transit for encryption as a service
    vault secrets enable -path=transit transit 2>/dev/null || log_warn "transit already enabled"

    log_info "Secrets engines enabled"
}

# Function to configure Vault authentication
configure_auth() {
    log_info "Configuring Vault authentication..."

    # Enable Kubernetes authentication
    vault auth enable kubernetes 2>/dev/null || log_warn "kubernetes auth already enabled"

    # Get Kubernetes host and token from service account
    if [[ -f /var/run/secrets/kubernetes.io/serviceaccount/token ]]; then
        KUBERNETES_HOST="https://$KUBERNETES_SERVICE_HOST:$KUBERNETES_SERVICE_PORT"
        SA_JWT_TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)
        SA_CA_CRT=$(cat /var/run/secrets/kubernetes.io/serviceaccount/ca.crt | base64 | tr -d '\n')

        # Write Kubernetes auth config
        vault write auth/kubernetes/config \
            kubernetes_host="${KUBERNETES_HOST}" \
            token_reviewer_jwt="${SA_JWT_TOKEN}" \
            kubernetes_ca_cert="${SA_CA_CRT}" \
            issuer="kubernetes.default.svc"

        log_info "Kubernetes authentication configured"
    else
        log_warn "Not running in Kubernetes, skipping Kubernetes auth setup"
    fi

    # Create roles for different components
    vault write auth/kubernetes/role/erlmcp \
        bound_service_account_names="erlmcp,erlmcp-*" \
        bound_service_account_namespaces="erlmcp,erlmcp-system,erlmcp-observability" \
        policies="erlmcp-config,erlmcp-tls,erlmcp-database" \
        ttl=24h

    vault write auth/kubernetes/role/erlmcp-admin \
        bound_service_account_names="erlmcp-admin" \
        bound_service_account_namespaces="erlmcp-system" \
        policies="erlmcp-admin" \
        ttl=4h

    vault write auth/kubernetes/role/erlmcp-rotation \
        bound_service_account_names="external-secrets-operator" \
        bound_service_account_namespaces="external-secrets" \
        policies="erlmcp-rotation" \
        ttl=1h

    log_info "Authentication roles created"
}

# Function to create Vault policies
create_policies() {
    log_info "Creating Vault policies..."

    local policy_dir="${PROJECT_ROOT}/config/vault/policies"

    # Check if policy directory exists
    if [[ -d "${policy_dir}" ]]; then
        for policy in "${policy_dir}"/*.hcl; do
            if [[ -f "${policy}" ]]; then
                local policy_name=$(basename "${policy}" .hcl)
                vault policy write "${policy_name}" "${policy}"
                log_info "Created policy: ${policy_name}"
            fi
        done
    else
        # Create inline policies if directory doesn't exist
        log_warn "Policy directory not found, creating inline policies..."

        vault policy write erlmcp-admin - <<EOF
path "erlmcp/data/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}
path "erlmcp/metadata/*" {
  capabilities = ["list", "read", "delete"]
}
path "database/creds/*" {
  capabilities = ["create", "read", "update", "delete"]
}
path "pki/issue/*" {
  capabilities = ["create", "update"]
}
EOF

        vault policy write erlmcp-config - <<EOF
path "erlmcp/config/*" {
  capabilities = ["read", "list"]
}
path "erlmcp/metadata/*" {
  capabilities = ["read", "list"]
}
EOF

        vault policy write erlmcp-tls - <<EOF
path "erlmcp/tls/*" {
  capabilities = ["read"]
}
path "pki/cert/ca" {
  capabilities = ["read"]
}
path "pki/issue/erlmcp-server" {
  capabilities = ["create"]
}
EOF

        vault policy write erlmcp-database - <<EOF
path "erlmcp/database/*" {
  capabilities = ["read"]
}
path "database/creds/erlmcp-app" {
  capabilities = ["read"]
}
EOF

        vault policy write erlmcp-rotation - <<EOF
path "erlmcp/data/*" {
  capabilities = ["create", "read", "update", "list"]
}
path "erlmcp/metadata/*" {
  capabilities = ["read", "update", "list"]
}
path "database/rotate/*" {
  capabilities = ["create", "update"]
}
EOF
    fi

    log_info "Vault policies created"
}

# Function to configure database secrets engine
configure_database() {
    log_info "Configuring database secrets engine..."

    local db_host="${DB_HOST:-postgres.postgres.svc.cluster.local}"
    local db_port="${DB_PORT:-5432}"
    local db_name="${DB_NAME:-erlmcp}"
    local db_user="${DB_ADMIN_USER:-postgres}"
    local db_password="${DB_ADMIN_PASSWORD:-}"

    if [[ -z "${db_password}" ]]; then
        log_warn "DB_ADMIN_PASSWORD not set, skipping database configuration"
        return
    fi

    # Configure PostgreSQL connection
    vault write database/config/erlmcp-postgres \
        plugin_name="postgresql-database-plugin" \
        allowed_roles="erlmcp-app,erlmcp-replica" \
        connection_url="postgresql://{{username}}:{{password}}@${db_host}:${db_port}/${db_name}?sslmode=require" \
        username="${db_user}" \
        password="${db_password}"

    # Create role for application
    vault write database/roles/erlmcp-app \
        db_name="erlmcp-postgres" \
        creation_statements="CREATE ROLE \"{{name}}\" WITH LOGIN PASSWORD '{{password}}' VALID UNTIL '{{expiration}}'; GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO \"{{name}}\"; GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO \"{{name}}\";" \
        default_ttl="1h" \
        max_ttl="24h"

    # Create role for read replica
    vault write database/roles/erlmcp-replica \
        db_name="erlmcp-postgres" \
        creation_statements="CREATE ROLE \"{{name}}\" WITH LOGIN PASSWORD '{{password}}' VALID UNTIL '{{expiration}}'; GRANT SELECT ON ALL TABLES IN SCHEMA public TO \"{{name}}\"; GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO \"{{name}}\";" \
        default_ttl="1h" \
        max_ttl="24h"

    log_info "Database secrets engine configured"
}

# Function to configure PKI for TLS certificates
configure_pki() {
    log_info "Configuring PKI secrets engine..."

    # Generate internal CA
    vault secrets tune -max-lease-ttl=87600h pki-internal
    vault write -field=certificate pki-internal/root/generate/internal \
        common_name="erlmcp Internal CA" \
        ttl=87600h \
        organization="erlmcp" \
        ou="Engineering" \
        locality="San Francisco" \
        region="California" \
        country=US > /tmp/ca.crt

    # Configure CA and CRL URLs
    vault write pki-internal/config/urls \
        issuing_certificates="${VAULT_ADDR}/v1/pki-internal/ca" \
        crl_distribution_points="${VAULT_ADDR}/v1/pki-internal/crl"

    # Create role for server certificates
    vault write pki-internal/roles/erlmcp-server \
        allowed_domains="erlmcp.svc.cluster.local,erlmcp-*.erlmcp.svc.cluster.local" \
        allow_subdomains=true \
        allow_bare_domains=false \
        max_ttl="720h"

    # Create role for client certificates
    vault write pki-internal/roles/erlmcp-client \
        allowed_domains="client.erlmcp.svc.cluster.local" \
        allow_subdomains=true \
        client_flag=true \
        max_ttl="720h"

    log_info "PKI secrets engine configured"
}

# Function to seed initial secrets
seed_secrets() {
    log_info "Seeding initial secrets..."

    # Generate and store JWT secret
    vault kv put erlmcp/config/jwt \
        secret="$(openssl rand -base64 32)" \
        algorithm="HS256" \
        expiration="3600" \
        issuer="erlmcp" \
        audience="erlmcp-users"

    # Generate and store session secret
    vault kv put erlmcp/config/session \
        secret="$(openssl rand -base64 32)" \
        ttl="3600" \
        max_sessions=10000

    # Generate and store encryption key
    vault kv put erlmcp/config/encryption \
        key="$(openssl rand -base64 32)" \
        algorithm="AES-256-GCM"

    # Generate and store OAuth2 secrets (placeholder)
    vault kv put erlmcp/auth/oauth2 \
        client_id="" \
        client_secret="" \
        redirect_uri="" \
        scopes="openid,profile,email"

    # Store API keys (placeholder - to be updated)
    vault kv put erlmcp/auth/api-keys \
        external_api="" \
        monitoring_api="" \
        alerting_api=""

    log_info "Initial secrets seeded"
}

# Function to configure audit logging
configure_audit() {
    log_info "Configuring audit logging..."

    # Enable file audit log
    vault audit enable file file_path=/var/log/vault/audit.log

    # Enable syslog audit log
    vault audit enable syslog tag="vault"

    log_info "Audit logging configured"
}

# Function to create rotation schedule
create_rotation_schedule() {
    log_info "Creating secret rotation schedule..."

    # Note: This is a placeholder. Actual rotation requires:
    # - External Secrets Operator with rotation enabled
    # - Vault Agent with auto-auth
    # - CronJob or Kubernetes Cron resource

    cat > "${SCRIPT_DIR}/rotation-cronjob.yaml" <<EOF
apiVersion: batch/v1
kind: CronJob
metadata:
  name: erlmcp-secret-rotation
  namespace: erlmcp-system
spec:
  schedule: "0 0 1 * *"  # Monthly rotation
  successfulJobsHistoryLimit: 3
  failedJobsHistoryLimit: 3
  jobTemplate:
    spec:
      template:
        spec:
          serviceAccountName: erlmcp-rotation
          restartPolicy: OnFailure
          containers:
          - name: vault-rotate
            image: hashicorp/vault:latest
            command:
            - /bin/sh
            - -c
            - |
              vault kv patch erlmcp/config/jwt \
                secret=\$(openssl rand -base64 32) && \
              kubectl rollout restart deployment/erlmcp -n erlmcp
            env:
            - name: VAULT_ADDR
              value: https://vault.vault.svc.cluster.local:8200
            - name: VAULT_ROLE
              value: erlmcp-rotation
            - name: VAULT_TOKEN
              valueFrom:
                secretKeyRef:
                  name: erlmcp-vault-auth
                  key: token
EOF

    log_info "Rotation schedule created at ${SCRIPT_DIR}/rotation-cronjob.yaml"
}

# Main execution
main() {
    log_info "Starting Vault setup for erlmcp v3 (${ENV})..."

    check_vault
    enable_secrets_engines
    configure_auth
    create_policies
    configure_database
    configure_pki
    seed_secrets
    configure_audit
    create_rotation_schedule

    log_info "Vault setup complete!"
    log_info "Next steps:"
    log_info "  1. Verify Vault is sealed and unsealed as needed"
    log_info "  2. Deploy External Secrets Operator"
    log_info "  3. Apply SecretStore and ExternalSecret manifests"
    log_info "  4. Verify secrets are being synced to Kubernetes"
}

# Run main function
main
