# Secret Management Guide for erlmcp v3

## Overview

This guide covers the comprehensive secret management implementation for erlmcp v3, including External Secrets Operator integration, HashiCorp Vault configuration, secret rotation policies, and pre-commit hooks to prevent secrets from being committed to git.

## Table of Contents

1. [Architecture](#architecture)
2. [External Secrets Operator](#external-secrets-operator)
3. [Vault Integration](#vault-integration)
4. [Secret Rotation](#secret-rotation)
5. [Pre-commit Hooks](#pre-commit-hooks)
6. [Usage Examples](#usage-examples)
7. [Troubleshooting](#troubleshooting)

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Kubernetes Cluster                          │
│                                                                  │
│  ┌──────────────┐         ┌──────────────────────────────────┐ │
│  │ erlmcp Pods  │────────>│ External Secrets Operator       │ │
│  └──────────────┘         │  (Syncs secrets)                 │ │
│                            └──────────────────────────────────┘ │
│                                      ▲                         │
│                                      │                         │
│  ┌──────────────┐                    │                         │
│  │ Kubernetes   │<───────────────────┘                         │
│  │ Secrets      │                                              │
│  └──────────────┘                                              │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌──────────────────────────────────────────────────────────────────┐
│                      HashiCorp Vault                             │
│                                                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐            │
│  │ KV Secrets  │  │ PKI         │  │ Database    │            │
│  │             │  │ (TLS Certs) │  │ Credentials  │            │
│  └─────────────┘  └─────────────┘  └─────────────┘            │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │              Secret Rotation Policies                   │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## External Secrets Operator

### Installation

The External Secrets Operator is installed via the Helm chart:

```bash
# Add the External Secrets Operator Helm repository
helm repo add external-secrets https://charts.external-secrets.io/
helm repo update

# Install the operator
helm install external-secrets external-secrets/external-secrets \
  -n external-secrets \
  --create-namespace
```

### Configuration

External secrets are configured in `values-external-secrets.yaml`:

```yaml
externalSecrets:
  enabled: true

  secrets:
    - name: erlmcp-jwt
      key: jwt
      secretStore: erlmcp-vault-store
      refreshInterval: 1h
      data:
        - secretKey: jwt-secret
          remoteRef:
            key: erlmcp/config/jwt
            property: secret
```

### Secrets Managed

| Secret Name | Source | Description | Refresh Interval |
|-------------|--------|-------------|------------------|
| erlmcp-jwt | Vault | JWT signing secret | 1h |
| erlmcp-session | Vault | Session encryption key | 1h |
| erlmcp-database | Vault | Database credentials | 24h |
| erlmcp-tls | Vault PKI | TLS certificates | 7d |
| erlmcp-oauth | Vault | OAuth2 credentials | 1h |
| erlmcp-api-keys | Vault | External API keys | 24h |
| erlmcp-encryption | Vault | Application encryption key | 30d |
| erlmcp-redis | Vault | Redis credentials | 24h |

### SecretStore

The SecretStore defines the connection to Vault:

```yaml
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: erlmcp-vault-store
  namespace: erlmcp-system
spec:
  provider:
    vault:
      server: https://vault.vault.svc.cluster.local:8200
      path: secret
      version: v2
      auth:
        kubernetes:
          mountPath: kubernetes
          role: erlmcp
```

## Vault Integration

### Vault Deployment

Vault is deployed as a StatefulSet with HA support:

```bash
# Deploy Vault
kubectl apply -f k8s/deployments/helm/erlmcp/crds/vault-deployment.yaml
```

### Initial Setup

Run the Vault setup script:

```bash
./scripts/secret-management/setup-vault.sh production
```

This script:
1. Enables secrets engines (KV v2, Database, PKI, Transit)
2. Configures Kubernetes authentication
3. Creates Vault policies
4. Configures database secrets engine
5. Sets up PKI for TLS certificates
6. Seeds initial secrets

### Vault Policies

The following policies are defined:

| Policy | Description | Permissions |
|--------|-------------|-------------|
| erlmcp-admin | Full access to erlmcp secrets | CRUD all |
| erlmcp-config | Application configuration | Read only |
| erlmcp-tls | Certificate management | Read/Issue |
| erlmcp-database | Database credentials | Read only |
| erlmcp-rotation | Secret rotation | Read/Update |
| erlmcp-cicd | CI/CD automation | Create/Read/Update/Delete |

### Secret Paths

```
secret/
├── erlmcp/
│   ├── config/
│   │   ├── jwt
│   │   ├── session
│   │   └── encryption
│   ├── tls/
│   │   ├── ca
│   │   └── server
│   ├── auth/
│   │   ├── oauth2
│   │   └── api-keys
│   └── database/
```

## Secret Rotation

### Automatic Rotation

Secrets are automatically rotated based on defined policies:

```yaml
secretRotation:
  enabled: true

  policies:
    - name: erlmcp-jwt-rotation
      key: jwt
      schedule: "0 0 1 * *"  # Monthly
      strategy:
        type: automatic
        generateNew: true
        length: 32
      postRotation:
        restartPods:
          deployment: erlmcp
```

### Manual Rotation

Rotate a secret manually:

```bash
./scripts/secret-management/rotate-secret.sh erlmcp/config/jwt
```

With options:

```bash
# Dry run
./scripts/secret-management/rotate-secret.sh erlmcp/config/jwt --dry-run

# No pod restart
./scripts/secret-management/rotate-secret.sh erlmcp/config/jwt --no-restart

# Send notification
./scripts/secret-management/rotate-secret.sh erlmcp/config/jwt --notify
```

### Rotation Schedule

| Secret | Schedule | Strategy |
|--------|----------|----------|
| JWT secret | Monthly | Auto-generate |
| Database credentials | Weekly | Vault rotate |
| TLS certificates | Monthly | PKI renew |
| Encryption keys | Quarterly | Auto-generate |

## Pre-commit Hooks

### Installation

The pre-commit hook is automatically installed. To manually install:

```bash
ln -sf ../../hooks/pre-commit-secret-audit.sh .git/hooks/pre-commit
```

### What It Checks

The pre-commit hook prevents:

- Hardcoded passwords
- API keys and tokens
- Private keys and certificates
- Database connection strings with passwords
- JWT tokens
- OAuth credentials
- Cloud provider keys (AWS, GCP, Azure)

### Excluded Files

The following files are excluded from checks:

- `.template` files
- `.env.template` files
- `.example` files
- External secrets configuration
- Vault configuration
- Secret management scripts

### Overriding

If you need to commit a file that triggers the hook:

```bash
git commit --no-verify
```

## Usage Examples

### Deploying with External Secrets

```bash
# Deploy using Helm with external secrets
helm install erlmcp ./k8s/deployments/helm/erlmcp \
  -n erlmcp \
  --create-namespace \
  -f k8s/deployments/helm/erlmcp/values-external-secrets.yaml
```

### Checking Secret Sync Status

```bash
# Check ExternalSecret status
kubectl get externalsecret -n erlmcp

# Describe specific external secret
kubectl describe externalsecret erlmcp-jwt -n erlmcp

# Check synced Kubernetes secret
kubectl get secret erlmcp-jwt -n erlmcp -o yaml
```

### Manually Triggering Sync

```bash
# Force sync by updating annotation
kubectl annotate externalsecret erlmcp-jwt \
  -n erlmcp \
  force-sync=$(date +%s) \
  --overwrite
```

### Vault Operations

```bash
# Enable userpass auth (for admin access)
vault auth enable userpass

# Create admin user
vault write auth/userpass/users/admin \
  password=$(openssl rand -base64 16) \
  policies=erlmcp-admin

# Login
vault login -method=userpass username=admin

# Write a secret
vault kv put erlmcp/config/jwt \
  secret=$(openssl rand -base64 32) \
  algorithm=HS256

# Read a secret
vault kv get erlmcp/config/jwt

# List secrets
vault kv list erlmcp/config
```

## Troubleshooting

### ExternalSecret Not Syncing

```bash
# Check ExternalSecret status
kubectl get externalsecret -n erlmcp

# Describe for more details
kubectl describe externalsecret <name> -n erlmcp

# Check operator logs
kubectl logs -n external-secrets \
  deployment/external-secrets-operator

# Check SecretStore status
kubectl get secretstore -n erlmcp
kubectl describe secretstore erlmcp-vault-store -n erlmcp
```

### Vault Connection Issues

```bash
# Check Vault pods
kubectl get pods -n vault

# Check Vault logs
kubectl logs -n vault statefulset/vault

# Port forward to Vault
kubectl port-forward -n vault svc/vault 8200:8200

# Test connection
export VAULT_ADDR=https://localhost:8200
vault status
```

### Secret Rotation Failed

```bash
# Check rotation CronJob
kubectl get cronjob -n erlmcp-system

# Check rotation job logs
kubectl logs job/erlmcp-secret-rotation-<timestamp> -n erlmcp-system

# Manually run rotation
./scripts/secret-management/rotate-secret.sh <secret-path> --dry-run
```

## Security Best Practices

1. **Never commit secrets to git** - Use the pre-commit hook
2. **Use external secret management** - Enable External Secrets Operator
3. **Rotate secrets regularly** - Configure automatic rotation
4. **Use least privilege** - Create specific Vault policies
5. **Enable audit logging** - Track all secret access
6. **Use short TTLs** - Set appropriate lease durations
7. **Encrypt at rest** - Enable Vault encryption
8. **Use TLS** - All communication should be encrypted

## Compliance

This implementation supports:

- **SOC 2**: Access control, audit logging, encryption
- **GDPR**: Right to erasure (via secret deletion), data portability
- **HIPAA**: PHI protection via encryption, audit trails
- **PCI DSS**: Secret rotation, access control, logging
