# Secrets Management for erlmcp v3

## Table of Contents

- [Overview](#overview)
- [Kubernetes Secrets](#kubernetes-secrets)
- [External Secrets Operator](#external-secrets-operator)
- [HashiCorp Vault Integration](#hashicorp-vault-integration)
- [Cloud Provider Integration](#cloud-provider-integration)
- [Secret Rotation](#secret-rotation)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

## Overview

erlmcp v3 requires secure management of sensitive data including:
- Erlang distributed cookies
- Database credentials
- API keys and tokens
- TLS certificates
- JWT secrets
- Encryption keys

### Security Principles

1. **Zero-Trust**: Never commit secrets to version control
2. **Least Privilege**: Secrets accessible only to required pods
3. **Rotation**: Regular secret rotation with zero downtime
4. **Audit**: All secret access logged and monitored
5. **Encryption**: Secrets encrypted at rest and in transit
6. **External Storage**: Secrets stored in dedicated secret management systems

## Kubernetes Secrets

### Basic Secret Creation

#### Manual Secret Creation

```bash
# Create generic secret
kubectl create secret generic erlmcp-secrets \
  --from-literal=erlang-cookie=$(openssl rand -base64 32) \
  --from-literal=jwt-secret=$(openssl rand -base64 64) \
  --from-literal=encryption-key=$(openssl rand -hex 32) \
  --namespace erlmcp

# Create TLS secret
kubectl create secret tls erlmcp-tls \
  --cert=path/to/tls.crt \
  --key=path/to/tls.key \
  --namespace erlmcp

# Create Docker registry secret
kubectl create secret docker-registry erlmcp-registry \
  --docker-server=docker.io \
  --docker-username=username \
  --docker-password=password \
  --docker-email=email@example.com \
  --namespace erlmcp
```

#### Secret Manifest

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-secrets
  namespace: erlmcp
  labels:
    app.kubernetes.io/name: erlmcp
    app.kubernetes.io/component: secrets
type: Opaque
stringData:
  # Erlang distributed
  erlang-cookie: "change-this-secure-random-string"

  # Database
  db-host: "postgres-service.erlmcp.svc.cluster.local"
  db-port: "5432"
  db-name: "erlmcp"
  db-user: "erlmcp"
  db-password: "secure-database-password"
  db-url: "postgresql://erlmcp:secure-database-password@postgres-service:5432/erlmcp?sslmode=require"

  # Redis
  redis-host: "redis-service.erlmcp.svc.cluster.local"
  redis-port: "6379"
  redis-password: "secure-redis-password"
  redis-url: "redis://:secure-redis-password@redis-service:6379/0"

  # Authentication
  jwt-secret: "your-jwt-secret-key-64-bytes-minimum"
  session-secret: "your-session-secret-key"
  api-key: "your-api-key"

  # Encryption
  encryption-key: "your-encryption-key-32-bytes-hex"

  # OAuth (if enabled)
  oauth-client-id: "your-oauth-client-id"
  oauth-client-secret: "your-oauth-client-secret"
```

### Using Secrets in Pods

#### Environment Variables from Secret

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  template:
    spec:
      containers:
        - name: erlmcp
          env:
            # Individual secret values
            - name: ERLANG_COOKIE
              valueFrom:
                secretKeyRef:
                  name: erlmcp-secrets
                  key: erlang-cookie

            - name: DB_PASSWORD
              valueFrom:
                secretKeyRef:
                  name: erlmcp-secrets
                  key: db-password

            # All secrets as environment variables
            envFrom:
              - secretRef:
                  name: erlmcp-secrets
```

#### Volume Mounts from Secret

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  template:
    spec:
      containers:
        - name: erlmcp
          volumeMounts:
            - name: secrets
              mountPath: /etc/secrets
              readOnly: true

            - name: tls-certs
              mountPath: /etc/tls
              readOnly: true

      volumes:
        - name: secrets
          secret:
            secretName: erlmcp-secrets
            defaultMode: 0400

        - name: tls-certs
          secret:
            secretName: erlmcp-tls
            items:
              - key: tls.crt
                path: server.crt
              - key: tls.key
                path: server.key
                mode: 0400
```

### Encryption at Rest

#### Enable Encryption Provider

```yaml
# /etc/kubernetes/encryption-config.yaml
apiVersion: apiserver.config.k8s.io/v1
kind: EncryptionConfiguration
resources:
  - resources:
      - secrets
    providers:
      - aescbc:
          keys:
            - name: key1
              secret: <base64-encoded-32-byte-key>
      - identity: {}
```

```bash
# Apply encryption configuration
kube-apiserver --encryption-provider-config=/etc/kubernetes/encryption-config.yaml

# Verify encryption
kubectl get secrets --all-namespaces -o json | kubectl replace -f -
```

## External Secrets Operator

External Secrets Operator (ESO) synchronizes secrets from external secret management systems to Kubernetes Secrets.

### Installation

```bash
# Add Helm repository
helm repo add external-secrets https://charts.external-secrets.io
helm repo update

# Install External Secrets Operator
helm install external-secrets \
  external-secrets/external-secrets \
  --namespace external-secrets-system \
  --create-namespace \
  --set installCRDs=true \
  --wait

# Verify installation
kubectl get pods -n external-secrets-system
```

### SecretStore Configuration

erlmcp v3 Helm chart includes SecretStore templates. Enable in `values.yaml`:

```yaml
externalSecrets:
  enabled: true

  secretStore:
    # HashiCorp Vault
    vault:
      enabled: true
      name: "erlmcp-vault-store"
      address: "https://vault.vault.svc.cluster.local:8200"
      path: "secret"
      version: "v2"
      authMountPath: "kubernetes"
      role: "erlmcp"
      secretRef: "erlmcp-vault-auth"

    # AWS Secrets Manager
    aws:
      enabled: false
      name: "erlmcp-aws-store"
      service: "SecretsManager"
      region: "us-east-1"
      serviceAccount: "erlmcp"

    # Azure Key Vault
    azure:
      enabled: false
      name: "erlmcp-azure-store"
      tenantId: "your-tenant-id"
      vaultUrl: "https://your-vault.vault.azure.net"
      identityId: "your-managed-identity-id"

    # GCP Secret Manager
    gcp:
      enabled: false
      name: "erlmcp-gcp-store"
      project: "your-gcp-project"
      clusterLocation: "us-central1"
      clusterName: "your-cluster"
      serviceAccount: "erlmcp"
```

### ExternalSecret Configuration

```yaml
externalSecrets:
  secrets:
    # Database credentials
    - name: erlmcp-db-credentials
      key: "database/erlmcp"
      refreshInterval: "1h"
      data:
        - secretKey: db-password
          remoteRef:
            key: database/erlmcp
            property: password
        - secretKey: db-user
          remoteRef:
            key: database/erlmcp
            property: username
        - secretKey: db-host
          remoteRef:
            key: database/erlmcp
            property: host

    # Erlang cookie
    - name: erlmcp-erlang-cookie
      key: "erlang/cookie"
      refreshInterval: "24h"
      data:
        - secretKey: erlang-cookie
          remoteRef:
            key: erlang/cookie
            property: value

    # JWT secret
    - name: erlmcp-jwt-secret
      key: "auth/jwt"
      refreshInterval: "1h"
      data:
        - secretKey: jwt-secret
          remoteRef:
            key: auth/jwt
            property: secret
```

### ClusterSecretStore (Multi-Cluster)

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ClusterSecretStore
metadata:
  name: erlmcp-cluster-vault-store
spec:
  provider:
    vault:
      server: "https://vault.example.com:8200"
      path: "secret"
      version: "v2"
      auth:
        kubernetes:
          mountPath: "kubernetes"
          role: "erlmcp-cluster"
          secretRef:
            name: erlmcp-vault-auth
            namespace: erlmcp-system
```

## HashiCorp Vault Integration

### Vault Installation

```bash
# Add Vault Helm repository
helm repo add hashicorp https://helm.releases.hashicorp.com
helm repo update

# Install Vault in HA mode
helm install vault hashicorp/vault \
  --namespace vault \
  --create-namespace \
  --set server.ha.enabled=true \
  --set server.ha.replicas=3 \
  --set injector.enabled=false \
  --wait

# Initialize Vault
kubectl exec -it vault-0 -n vault -- vault operator init

# Unseal Vault (repeat for each pod)
kubectl exec -it vault-0 -n vault -- vault operator unseal <key-1>
kubectl exec -it vault-0 -n vault -- vault operator unseal <key-2>
kubectl exec -it vault-0 -n vault -- vault operator unseal <key-3>
```

### Vault Configuration

```bash
# Login to Vault
export VAULT_ADDR="http://127.0.0.1:8200"
kubectl port-forward -n vault vault-0 8200:8200 &
vault login <root-token>

# Enable KV v2 secrets engine
vault secrets enable -path=secret kv-v2

# Create secrets
vault kv put secret/database/erlmcp \
  host="postgres-service.erlmcp.svc.cluster.local" \
  port="5432" \
  database="erlmcp" \
  username="erlmcp" \
  password="$(openssl rand -base64 32)"

vault kv put secret/erlang/cookie \
  value="$(openssl rand -base64 32)"

vault kv put secret/auth/jwt \
  secret="$(openssl rand -base64 64)"

# Enable Kubernetes auth
vault auth enable kubernetes

# Configure Kubernetes auth
vault write auth/kubernetes/config \
  kubernetes_host="https://$KUBERNETES_SERVICE_HOST:$KUBERNETES_SERVICE_PORT" \
  kubernetes_ca_cert=@/var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
  token_reviewer_jwt=@/var/run/secrets/kubernetes.io/serviceaccount/token

# Create policy
vault policy write erlmcp - <<EOF
path "secret/data/database/erlmcp" {
  capabilities = ["read"]
}
path "secret/data/erlang/cookie" {
  capabilities = ["read"]
}
path "secret/data/auth/*" {
  capabilities = ["read"]
}
EOF

# Create role
vault write auth/kubernetes/role/erlmcp \
  bound_service_account_names=erlmcp \
  bound_service_account_namespaces=erlmcp \
  policies=erlmcp \
  ttl=24h
```

### Vault Agent Injector (Alternative)

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  template:
    metadata:
      annotations:
        vault.hashicorp.com/agent-inject: "true"
        vault.hashicorp.com/role: "erlmcp"
        vault.hashicorp.com/agent-inject-secret-database: "secret/data/database/erlmcp"
        vault.hashicorp.com/agent-inject-template-database: |
          {{- with secret "secret/data/database/erlmcp" -}}
          export DB_HOST="{{ .Data.data.host }}"
          export DB_PORT="{{ .Data.data.port }}"
          export DB_NAME="{{ .Data.data.database }}"
          export DB_USER="{{ .Data.data.username }}"
          export DB_PASSWORD="{{ .Data.data.password }}"
          {{- end }}
    spec:
      serviceAccountName: erlmcp
      containers:
        - name: erlmcp
          command: ["/bin/sh", "-c"]
          args:
            - source /vault/secrets/database && exec /app/start.sh
```

## Cloud Provider Integration

### AWS Secrets Manager

#### IRSA (IAM Roles for Service Accounts)

```yaml
# Service Account with IAM annotation
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: erlmcp
  annotations:
    eks.amazonaws.com/role-arn: arn:aws:iam::ACCOUNT_ID:role/erlmcp-secrets-role
```

#### ExternalSecret for AWS

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: erlmcp-aws-secrets
  namespace: erlmcp
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: erlmcp-aws-store
    kind: SecretStore
  target:
    name: erlmcp-secrets
    creationPolicy: Owner
  data:
    - secretKey: db-password
      remoteRef:
        key: erlmcp/database
        property: password
    - secretKey: erlang-cookie
      remoteRef:
        key: erlmcp/erlang
        property: cookie
```

#### IAM Policy

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "secretsmanager:GetSecretValue",
        "secretsmanager:DescribeSecret"
      ],
      "Resource": [
        "arn:aws:secretsmanager:us-east-1:ACCOUNT_ID:secret:erlmcp/*"
      ]
    }
  ]
}
```

### Azure Key Vault

#### Managed Identity

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: erlmcp
  annotations:
    azure.workload.identity/client-id: "your-client-id"
```

#### ExternalSecret for Azure

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: erlmcp-azure-secrets
  namespace: erlmcp
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: erlmcp-azure-store
    kind: SecretStore
  target:
    name: erlmcp-secrets
    creationPolicy: Owner
  data:
    - secretKey: db-password
      remoteRef:
        key: erlmcp-db-password
    - secretKey: erlang-cookie
      remoteRef:
        key: erlmcp-erlang-cookie
```

### GCP Secret Manager

#### Workload Identity

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: erlmcp
  annotations:
    iam.gke.io/gcp-service-account: erlmcp@PROJECT_ID.iam.gserviceaccount.com
```

#### ExternalSecret for GCP

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: erlmcp-gcp-secrets
  namespace: erlmcp
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: erlmcp-gcp-store
    kind: SecretStore
  target:
    name: erlmcp-secrets
    creationPolicy: Owner
  data:
    - secretKey: db-password
      remoteRef:
        key: erlmcp-db-password
        version: latest
    - secretKey: erlang-cookie
      remoteRef:
        key: erlmcp-erlang-cookie
        version: latest
```

## Secret Rotation

### Automated Rotation with External Secrets

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: erlmcp-rotating-secrets
  namespace: erlmcp
spec:
  refreshInterval: 15m  # Check every 15 minutes
  secretStoreRef:
    name: erlmcp-vault-store
    kind: SecretStore
  target:
    name: erlmcp-secrets
    creationPolicy: Owner
    deletionPolicy: Retain
  data:
    - secretKey: api-key
      remoteRef:
        key: erlmcp/api-key
        version: latest  # Always fetch latest version
```

### Rolling Restart on Secret Change

```yaml
# Reloader (stakater/reloader)
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
  annotations:
    secret.reloader.stakater.com/reload: "erlmcp-secrets"
spec:
  template:
    metadata:
      annotations:
        checksum/secret: {{ include (print $.Template.BasePath "/secret.yaml") . | sha256sum }}
```

### Manual Secret Rotation

```bash
# Step 1: Create new secret version
kubectl create secret generic erlmcp-secrets-v2 \
  --from-literal=erlang-cookie=$(openssl rand -base64 32) \
  --namespace erlmcp

# Step 2: Update deployment to use new secret
kubectl patch deployment erlmcp -n erlmcp \
  --type='json' \
  -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/envFrom/0/secretRef/name", "value":"erlmcp-secrets-v2"}]'

# Step 3: Wait for rollout
kubectl rollout status deployment erlmcp -n erlmcp

# Step 4: Delete old secret
kubectl delete secret erlmcp-secrets -n erlmcp
```

## Best Practices

### 1. Never Commit Secrets

```bash
# Add to .gitignore
echo "*.secret" >> .gitignore
echo "*-secrets.yaml" >> .gitignore
echo ".env*" >> .gitignore
```

### 2. Use Separate Secrets Per Environment

```
erlmcp-secrets-dev
erlmcp-secrets-staging
erlmcp-secrets-production
```

### 3. Principle of Least Privilege

```yaml
# RBAC for secret access
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: erlmcp-secrets-reader
  namespace: erlmcp
rules:
  - apiGroups: [""]
    resources: ["secrets"]
    resourceNames: ["erlmcp-secrets"]
    verbs: ["get"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: erlmcp-secrets-binding
  namespace: erlmcp
subjects:
  - kind: ServiceAccount
    name: erlmcp
    namespace: erlmcp
roleRef:
  kind: Role
  name: erlmcp-secrets-reader
  apiGroup: rbac.authorization.k8s.io
```

### 4. Audit Secret Access

```yaml
# Audit policy for secrets
apiVersion: audit.k8s.io/v1
kind: Policy
rules:
  - level: RequestResponse
    resources:
      - group: ""
        resources: ["secrets"]
    verbs: ["get", "list", "watch", "create", "update", "patch", "delete"]
```

### 5. Regular Rotation

- Database passwords: Every 90 days
- API keys: Every 30 days
- TLS certificates: Before expiration (auto-renewed)
- Erlang cookie: Every 180 days

### 6. Secret Validation

```yaml
# Init container to validate secrets
initContainers:
  - name: validate-secrets
    image: busybox
    command:
      - sh
      - -c
      - |
        if [ -z "$ERLANG_COOKIE" ]; then
          echo "ERROR: ERLANG_COOKIE not set"
          exit 1
        fi
        if [ -z "$DB_PASSWORD" ]; then
          echo "ERROR: DB_PASSWORD not set"
          exit 1
        fi
        echo "Secret validation passed"
    envFrom:
      - secretRef:
          name: erlmcp-secrets
```

## Troubleshooting

### Issue: ExternalSecret not syncing

```bash
# Check ExternalSecret status
kubectl describe externalsecret erlmcp-secrets -n erlmcp

# Check SecretStore status
kubectl describe secretstore erlmcp-vault-store -n erlmcp

# Check External Secrets Operator logs
kubectl logs -n external-secrets-system -l app.kubernetes.io/name=external-secrets

# Force sync
kubectl annotate externalsecret erlmcp-secrets -n erlmcp \
  force-sync="$(date +%s)" --overwrite
```

### Issue: Vault authentication failure

```bash
# Verify service account token
kubectl exec -it vault-0 -n vault -- vault write auth/kubernetes/login \
  role=erlmcp \
  jwt=@/var/run/secrets/kubernetes.io/serviceaccount/token

# Check Vault logs
kubectl logs -n vault vault-0
```

### Issue: Secret not mounted in pod

```bash
# Check pod events
kubectl describe pod erlmcp-0 -n erlmcp

# Verify secret exists
kubectl get secret erlmcp-secrets -n erlmcp

# Check RBAC permissions
kubectl auth can-i get secrets --as=system:serviceaccount:erlmcp:erlmcp -n erlmcp
```

## Docker-Only Validation

Per CLAUDE.md requirements:

```bash
# Validate secret configuration
docker compose run erlmcp-build kubectl apply --dry-run=client -f /workspace/k8s/deployments/helm/erlmcp/templates/secret.yaml

# Test External Secrets Operator
docker compose run erlmcp-build kubectl apply --dry-run=client -f /workspace/k8s/deployments/helm/erlmcp/templates/external-secrets/
```

## Conclusion

Secure secrets management is critical for erlmcp v3 production deployments:
- Use External Secrets Operator for enterprise deployments
- Integrate with cloud-native secret managers (Vault, AWS, Azure, GCP)
- Implement regular secret rotation
- Audit all secret access
- Never commit secrets to version control

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3/kubernetes/secrets
