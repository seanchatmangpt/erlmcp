# erlmcp v3 Helm Chart Usage and Customization

## Table of Contents

- [Overview](#overview)
- [Chart Structure](#chart-structure)
- [Installation](#installation)
- [Configuration](#configuration)
- [Customization](#customization)
- [Environment-Specific Deployments](#environment-specific-deployments)
- [Advanced Configuration](#advanced-configuration)
- [Troubleshooting](#troubleshooting)

## Overview

The erlmcp v3 Helm charts provide enterprise-grade Kubernetes deployments with comprehensive configuration options. Two primary charts are available:

- **`helm/erlmcp-enterprise`**: Full enterprise deployment with all features
- **`k8s/deployments/helm/erlmcp`**: Production-grade deployment with external secrets integration

### Chart Locations

```
erlmcp/
├── helm/erlmcp-enterprise/          # Enterprise chart
│   ├── Chart.yaml
│   ├── values.yaml
│   ├── values-dev.yaml
│   ├── values-staging.yaml
│   ├── values-production.yaml
│   └── templates/
└── k8s/deployments/helm/erlmcp/     # Production chart
    ├── Chart.yaml
    ├── values.yaml
    └── templates/
```

## Chart Structure

### Enterprise Chart Components

The `erlmcp-enterprise` chart includes:

- **Deployment**: Stateless application pods
- **StatefulSet**: Stateful clustered Erlang nodes
- **Services**: ClusterIP, headless, and LoadBalancer services
- **Ingress**: NGINX ingress with TLS
- **ConfigMaps**: Application and logging configuration
- **Secrets**: Credentials and certificates
- **PVC**: Persistent volume claims for data
- **HPA**: Horizontal Pod Autoscaler
- **PDB**: Pod Disruption Budget
- **ServiceMonitor**: Prometheus monitoring
- **NetworkPolicy**: Network security policies
- **RBAC**: Role-based access control
- **ResourceQuota**: Resource limits per namespace
- **LimitRange**: Container resource constraints

## Installation

### Prerequisites

```bash
# Verify Kubernetes cluster
kubectl version --client
kubectl cluster-info

# Verify Helm installation
helm version

# Add required Helm repositories (if using dependencies)
helm repo add bitnami https://charts.bitnami.com/bitnami
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo update
```

### Basic Installation

#### Development Environment

```bash
# Install with development values
helm install erlmcp-dev ./helm/erlmcp-enterprise \
  --namespace erlmcp-dev \
  --create-namespace \
  --values ./helm/erlmcp-enterprise/values-dev.yaml \
  --wait \
  --timeout 10m
```

#### Staging Environment

```bash
# Install with staging values
helm install erlmcp-staging ./helm/erlmcp-enterprise \
  --namespace erlmcp-staging \
  --create-namespace \
  --values ./helm/erlmcp-enterprise/values-staging.yaml \
  --wait \
  --timeout 15m
```

#### Production Environment

```bash
# Install with production values
helm install erlmcp-prod ./helm/erlmcp-enterprise \
  --namespace erlmcp-prod \
  --create-namespace \
  --values ./helm/erlmcp-enterprise/values-production.yaml \
  --wait \
  --timeout 20m
```

### Installation with Custom Values

```bash
# Create custom values file
cat > custom-values.yaml <<EOF
global:
  cluster:
    name: "my-erlmcp-cluster"
    environment: "production"
    region: "us-west-2"

deployment:
  replicas: 5

resources:
  requests:
    cpu: "2000m"
    memory: "4Gi"
  limits:
    cpu: "8000m"
    memory: "16Gi"

autoscaling:
  enabled: true
  minReplicas: 5
  maxReplicas: 50
EOF

# Install with custom values
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --create-namespace \
  --values ./helm/erlmcp-enterprise/values.yaml \
  --values custom-values.yaml \
  --wait
```

### Dry Run and Template Validation

```bash
# Validate templates without installing
helm template erlmcp ./helm/erlmcp-enterprise \
  --values ./helm/erlmcp-enterprise/values-production.yaml \
  --namespace erlmcp \
  --debug > /tmp/rendered-manifests.yaml

# Dry run installation
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --values ./helm/erlmcp-enterprise/values-production.yaml \
  --dry-run \
  --debug
```

## Configuration

### Global Settings

```yaml
global:
  cluster:
    name: "erlmcp-production"       # Cluster identifier
    region: "us-east-1"             # Cloud region
    environment: "production"        # Environment: dev/staging/production
    dnsSuffix: "cluster.local"      # Kubernetes DNS suffix

  monitoring:
    enabled: true
    prometheus:
      enabled: true
    grafana:
      enabled: true

  serviceMesh:
    enabled: false                   # Enable Istio/Linkerd
    provider: "istio"                # istio or linkerd
```

### Image Configuration

```yaml
image:
  registry: "docker.io"
  repository: "erlang-mcp/erlmcp"
  tag: "3.0.0"                       # Override with --set image.tag=3.0.1
  pullPolicy: "IfNotPresent"
  imagePullSecrets: []
```

### Deployment vs StatefulSet

Both deployment modes can be enabled simultaneously:

```yaml
# Stateless deployment for API/gateway nodes
deployment:
  enabled: true
  replicas: 3
  maxUnavailable: 0                  # Zero-downtime updates
  maxSurge: 1

# Stateful deployment for clustered Erlang nodes
statefulset:
  enabled: true
  replicas: 3
  podManagementPolicy: "OrderedReady"  # OrderedReady or Parallel
  partition: 0                       # For canary updates
```

### Resource Configuration

```yaml
resources:
  requests:
    cpu: "1000m"
    memory: "2Gi"
  limits:
    cpu: "4000m"
    memory: "8Gi"
```

### Autoscaling Configuration

```yaml
autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 100
  targetCPUUtilizationPercentage: 70
  targetMemoryUtilizationPercentage: 80

  # Advanced scaling behavior
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
        - type: Pods
          value: 1
          periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
        - type: Pods
          value: 5
          periodSeconds: 60
```

### Storage Configuration

```yaml
storage:
  persistentVolumeClaim:
    enabled: true
    name: "erlmcp-data"
    size: "100Gi"
    accessMode: "ReadWriteOnce"
    storageClass: "gp3"              # AWS gp3, Azure premium-ssd, GCP pd-ssd

  logs:
    enabled: true
    name: "erlmcp-logs"
    size: "20Gi"
    storageClass: "gp3"
```

### Security Configuration

```yaml
security:
  rbac:
    create: true
    serviceAccountName: ""           # Auto-generated if empty
    createClusterRoleBinding: false

  networkPolicy:
    enabled: true
    egressOnly: false

  secrets:
    enabled: true
    name: "erlmcp-secrets"
    erlangCookie: ""                 # Auto-generated if empty
    jwtSecret: ""
    encryptionKey: ""
```

## Customization

### Custom Application Configuration

Create a custom ConfigMap:

```yaml
# custom-config.yaml
app:
  erlang:
    cookie: "my-secure-cookie"
    env:
      ERLMCP_CLUSTER_NAME: "production-cluster"
      ERLMCP_LOG_LEVEL: "warning"
      ERLMCP_METRICS_ENABLED: "true"
      ERLMCP_MAX_CONNECTIONS: "50000"
      ERLMCP_BACKPRESSURE_ENABLED: "true"
      ERLMCP_QUOTA_PER_TENANT: "1000"
```

### Custom Probes

```yaml
deployment:
  livenessProbe:
    httpGet:
      path: /health
      port: http
    initialDelaySeconds: 30
    periodSeconds: 10
    timeoutSeconds: 5
    failureThreshold: 3

  readinessProbe:
    httpGet:
      path: /ready
      port: http
    initialDelaySeconds: 10
    periodSeconds: 5
    timeoutSeconds: 3
    failureThreshold: 3

  startupProbe:
    httpGet:
      path: /ready
      port: http
    initialDelaySeconds: 5
    periodSeconds: 5
    timeoutSeconds: 3
    failureThreshold: 30             # 150s max startup time
```

### Custom Init Containers

```yaml
deployment:
  initContainers:
    - name: wait-for-postgres
      image: busybox:1.36
      command:
        - sh
        - -c
        - |
          until nc -z postgres-service 5432; do
            echo "Waiting for PostgreSQL..."
            sleep 2
          done
    - name: wait-for-redis
      image: busybox:1.36
      command:
        - sh
        - -c
        - |
          until nc -z redis-service 6379; do
            echo "Waiting for Redis..."
            sleep 2
          done
```

### Custom Sidecars

```yaml
deployment:
  sidecars:
    - name: log-forwarder
      image: fluent/fluent-bit:2.0
      volumeMounts:
        - name: logs
          mountPath: /var/log/erlmcp
      resources:
        requests:
          cpu: "100m"
          memory: "128Mi"
        limits:
          cpu: "200m"
          memory: "256Mi"
```

## Environment-Specific Deployments

### Development Environment

```yaml
# values-dev.yaml
global:
  cluster:
    environment: "development"

deployment:
  replicas: 1

resources:
  requests:
    cpu: "100m"
    memory: "256Mi"
  limits:
    cpu: "500m"
    memory: "512Mi"

autoscaling:
  enabled: false

podDisruptionBudget:
  enabled: false

monitoring:
  prometheus:
    enabled: false
```

### Staging Environment

```yaml
# values-staging.yaml
global:
  cluster:
    environment: "staging"

deployment:
  replicas: 2

resources:
  requests:
    cpu: "250m"
    memory: "512Mi"
  limits:
    cpu: "1000m"
    memory: "2Gi"

autoscaling:
  enabled: true
  minReplicas: 2
  maxReplicas: 10
```

### Production Environment

```yaml
# values-production.yaml
global:
  cluster:
    environment: "production"

deployment:
  replicas: 3
  maxUnavailable: 0                  # Zero-downtime updates

resources:
  requests:
    cpu: "1000m"
    memory: "2Gi"
  limits:
    cpu: "4000m"
    memory: "8Gi"

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 100

podDisruptionBudget:
  enabled: true
  minAvailable: 2

monitoring:
  prometheus:
    enabled: true
    serviceMonitor:
      enabled: true
      interval: "15s"
```

## Advanced Configuration

### Multi-Tenant Configuration

```yaml
resourceQuota:
  enabled: true

  tenants:
    production:
      enabled: true
      namespace: "erlmcp-production"
      tier: "platinum"
      resources:
        requests.cpu: "16"
        requests.memory: 32Gi
        limits.cpu: "32"
        limits.memory: 64Gi
        pods: "50"

    staging:
      enabled: true
      namespace: "erlmcp-staging"
      tier: "gold"
      resources:
        requests.cpu: "8"
        requests.memory: 16Gi
        limits.cpu: "16"
        limits.memory: 32Gi
        pods: "30"
```

### Service Mesh Integration

```yaml
global:
  serviceMesh:
    enabled: true
    provider: "istio"

tls:
  mtls:
    enabled: true
    mode: "STRICT"
    cipherSuites:
      - "TLS_AES_128_GCM_SHA256"
      - "TLS_AES_256_GCM_SHA384"
      - "TLS_CHACHA20_POLY1305_SHA256"
    minProtocolVersion: "TLSV1_3"
```

### External Secrets Integration

```yaml
externalSecrets:
  enabled: true

  secretStore:
    vault:
      enabled: true
      address: "https://vault.vault.svc.cluster.local:8200"
      path: "secret"
      version: "v2"
      role: "erlmcp"

  secrets:
    - name: erlmcp-db-credentials
      key: "database/erlmcp"
      refreshInterval: "1h"
      data:
        - secretKey: db-password
          remoteRef:
            key: database/erlmcp
            property: password
```

## Upgrade and Rollback

### Upgrade Chart

```bash
# Upgrade to new version
helm upgrade erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --values ./helm/erlmcp-enterprise/values-production.yaml \
  --wait \
  --timeout 15m

# Upgrade with new image tag
helm upgrade erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --reuse-values \
  --set image.tag=3.0.1 \
  --wait
```

### Rollback

```bash
# View release history
helm history erlmcp --namespace erlmcp

# Rollback to previous version
helm rollback erlmcp --namespace erlmcp --wait

# Rollback to specific revision
helm rollback erlmcp 3 --namespace erlmcp --wait
```

## Troubleshooting

### Validate Chart

```bash
# Lint chart
helm lint ./helm/erlmcp-enterprise \
  --values ./helm/erlmcp-enterprise/values-production.yaml

# Verify chart structure
helm show chart ./helm/erlmcp-enterprise

# Show all values
helm show values ./helm/erlmcp-enterprise
```

### Debug Installation

```bash
# Enable debug output
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --debug \
  --dry-run

# Get rendered manifests
helm get manifest erlmcp --namespace erlmcp

# Get values
helm get values erlmcp --namespace erlmcp
```

### Common Issues

#### Issue: Chart fails to install

```bash
# Check events
kubectl get events --namespace erlmcp --sort-by='.lastTimestamp'

# Check pod status
kubectl get pods --namespace erlmcp

# Check pod logs
kubectl logs -l app.kubernetes.io/name=erlmcp --namespace erlmcp
```

#### Issue: Persistent volume not binding

```bash
# Check PVC status
kubectl get pvc --namespace erlmcp

# Describe PVC
kubectl describe pvc erlmcp-data --namespace erlmcp

# Check storage classes
kubectl get storageclass
```

#### Issue: Image pull errors

```bash
# Check image pull secrets
kubectl get secrets --namespace erlmcp

# Create image pull secret
kubectl create secret docker-registry erlmcp-registry \
  --docker-server=docker.io \
  --docker-username=username \
  --docker-password=password \
  --namespace erlmcp

# Update values.yaml
image:
  imagePullSecrets:
    - name: erlmcp-registry
```

## Best Practices

### 1. Version Control

- Store all custom values files in version control
- Use separate values files per environment
- Document all customizations

### 2. Secret Management

- Never commit secrets to version control
- Use external secret management (Vault, AWS Secrets Manager)
- Rotate secrets regularly

### 3. Resource Planning

- Set appropriate resource requests and limits
- Use resource quotas to prevent resource exhaustion
- Monitor actual resource usage and adjust

### 4. High Availability

- Enable Pod Disruption Budgets
- Use pod anti-affinity for distribution
- Set minAvailable >= 2 for critical services

### 5. Monitoring

- Enable ServiceMonitor for Prometheus
- Configure alerting rules
- Set up Grafana dashboards

### 6. Testing

- Always test in development/staging first
- Use `--dry-run` before production deployments
- Validate manifests with tools like kubeval

## Reference

### Helm Commands

```bash
# Install
helm install <release> <chart> --namespace <ns> --values <file>

# Upgrade
helm upgrade <release> <chart> --namespace <ns> --values <file>

# Rollback
helm rollback <release> <revision> --namespace <ns>

# Uninstall
helm uninstall <release> --namespace <ns>

# List releases
helm list --namespace <ns>

# Get values
helm get values <release> --namespace <ns>

# Get manifest
helm get manifest <release> --namespace <ns>
```

### Chart Values Precedence

1. Values from `--set` flags (highest priority)
2. Values from `--values` files (last file wins)
3. Default values from `values.yaml` (lowest priority)

### Docker-Only Execution

Per CLAUDE.md requirements, all validation and testing must be done via Docker:

```bash
# Validate chart via Docker
docker compose run erlmcp-build sh -c "helm lint /workspace/helm/erlmcp-enterprise"

# Test deployment via Docker
docker compose run erlmcp-build sh -c "helm template erlmcp /workspace/helm/erlmcp-enterprise --values /workspace/helm/erlmcp-enterprise/values-production.yaml"
```

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3/kubernetes/helm
