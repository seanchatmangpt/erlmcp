# ERLMCP Enterprise Helm Chart

Production-ready Helm chart for deploying ERLMCP v3 Enterprise on Kubernetes.

## Features

- **Zero-downtime deployments** with RollingUpdate and StatefulSet support
- **Horizontal Pod Autoscaling** based on CPU and memory metrics
- **Pod Disruption Budgets** for guaranteed availability during maintenance
- **Network Policies** implementing zero-trust security model
- **Service Accounts & RBAC** with least-privilege access
- **ConfigMaps** for environment-specific configuration
- **Secrets** management with auto-generated credentials
- **ServiceMonitors** for Prometheus Operator integration
- **PrometheusRules** for alerting
- **Persistent Volume Claims** for data persistence
- **Ingress** with TLS certificate support

## Quick Start

### Prerequisites

- Kubernetes 1.19+
- Helm 3.0+
- For monitoring: Prometheus Operator installed

### Install

```bash
# Add Helm repository (if published)
helm repo add erlmcp https://charts.erlmcp.io
helm repo update

# Install for production
helm install erlmcp-prod ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --create-namespace \
  --values helm/erlmcp-enterprise/values-production.yaml

# Install for development
helm install erlmcp-dev ./helm/erlmcp-enterprise \
  --namespace erlmcp-dev \
  --create-namespace \
  --values helm/erlmcp-enterprise/values-dev.yaml

# Install for staging
helm install erlmcp-staging ./helm/erlmcp-enterprise \
  --namespace erlmcp-staging \
  --create-namespace \
  --values helm/erlmcp-enterprise/values-staging.yaml
```

### Using the deployment script

```bash
# Deploy to production
./helm/erlmcp-enterprise/deploy.sh production erlmcp

# Deploy to staging
./helm/erlmcp-enterprise/deploy.sh staging erlmcp-staging

# Deploy to development
./helm/erlmcp-enterprise/deploy.sh dev erlmcp-dev

# Check status
./helm/erlmcp-enterprise/deploy.sh production erlmcp status

# View logs
./helm/erlmcp-enterprise/deploy.sh production erlmcp logs

# Uninstall
./helm/erlmcp-enterprise/deploy.sh production erlmcp uninstall
```

## Configuration

### Global Settings

| Parameter | Description | Default |
|-----------|-------------|---------|
| `global.cluster.name` | Cluster name | `erlmcp-enterprise` |
| `global.cluster.environment` | Environment (dev/staging/production) | `production` |
| `global.cluster.region` | AWS/GCP region | `us-east-1` |

### Deployment Settings

| Parameter | Description | Default |
|-----------|-------------|---------|
| `deployment.enabled` | Enable Deployment | `true` |
| `deployment.replicas` | Number of replicas | `3` |
| `resources.requests.cpu` | CPU request | `500m` |
| `resources.requests.memory` | Memory request | `1Gi` |
| `resources.limits.cpu` | CPU limit | `2000m` |
| `resources.limits.memory` | Memory limit | `4Gi` |

### Autoscaling

| Parameter | Description | Default |
|-----------|-------------|---------|
| `autoscaling.enabled` | Enable HPA | `true` |
| `autoscaling.minReplicas` | Minimum replicas | `3` |
| `autoscaling.maxReplicas` | Maximum replicas | `100` |
| `autoscaling.targetCPUUtilizationPercentage` | Target CPU % | `70` |

### Service Settings

| Parameter | Description | Default |
|-----------|-------------|---------|
| `service.type` | Service type | `ClusterIP` |
| `service.ports.http.port` | HTTP port | `80` |
| `service.ports.http.targetPort` | Container HTTP port | `8080` |

### Storage

| Parameter | Description | Default |
|-----------|-------------|---------|
| `storage.persistentVolumeClaim.enabled` | Enable PVC | `true` |
| `storage.persistentVolumeClaim.size` | PVC size | `100Gi` |
| `storage.persistentVolumeClaim.storageClass` | Storage class | `""` |

### Security

| Parameter | Description | Default |
|-----------|-------------|---------|
| `security.rbac.create` | Create RBAC resources | `true` |
| `security.networkPolicy.enabled` | Enable NetworkPolicy | `true` |
| `security.secrets.enabled` | Create Secrets | `true` |

## Values Files

The chart includes pre-configured values for each environment:

- `values.yaml` - Default values
- `values-dev.yaml` - Development (1 replica, minimal resources)
- `values-staging.yaml` - Staging (2 replicas, moderate resources)
- `values-production.yaml` - Production (3 replicas, full resources)

## Upgrading

```bash
helm upgrade erlmcp-prod ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --values helm/erlmcp-enterprise/values-production.yaml
```

## Uninstalling

```bash
helm uninstall erlmcp-prod --namespace erlmcp
```

## Troubleshooting

### Check pod status

```bash
kubectl get pods -n erlmcp -l app.kubernetes.io/name=erlmcp-enterprise
```

### View logs

```bash
kubectl logs -n erlmcp -l app.kubernetes.io/name=erlmcp-enterprise --tail=100 -f
```

### Describe pod

```bash
kubectl describe pod -n erlmcp <pod-name>
```

### Get events

```bash
kubectl get events -n erlmcp --sort-by='.lastTimestamp'
```

## License

Copyright (c) 2024 ERLMCP Team
