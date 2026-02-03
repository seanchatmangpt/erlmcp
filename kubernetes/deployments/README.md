# erlmcp v3 Kubernetes Deployment Suite - Complete Production Manifests

This directory contains the complete Kubernetes deployment suite for erlmcp v3, including production-grade manifests for blue/green deployments, canary releases, and enterprise-grade operations.

## 📁 Directory Structure

```
kubernetes/
├── README.md                          # This documentation
├── deployments/                       # Standalone YAML manifests
│   ├── app-resources.yaml            # Complete application resources for GitOps
│   ├── configmap.yaml                 # Configuration maps
│   ├── hpa.yaml                      # Horizontal Pod Autoscaler
│   ├── ingress.yaml                  # Ingress configuration
│   ├── network-policy.yaml           # Network policies
│   ├── pdb.yaml                      # Pod disruption budgets
│   ├── secret.yaml                   # Secrets management
│   ├── services.yaml                 # Service definitions
│   └── statefulset.yaml             # StatefulSet configuration
├── helm/                            # Helm charts
│   ├── Chart.yaml                   # Chart metadata
│   ├── values.yaml                  # Default values
│   ├── templates/                   # Helm templates
│   │   ├── _helpers.tpl            # Template helpers
│   │   ├── app-resources.yaml       # Application resources
│   │   ├── certificate.yaml         # TLS certificates
│   │   ├── cluster-role.yaml        # RBAC roles
│   │   ├── cluster-role-binding.yaml  # RBAC bindings
│   │   ├── configmap.yaml          # Configuration maps
│   │   ├── cronjob.yaml           # Cron jobs
│   │   ├── deployment.yaml         # Kubernetes deployment
│   │   ├── horizontal-pod-autoscaler.yaml  # HPA
│   │   ├── ingress.yaml            # Ingress resources
│   │   ├── job.yaml                # Jobs
│   │   ├── namespace.yaml          # Namespace
│   │   ├── network-policy.yaml     # Network policies
│   │   ├── pod-disruption-budget.yaml  # PDB
│   │   ├── secret.yaml             # Secrets
│   │   ├── service-account.yaml    # Service accounts
│   │   ├── service.yaml            # Services
│   │   ├── service-monitor.yaml    # ServiceMonitor
│   │   ├── statefulset.yaml        # StatefulSet
│   │   ├── virtual-service.yaml    # Istio VirtualService
│   │   └── destination-rule.yaml   # Istio DestinationRule
│   ├── values/                      # Value overrides
│   │   ├── development.yaml         # Development environment
│   │   ├── production.yaml         # Production environment
│   │   ├── staging.yaml            # Staging environment
│   │   └── values-secrets.yaml     # Secret values
│   └── tests/                      # Helm tests
│       ├── values.yaml              # Test values
│       ├── test-backup.yaml        # Backup test
│       ├── test-configuration.yaml # Configuration test
│       ├── test-health.yaml        # Health test
│       ├── test-load.yaml          # Load test
│       ├── test-migration.yaml     # Migration test
│       └── test-restore.yaml       # Restore test
└── kustomize/                       # Kustomize overlays
    ├── base/                        # Base configuration
    │   ├── kustomization.yaml      # Base kustomization
    │   ├── configmap.yaml          # Base configmaps
    │   ├── secret.yaml             # Base secrets
    │   ├── service.yaml            # Base services
    │   └── statefulset.yaml        # StatefulSet
    ├── overlays/                   # Environment overlays
    │   ├── production/              # Production overlay
    │   │   ├── kustomization.yaml  # Production kustomization
    │   │   ├── hpa.yaml            # Production HPA
    │   │   ├── network-policy.yaml # Production network policies
    │   │   ├── pdb.yaml           # Production PDB
    │   │   └── values.yaml        # Production values
    │   ├── staging/                # Staging overlay
    │   │   ├── kustomization.yaml # Staging kustomization
    │   │   └── values.yaml        # Staging values
    │   └── development/            # Development overlay
    │       ├── kustomization.yaml # Development kustomization
    │       └── values.yaml       # Development values
    └── patches/                    # Common patches
        ├── network-policy.yaml     # Network policy patches
        ├── resource-limits.yaml   # Resource limit patches
        └── security-patches.yaml  # Security patches
```

## 🚀 Quick Start

### Prerequisites

- Kubernetes cluster (v1.25+)
- Helm v3.8+
- kubectl
- Istio (optional, for service mesh)

### Installation

#### Option 1: Helm Chart (Recommended)

```bash
# Clone the repository
git clone https://github.com/your-org/erlmcp-kubernetes.git
cd erlmcp-kubernetes

# Install with Helm
helm install erlmcp ./helm \
  --namespace erlmcp \
  --create-namespace \
  --set replicaCount=3 \
  --set node.resources.requests.cpu="500m" \
  --set node.resources.requests.memory="1Gi"

# Verify deployment
kubectl get pods -n erlmcp
kubectl get services -n erlmcp
```

#### Option 2: Kustomize

```bash
# Apply base configuration
kubectl apply -k kustomize/base/

# Apply production overlay
kubectl apply -k kustomize/overlays/production/
```

#### Option 3: Standalone YAML

```bash
# Apply all manifests
kubectl apply -f deployments/
```

## 📋 Feature Overview

### 1. **StatefulSet for Erlang Clustering**
- Stable network identities for Erlang nodes
- Automatic pod recovery and rescheduling
- Persistent volumes for data persistence
- Cluster initialization with proper bootstrapping

### 2. **Advanced Networking**
- Service mesh integration (Istio/Linkerd)
- Network policies for zero-trust security
- Load balancer configurations
- Multi-port service definitions

### 3. **Auto Scaling**
- Horizontal Pod Autoscaler (HPA)
- Custom metrics support
- Vertical Pod Autoscaler (VPA) integration
- Cluster Autoscaler integration

### 4. **High Availability**
- Pod Disruption Budgets (PDB)
- Node anti-affinity rules
- Zone-aware deployment strategies
- Graceful shutdown procedures

### 5. **Secret Management**
- Kubernetes Secrets
- External secret managers support
- Auto-rotated credentials
- Secure configuration handling

### 6. **Monitoring & Observability**
- Prometheus ServiceMonitor
- OpenTelemetry integration
- Grafana dashboards
- Health check endpoints

### 7. **Deployment Strategies**
- Blue/Green deployments
- Canary releases
- Progressive traffic shifting
- Rollback mechanisms

## 🔧 Configuration

### Helm Values

Key configuration options in `values.yaml`:

```yaml
# Cluster Configuration
cluster:
  enabled: true
  nodes: 3
  cookie: "erlmcp-cluster-3-0-0"

# Resource Management
node:
  resources:
    requests:
      cpu: "500m"
      memory: "1Gi"
    limits:
      cpu: "2"
      memory: "4Gi"

# Networking
network:
  serviceType: LoadBalancer
  loadBalancerIP: ""
  ingress:
    enabled: true
    hosts:
      - host: erlmcp.example.com
        paths: ["/"]

# Monitoring
monitoring:
  enabled: true
  prometheus:
    enabled: true
  otel:
    enabled: true
```

### Environment-Specific Overrides

#### Production

```yaml
# kustomize/overlays/production/values.yaml
node:
  replicaCount: 6
  resources:
    requests:
      cpu: "1000m"
      memory: "2Gi"
    limits:
      cpu: "4"
      memory: "8Gi"
```

#### Staging

```yaml
# kustomize/overlays/staging/values.yaml
node:
  replicaCount: 3
  resources:
    requests:
      cpu: "500m"
      memory: "1Gi"
    limits:
      cpu: "2"
      memory: "4Gi"
```

## 🔐 Security

### Network Policies

The deployment includes comprehensive network policies:

- **Zero Trust Architecture**: All traffic is denied by default
- **Service Isolation**: Services can only communicate with authorized endpoints
- **Load Balancer Ingress**: Traffic only through ingress controller
- **Mesh Integration**: Service-to-service communication via Istio
- **DNS Security**: Egress DNS allowed only to trusted services

### RBAC & Service Accounts

- Dedicated service accounts for different components
- Role-based access control for Kubernetes resources
- Secret management with proper access controls
- Audit logging enabled

### Secret Management

- Auto-generated database passwords
- Secure API key storage
- TLS certificate management
- External secret manager integration (Vault)

## 📊 Monitoring & Observability

### Metrics

- HTTP request rates and latency
- Erlang VM metrics
- Database connection pooling
- Message queue throughput
- Memory and CPU usage

### Tracing

- Request tracing with OpenTelemetry
- Distributed tracing across services
- Error propagation and root cause analysis
- Performance bottleneck identification

### Logging

- Structured JSON logging
- Log aggregation with Loki
- Log rotation and retention
- Debug mode for troubleshooting

## 🚀 Deployment Patterns

### Blue/Green Deployments

The suite supports both blue/green and canary deployment strategies:

1. **Blue/Green**: Deploy new version in parallel, switch traffic atomically
2. **Canary**: Gradually shift traffic to new version with rollback capability

### Traffic Management

- Istio VirtualService for traffic splitting
- Weight-based routing for canary releases
- Circuit breakers and fault injection
- Request mirroring for testing

## 🔧 Maintenance Operations

### Jobs

```yaml
# Migration Job
- name: erlmcp-migration
  command: ["./bin/migrate"]

# Backup Job
- name: erlmcp-backup
  command: ["./bin/backup"]

# Health Check Job
- name: erlmcp-health
  command: ["./bin/health-check"]
```

### CronJobs

- Hourly health checks
- Daily backups
- Weekly metrics cleanup
- Log rotation

### Backup & Recovery

- Persistent volume snapshots
- ConfigMap and Secret backup
- Database backup and restore
- Disaster recovery procedures

## 📈 Performance Tuning

### Resource Management

- Request and limits for all pods
- Pod affinity for performance
- Resource quotas for namespace isolation
- Horizontal and vertical scaling

### JVM Tuning

```yaml
jvm:
  options: [
    "-Xms1g",
    "-Xmx4g",
    "-XX:+UseG1GC",
    "-XX:MaxGCPauseMillis=200"
  ]
```

### Connection Pooling

- Database connection pooling
- HTTP client connection reuse
- Keep-alive configurations
- Pipeline optimization

## 🐛 Troubleshooting

### Common Issues

1. **Pod not starting**: Check logs with `kubectl logs -f <pod-name>`
2. **Service not accessible**: Verify service type and load balancer configuration
3. **Connection refused**: Check network policies and port configurations
4. **Resource limits**: Monitor resource usage and adjust limits accordingly

### Debug Mode

```bash
# Enable debug logging
kubectl set env deployment/erlmcp ERLMCP_LOG_LEVEL=debug

# Get pod status
kubectl get pods -n erlmcp

# View service endpoints
kubectl get endpoints -n erlmcp
```

### Logs

```bash
# View container logs
kubectl logs -f deployment/erlmcp

# View all pods in erlmcp namespace
kubectl get pods -n erlmcp

# Describe a specific pod
kubectl describe pod erlmcp-0 -n erlmcp
```

## 📚 References

### Documentation

- [erlmcp Architecture Documentation](../../docs/architecture.md)
- [Erlang Clustering Guide](../../docs/CLUSTERING.md)
- [Transport Layer Documentation](../../docs/TRANSPORT.md)
- [Monitoring and Observability](../../docs/OBSERVABILITY.md)

### Kubernetes Resources

- [StatefulSet Documentation](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/)
- [Horizontal Pod Autoscaler](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/)
- [Network Policies](https://kubernetes.io/docs/concepts/services-networking/network-policies/)
- [Ingress Controllers](https://kubernetes.io/docs/concepts/services-networking/ingress/)

### Helm Charts

- [Helm Chart Documentation](https://helm.sh/docs/)
- [Template Guide](https://helm.sh/docs/howto/charts_tips_and_tricks/)
- [Best Practices](https://helm.sh/docs/intro/using_helm/#best-practices)

## 🤝 Support

For issues and questions:

1. Check the [troubleshooting section](#-troubleshooting)
2. Review the [Kubernetes documentation](#-references)
3. Open an issue in the [erlmcp-kubernetes repository](https://github.com/your-org/erlmcp-kubernetes)

---

## 📝 Version History

- **v3.0.0**: Initial release with complete Kubernetes deployment suite
- **v3.0.1**: Added blue/green and canary deployment support
- **v3.0.2**: Enhanced security and monitoring capabilities

---

© 2024 erlmcp Team. Licensed under the Apache License, Version 2.0.