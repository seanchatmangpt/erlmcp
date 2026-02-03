# erlmcp v3 Kubernetes Deployment Suite

Production-grade Kubernetes deployment configurations for erlmcp, featuring:

## 🏗️ Architecture Overview

- **StatefulSets** for Erlang cluster nodes with stable network identities
- **Horizontal Pod Autoscaling** based on CPU/memory and custom metrics
- **Pod Affinity/Anti-affinity** for high availability and fault tolerance
- **Service Mesh** integration with Istio for advanced networking
- **Ingress** with TLS termination and load balancing
- **Secret Management** using Kubernetes Vault integration
- **ConfigMaps** for environment configuration management
- **Liveness/Readiness Probes** for health monitoring
- **Resource Management** with requests and limits
- **Pod Disruption Budgets** for availability guarantees
- **Cluster Autoscaler** integration for node scaling
- **Multi-zone Deployment** strategies for fault tolerance
- **Canary/Blue-Green** deployment patterns

## 📁 Directory Structure

```
kubernetes/
├── helm/                    # Helm charts
│   ├── templates/          # K8s manifest templates
│   ├── Chart.yaml           # Chart metadata
│   └── values.yaml         # Default values
├── kustomize/
│   ├── base/               # Base configurations
│   ├── overlays/
│   │   ├── prod/           # Production overlay
│   │   └── staging/        # Staging overlay
│   └── kustomization.yaml  # Base kustomization
├── deployments/           # Standalone YAML files
├── scripts/               # Deployment scripts
└── docs/                 # Documentation
```

## 🚀 Features

### Enterprise-Grade Components
- Multi-replica StatefulSet with persistent data
- Automatic failover with pod disruption budgets
- TLS termination at ingress
- Monitoring with OpenTelemetry integration
- Logging with structured JSON
- Metrics aggregation and alerting
- Security policies (network policies, RBAC)

### High Availability
- Zone-aware pod scheduling
- Anti-affinity rules for spread across nodes
- Health checks with configurable timeouts
- Graceful shutdown and draining
- Circuit breakers for cascading failures

### Scalability
- Horizontal pod autoscaling
- Cluster autoscaler integration
- Connection pooling limits
- Resource quotas namespace management
- Request rate limiting

### Observability
- Prometheus metrics scraping
- OpenTelemetry tracing
- Structured JSON logging
- Grafana dashboards
- Jaeger distributed tracing
- Alertmanager integration

## 🛠️ Deployment Patterns

### Canary Deployment
- Progressive traffic shifting
- A/B testing capability
- Automated rollback on failure
- Metrics-based promotion

### Blue-Green Deployment
- Zero-downtime deployments
- Instant rollback capability
- Traffic switching via Ingress
- Dual environment maintenance

## 🔧 Prerequisites

- Kubernetes 1.24+
- Helm 3.8+
- Kustomize 4.5+
- Istio 1.15+ (optional, for service mesh)
- Cert-Manager (for TLS certificates)
- Cluster autoscaler configured

## 📖 Documentation

See `docs/` for detailed guides on:
- Installation and setup
- Configuration options
- Deployment strategies
- Monitoring and alerting
- Troubleshooting
- Performance tuning