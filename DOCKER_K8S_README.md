# erlmcp Docker and Kubernetes Deployment

Complete production-ready Docker and Kubernetes setup for erlmcp Erlang/OTP MCP Server.

## Overview

This repository includes:

1. **Multi-stage Dockerfile** - Optimized for production (140MB runtime image)
2. **Docker Compose** - Full stack with optional services (postgres, redis, monitoring)
3. **Kubernetes Manifests** - Production-grade K8s deployment
4. **Security Configuration** - RBAC, network policies, secrets management
5. **Monitoring** - Prometheus, Grafana, OpenTelemetry integration
6. **Validation Scripts** - Automated testing for Docker and Kubernetes

## Quick Start

### Docker

```bash
# Build image
docker build -t erlmcp:0.7.0 .

# Run container
docker run -d -p 8080:8080 erlmcp:0.7.0

# Check health
curl http://localhost:8080/health
```

### Docker Compose

```bash
# Start full stack
docker-compose up -d

# View logs
docker-compose logs -f erlmcp

# Stop services
docker-compose down
```

### Kubernetes

```bash
# Create namespace
kubectl create namespace erlmcp

# Create secrets
kubectl create secret generic erlmcp-secrets \
  --from-literal=db-password=$(openssl rand -base64 32) \
  -n erlmcp

# Deploy
kubectl apply -f k8s/ -n erlmcp

# Check status
kubectl get deployment erlmcp-tcps -n erlmcp
```

## File Structure

```
.
├── Dockerfile                          # Multi-stage production image
├── Dockerfile.dev                      # Development image
├── docker-compose.yml                  # Full stack orchestration
├── .dockerignore                       # Ignore list for Docker builds
├── docker/                             # Additional Docker files
│   ├── Dockerfile.production           # Alternative production build
│   ├── docker-compose.yml              # Additional compose configs
│   ├── prometheus.yml                  # Prometheus configuration
│   ├── otel-collector-config.yaml      # OpenTelemetry collector config
│   └── grafana-datasources.yml         # Grafana datasources
├── k8s/                                # Kubernetes manifests
│   ├── namespace.yaml                  # Namespace definition
│   ├── deployment.yaml                 # Deployment + Service + ConfigMap
│   ├── statefulset.yaml                # StatefulSet for clustering
│   ├── service.yaml                    # ClusterIP, Headless, LoadBalancer
│   ├── configmap.yaml                  # Configuration files
│   ├── secret.yaml.template            # Secret template (DO NOT COMMIT)
│   ├── rbac.yaml                       # Role-Based Access Control
│   ├── network-policy.yaml             # Network policies (zero-trust)
│   ├── hpa.yaml                        # Horizontal Pod Autoscaler
│   ├── pdb.yaml                        # Pod Disruption Budgets
│   ├── ingress.yaml                    # Ingress routing
│   ├── backup-pvc.yaml                 # Backup/restore jobs
│   └── monitoring.yaml                 # ServiceMonitor + PrometheusRule
├── scripts/
│   ├── docker-validation.sh            # Docker image validation
│   └── k8s-validation.sh               # Kubernetes deployment validation
├── docs/
│   ├── DOCKER_KUBERNETES_SETUP.md      # Complete setup guide
│   └── DEPLOYMENT_CHECKLIST.md         # Production checklist
└── DOCKER_K8S_README.md                # This file
```

## Features

### Docker

- ✅ Multi-stage build (builder + runtime + debug)
- ✅ <150MB runtime image (Alpine-based)
- ✅ Non-root user (UID 1000)
- ✅ Health checks included
- ✅ Proper signal handling (SIGTERM)
- ✅ OTEL observability ready
- ✅ Security hardened

### Docker Compose

- ✅ erlmcp service
- ✅ PostgreSQL (optional, profile: postgres)
- ✅ Redis (optional, profile: redis)
- ✅ Prometheus (optional, profile: monitoring)
- ✅ Grafana (optional, profile: monitoring)
- ✅ Health checks for all services
- ✅ Log rotation configured
- ✅ Volume persistence

### Kubernetes

- ✅ Deployment + StatefulSet options
- ✅ Rolling updates with zero downtime
- ✅ Service discovery (ClusterIP, Headless, LoadBalancer, NodePort)
- ✅ Ingress with TLS support
- ✅ ConfigMap for configuration
- ✅ Secrets for credentials (with encryption options)
- ✅ RBAC (least privilege)
- ✅ Network Policies (zero-trust security)
- ✅ Resource limits and requests
- ✅ Liveness, readiness, startup probes
- ✅ HPA (Horizontal Pod Autoscaler)
- ✅ PDB (Pod Disruption Budgets)
- ✅ Anti-affinity for high availability
- ✅ Persistent storage (PVC)
- ✅ Automated backup (CronJob)

### Monitoring & Observability

- ✅ Prometheus metrics endpoint
- ✅ ServiceMonitor for Prometheus Operator
- ✅ PrometheusRule for alerting
- ✅ Grafana dashboard templates
- ✅ OpenTelemetry integration
- ✅ Structured logging
- ✅ Distributed tracing ready

### Security

- ✅ Non-root container user
- ✅ Read-only filesystem (where applicable)
- ✅ Network policies (deny all by default)
- ✅ RBAC with minimal permissions
- ✅ TLS/HTTPS enforcement
- ✅ Secret management best practices
- ✅ Security scanning compatible (Trivy)
- ✅ Pod security standards

## Deployment Guides

### [DOCKER_KUBERNETES_SETUP.md](./docs/DOCKER_KUBERNETES_SETUP.md)

Complete guide covering:
- Docker image building and running
- Docker Compose configuration and usage
- Kubernetes deployment and operations
- Production best practices
- Monitoring and observability
- Security configuration
- Troubleshooting

### [DEPLOYMENT_CHECKLIST.md](./docs/DEPLOYMENT_CHECKLIST.md)

Production deployment checklist:
- Pre-deployment verification
- Docker image preparation
- Kubernetes configuration
- Cluster preparation
- Testing and validation
- Monitoring and alerting
- Backup and disaster recovery
- Go/no-go decision

## Validation Scripts

### Docker Validation

```bash
./scripts/docker-validation.sh [image_tag]

# Tests:
# - Image build
# - Image size
# - Security (non-root user, healthcheck)
# - Container startup
# - Health endpoint
# - Port accessibility
# - Volume mounting
# - Security scanning (Trivy)
```

### Kubernetes Validation

```bash
./scripts/k8s-validation.sh [namespace]

# Tests:
# - Prerequisites (kubectl, cluster, namespace)
# - Manifest syntax validation
# - RBAC configuration
# - Network policies
# - ConfigMaps and Secrets
# - Deployment status
# - Pod health
# - Service connectivity
# - Ingress configuration
# - Monitoring setup
```

## Configuration

### Environment Variables

**Docker/Compose**:
```bash
ERLMCP_ENV=production
ERLANG_COOKIE=erlmcp_prod_cookie
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
ERL_MAX_PORTS=65536
ERL_MAX_ETS_TABLES=20000
```

**Kubernetes** (in deployment.yaml):
```yaml
env:
  - name: ERLMCP_ENV
    value: "production"
  - name: ERLMCP_DB_HOST
    value: "postgres"
  - name: OTEL_EXPORTER_OTLP_ENDPOINT
    value: "http://otel-collector:4317"
```

### ConfigMap

Database configuration, logging, performance tuning in:
- `k8s/configmap.yaml` - sys.config, vm.args, prometheus.yml

### Secrets

Sensitive data stored in Kubernetes Secrets:
- Database credentials
- JWT secret
- TLS certificates
- OAuth2 tokens (if used)

**Important**: Use Sealed Secrets or Vault for encryption in transit and at rest.

## Best Practices

### Image Size
- Production image: ~140MB (without build tools)
- Debug image: ~320MB (includes debugging tools)
- Base: Alpine 3.20 (minimal footprint)

### Security
- Non-root user (UID 1000)
- Read-only root filesystem (where possible)
- No hardcoded secrets
- Security scanning (Trivy compatible)
- Network policies (zero-trust model)

### High Availability
- Minimum 3 replicas across availability zones
- Pod Disruption Budgets ensure availability during updates
- Anti-affinity spreads pods across nodes
- Health checks (liveness, readiness, startup)
- Graceful shutdown (60s termination period)

### Observability
- Prometheus metrics on port 9090
- OpenTelemetry tracing
- Structured logging
- Kubernetes events
- Health check endpoints

### Persistence
- PostgreSQL for data (optional)
- Redis for caching (optional)
- PersistentVolume for stateful data
- Automated backups (daily CronJob)
- Tested restore procedure

## Common Operations

### Docker

```bash
# Build
docker build -t erlmcp:0.7.0 .

# Run
docker run -d -p 8080:8080 erlmcp:0.7.0

# Logs
docker logs -f erlmcp

# Shell
docker exec -it erlmcp /bin/bash

# Health check
docker inspect erlmcp --format='{{.State.Health.Status}}'
```

### Kubernetes

```bash
# Deploy
kubectl apply -f k8s/ -n erlmcp

# Check status
kubectl get deployment erlmcp-tcps -n erlmcp

# View logs
kubectl logs -f erlmcp-0 -n erlmcp

# Shell access
kubectl exec -it erlmcp-0 -n erlmcp -- /bin/bash

# Port forward
kubectl port-forward svc/erlmcp 8080:8080 -n erlmcp

# Scaling
kubectl scale deployment erlmcp-tcps --replicas=5 -n erlmcp

# Rolling update
kubectl set image deployment/erlmcp-tcps erlmcp=erlmcp:0.7.1 -n erlmcp

# Rollback
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp
```

## Troubleshooting

### Pod won't start
```bash
kubectl describe pod erlmcp-0 -n erlmcp
kubectl logs erlmcp-0 -n erlmcp
kubectl get events -n erlmcp --sort-by='.lastTimestamp'
```

### Health check failing
```bash
kubectl exec erlmcp-0 -n erlmcp -- /opt/erlmcp/bin/erlmcp ping
kubectl exec erlmcp-0 -n erlmcp -- curl -s http://localhost:8080/health
```

### High memory usage
```bash
kubectl top pod erlmcp-0 -n erlmcp
kubectl patch deployment erlmcp-tcps -p \
  '{"spec":{"template":{"spec":{"containers":[{"name":"erlmcp","resources":{"limits":{"memory":"4Gi"}}}]}}}}'
```

See [DOCKER_KUBERNETES_SETUP.md - Troubleshooting](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting) for more.

## Production Checklist

Before deploying to production, verify:
- ✅ Docker validation script passes
- ✅ Kubernetes validation script passes
- ✅ All unit tests passing
- ✅ Code coverage >= 80%
- ✅ Security scan clean (Trivy)
- ✅ Database migration tested
- ✅ Backup/restore tested
- ✅ Load test completed
- ✅ Team training complete
- ✅ Runbooks documented

See [DEPLOYMENT_CHECKLIST.md](./docs/DEPLOYMENT_CHECKLIST.md) for complete checklist.

## Resources

### Documentation
- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Documentation](https://docs.docker.com/compose/)
- [Kubernetes Documentation](https://kubernetes.io/docs/)

### Tools
- [Docker](https://www.docker.com/)
- [Docker Compose](https://docs.docker.com/compose/)
- [kubectl](https://kubernetes.io/docs/reference/kubectl/)
- [Trivy](https://github.com/aquasecurity/trivy) - Security scanning
- [Prometheus](https://prometheus.io/) - Metrics and monitoring
- [Grafana](https://grafana.com/) - Visualization

### Standards
- [OCI Image Spec](https://github.com/opencontainers/image-spec)
- [Container Security Best Practices](https://kubernetes.io/docs/concepts/security/)
- [Kubernetes Best Practices](https://kubernetes.io/docs/concepts/configuration/overview/)

## Version Information

- **erlmcp Version**: 0.7.0
- **Erlang/OTP**: 27+ (Alpine 3.20)
- **Kubernetes**: 1.20+
- **Docker**: 20.10+

## License

This deployment configuration is provided under the same license as erlmcp.

## Support

For issues or questions:
1. Check the [troubleshooting section](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting)
2. Review the [deployment checklist](./docs/DEPLOYMENT_CHECKLIST.md)
3. Run validation scripts
4. Check logs and events
5. Contact support team

---

**Last Updated**: 2026-01-27
**Maintained By**: erlmcp contributors
