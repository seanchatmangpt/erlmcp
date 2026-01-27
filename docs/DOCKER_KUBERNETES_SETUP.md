# Docker and Kubernetes Deployment Guide for erlmcp

Complete guide for containerizing and deploying erlmcp in production environments.

## Table of Contents

1. [Docker Setup](#docker-setup)
2. [Docker Compose](#docker-compose)
3. [Kubernetes Deployment](#kubernetes-deployment)
4. [Production Best Practices](#production-best-practices)
5. [Monitoring and Observability](#monitoring-and-observability)
6. [Security](#security)
7. [Troubleshooting](#troubleshooting)

---

## Docker Setup

### Building Docker Images

#### Production Image (Runtime only)
```bash
# Build production image (~140MB)
docker build -t erlmcp:0.7.0 .

# Build with build args
docker build \
  --build-arg BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ') \
  --build-arg VCS_REF=$(git rev-parse --short HEAD) \
  --build-arg VERSION=0.7.0 \
  -t erlmcp:0.7.0 \
  .

# Tag for registry
docker tag erlmcp:0.7.0 registry.example.com/erlmcp:0.7.0
docker push registry.example.com/erlmcp:0.7.0
```

#### Debug Image (with tools)
```bash
# Build debug image (~320MB, includes debugging tools)
docker build --target debug -t erlmcp:0.7.0-debug .

# Run debug container
docker run -it \
  -p 8080:8080 \
  -p 9090:9090 \
  -p 4369:4369 \
  erlmcp:0.7.0-debug
```

### Running Docker Containers

#### Basic Usage
```bash
# Run erlmcp in foreground mode
docker run -p 8080:8080 erlmcp:0.7.0

# Run with environment variables
docker run -d \
  -p 8080:8080 \
  -p 9090:9090 \
  -e ERLMCP_ENV=production \
  -e ERLANG_COOKIE=secret_cookie \
  -e OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317 \
  --name erlmcp \
  erlmcp:0.7.0

# Run with volume mounts
docker run -d \
  -p 8080:8080 \
  -v erlmcp-data:/var/lib/erlmcp \
  -v erlmcp-logs:/var/log/erlmcp \
  --name erlmcp \
  erlmcp:0.7.0

# Run with resource limits
docker run -d \
  -p 8080:8080 \
  --memory=2g \
  --cpus=2 \
  --name erlmcp \
  erlmcp:0.7.0
```

### Health Checks

Docker includes a built-in health check:
```bash
# Check container health
docker ps
# HEALTHCHECK output shows (healthy|unhealthy|starting)

# Manual health check
docker run --rm erlmcp:0.7.0 /opt/erlmcp/bin/erlmcp ping
```

### Image Inspection

```bash
# Get image details
docker inspect erlmcp:0.7.0

# Check image size
docker images erlmcp:0.7.0

# Check layers
docker history erlmcp:0.7.0

# Run security scan (Trivy)
trivy image erlmcp:0.7.0
```

---

## Docker Compose

### Quick Start

```bash
# Start full stack (erlmcp + postgres + redis + monitoring)
docker-compose up -d

# Start only erlmcp service
docker-compose up -d erlmcp

# Start with postgres
docker-compose --profile postgres up -d

# Start with monitoring (prometheus + grafana)
docker-compose --profile monitoring up -d
```

### Configuration

Create `.env` file for environment variables:
```bash
# erlmcp Configuration
ERLMCP_ENV=production
ERLANG_COOKIE=erlmcp_prod_cookie_change_me
TAIEA_PORT=8080
EPMD_PORT=4369
METRICS_PORT=9090

# Database Configuration
DB_PORT=5432
DB_NAME=erlmcp
DB_USER=erlmcp_user
DB_PASSWORD=secure_password_here

# Redis Configuration
REDIS_PORT=6379

# Monitoring
PROMETHEUS_PORT=9090
GRAFANA_PORT=3000
GRAFANA_PASSWORD=admin

# Performance Tuning
ERL_MAX_PORTS=65536
ERL_MAX_ETS_TABLES=20000
```

### Services

#### erlmcp (Main Service)
```yaml
# Port: 8080 (HTTP API), 9090 (Metrics), 4369 (EPMD)
# Health check: every 30s
# Restart: unless-stopped
# Volumes: logs, data persistence
```

#### postgres (Optional)
```bash
# Enable postgres service
docker-compose --profile postgres up -d

# Access database
docker-compose exec postgres psql -U erlmcp_user -d erlmcp
```

#### redis (Optional)
```bash
# Enable redis service
docker-compose --profile redis up -d

# Access redis
docker-compose exec redis redis-cli
```

#### Monitoring (Optional)
```bash
# Enable monitoring stack (prometheus + grafana)
docker-compose --profile monitoring up -d

# Access services:
# Prometheus: http://localhost:9090
# Grafana: http://localhost:3000 (admin/admin)
```

### Common Operations

```bash
# View logs
docker-compose logs -f erlmcp

# Stop all services
docker-compose down

# Remove all data (volumes)
docker-compose down -v

# Rebuild image
docker-compose build --no-cache erlmcp

# Scale service (not recommended for erlmcp due to distribution)
docker-compose up -d --scale erlmcp=3

# Export logs
docker-compose logs erlmcp > logs.txt
```

---

## Kubernetes Deployment

### Prerequisites

- Kubernetes cluster 1.20+
- kubectl configured
- Optional: Helm, Prometheus Operator, Cert-Manager

### Quick Start

#### 1. Create Namespace
```bash
kubectl create namespace erlmcp
kubectl label namespace erlmcp pod-security.kubernetes.io/enforce=restricted
```

#### 2. Create Secrets
```bash
# Create database secrets
kubectl create secret generic erlmcp-secrets \
  --from-literal=db-name=erlmcp \
  --from-literal=db-user=erlmcp_user \
  --from-literal=db-password=$(openssl rand -base64 32) \
  --from-literal=jwt-secret=$(openssl rand -base64 32) \
  --from-literal=erlang-cookie=$(openssl rand -base64 32) \
  -n erlmcp

# Create TLS certificate
kubectl create secret tls erlmcp-tls \
  --cert=path/to/cert.crt \
  --key=path/to/key.key \
  -n erlmcp
```

#### 3. Deploy
```bash
# Apply manifests in order
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/rbac.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/secret.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/hpa.yaml
kubectl apply -f k8s/ingress.yaml
kubectl apply -f k8s/network-policy.yaml
kubectl apply -f k8s/pdb.yaml
kubectl apply -f k8s/monitoring.yaml
```

Or use kubectl apply on directory:
```bash
kubectl apply -f k8s/ -n erlmcp
```

### Manifest Overview

| File | Purpose |
|------|---------|
| `namespace.yaml` | Kubernetes namespace definition |
| `deployment.yaml` | Deployment, service, PVC, and ConfigMap |
| `statefulset.yaml` | StatefulSet for distributed Erlang clustering |
| `service.yaml` | ClusterIP, Headless, LoadBalancer, NodePort services |
| `configmap.yaml` | Configuration files (sys.config, vm.args, prometheus.yml) |
| `secret.yaml.template` | Secrets template (DO NOT commit secrets) |
| `rbac.yaml` | Role-Based Access Control configuration |
| `network-policy.yaml` | Network policies (zero-trust security) |
| `hpa.yaml` | Horizontal Pod Autoscaler configuration |
| `pdb.yaml` | Pod Disruption Budgets for availability |
| `ingress.yaml` | Ingress routing rules |
| `backup-pvc.yaml` | Backup and restore configuration |
| `monitoring.yaml` | ServiceMonitor, PrometheusRule, Grafana dashboards |

### Using StatefulSet (Recommended)

StatefulSet is better for distributed Erlang applications:

```bash
# Deploy using StatefulSet instead of Deployment
kubectl apply -f k8s/statefulset.yaml -n erlmcp

# Check StatefulSet status
kubectl get statefulset -n erlmcp
kubectl describe statefulset erlmcp-cluster -n erlmcp

# View pod names (stable DNS names)
kubectl get pods -n erlmcp
# erlmcp-cluster-0, erlmcp-cluster-1, erlmcp-cluster-2

# Access logs
kubectl logs erlmcp-cluster-0 -n erlmcp
kubectl logs -f erlmcp-cluster-0 -n erlmcp

# Port-forward to pod
kubectl port-forward erlmcp-cluster-0 8080:8080 -n erlmcp
```

### Common Operations

#### Deployment

```bash
# Check deployment status
kubectl get deployment erlmcp-tcps -n erlmcp
kubectl describe deployment erlmcp-tcps -n erlmcp

# Roll out update
kubectl set image deployment/erlmcp-tcps \
  erlmcp=erlmcp:0.7.1 -n erlmcp

# Rollback
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp

# Check rollout status
kubectl rollout status deployment/erlmcp-tcps -n erlmcp
```

#### Pods

```bash
# Check pod status
kubectl get pods -n erlmcp
kubectl describe pod erlmcp-0 -n erlmcp

# View logs
kubectl logs erlmcp-0 -n erlmcp
kubectl logs -f erlmcp-0 -n erlmcp
kubectl logs --previous erlmcp-0 -n erlmcp

# Execute commands
kubectl exec -it erlmcp-0 -n erlmcp -- /bin/bash
kubectl exec erlmcp-0 -n erlmcp -- /opt/erlmcp/bin/erlmcp ping

# Port forwarding
kubectl port-forward erlmcp-0 8080:8080 -n erlmcp
kubectl port-forward erlmcp-0 9090:9090 -n erlmcp
```

#### Services

```bash
# Check services
kubectl get svc -n erlmcp
kubectl get svc erlmcp -n erlmcp -o wide

# Test service connectivity
kubectl exec erlmcp-0 -n erlmcp -- curl http://erlmcp:8080/health

# Port forward to service
kubectl port-forward svc/erlmcp 8080:8080 -n erlmcp
```

#### Scaling

```bash
# Manual scaling
kubectl scale deployment erlmcp-tcps --replicas=5 -n erlmcp

# Check HPA status
kubectl get hpa -n erlmcp
kubectl describe hpa erlmcp-hpa -n erlmcp

# View scaling history
kubectl get events -n erlmcp
```

#### Updates

```bash
# Rolling update image
kubectl set image deployment/erlmcp-tcps \
  erlmcp=erlmcp:0.7.1 \
  --record -n erlmcp

# Rolling update with max surge
kubectl patch deployment erlmcp-tcps -p \
  '{"spec":{"strategy":{"rollingUpdate":{"maxSurge":2}}}}' \
  -n erlmcp

# Blue-green deployment
kubectl create deployment erlmcp-blue --image=erlmcp:0.7.0 -n erlmcp
kubectl create deployment erlmcp-green --image=erlmcp:0.7.1 -n erlmcp
# Switch traffic by updating service selector
```

---

## Production Best Practices

### Resource Management

```yaml
# Recommended resource requests and limits
resources:
  requests:
    cpu: 500m      # Minimum CPU
    memory: 512Mi   # Minimum memory
  limits:
    cpu: 2000m     # Maximum CPU (4x request)
    memory: 2Gi    # Maximum memory (4x request)
```

### High Availability

1. **Replicas**: Minimum 3 pods across availability zones
2. **Pod Disruption Budgets**: Ensure minimum availability during updates
3. **Anti-affinity**: Spread pods across nodes
4. **Health Checks**: Liveness and readiness probes
5. **Timeouts**: Proper termination grace period (60s)

### Security

1. **Non-root user**: Runs as UID 1000
2. **Read-only filesystem**: Configure where possible
3. **Network policies**: Zero-trust ingress/egress rules
4. **RBAC**: Minimal permissions per service
5. **Secrets**: Use Sealed Secrets or external vault
6. **TLS**: All external communication encrypted
7. **Pod security**: Restricted security context

### Monitoring

1. **Prometheus metrics**: Exposed on port 9090
2. **Log aggregation**: Kubernetes logs to ELK/Loki
3. **Alerts**: PrometheusRules for critical metrics
4. **Health checks**: HTTP /health endpoint
5. **Distributed tracing**: OpenTelemetry integration

### Persistence

1. **Data volume**: 10Gi PVC for database
2. **Log volume**: emptyDir for logs (lifecycle scoped)
3. **Backup**: Daily automated backups (CronJob)
4. **Restore**: Documented restore procedure

---

## Monitoring and Observability

### Prometheus Metrics

Metrics available on port 9090/metrics:

```
erlmcp_http_requests_total{method, status, path}
erlmcp_http_duration_seconds{method, status, path}
erlmcp_db_connections_active
erlmcp_db_connection_errors_total
erlmcp_erlang_memory_bytes{type}
erlmcp_erlang_processes
erlmcp_message_queue_length
```

### ServiceMonitor (Prometheus Operator)

```bash
# Deploy ServiceMonitor
kubectl apply -f k8s/monitoring.yaml -n erlmcp

# Verify in Prometheus targets
kubectl port-forward svc/prometheus 9090:9090 -n monitoring
# Visit http://localhost:9090/targets
```

### PrometheusRule (Alerts)

```bash
# Check alert rules
kubectl get prometheusrule -n erlmcp
kubectl describe prometheusrule erlmcp-alerts -n erlmcp
```

### Grafana Dashboards

```bash
# Import dashboard from ConfigMap
# Grafana → Dashboards → Import → erlmcp-grafana-dashboard

# Or manually create using metrics above
```

### Logging

```bash
# View real-time logs
kubectl logs -f erlmcp-0 -n erlmcp

# View all container logs
kubectl logs -f -l app=erlmcp -n erlmcp --all-containers=true

# Export logs
kubectl logs erlmcp-0 -n erlmcp > erlmcp-logs.txt

# Structured logging (JSON)
kubectl logs erlmcp-0 -n erlmcp | jq .
```

---

## Security

### Network Policies

Applied in zero-trust model:
- Deny all ingress/egress by default
- Allow only necessary traffic
- Pod-to-pod communication allowed
- External access through Ingress only

```bash
# View network policies
kubectl get networkpolicy -n erlmcp
kubectl describe networkpolicy erlmcp-allow-ingress -n erlmcp
```

### RBAC

Service account has minimal permissions:
- Read pods, configmaps, secrets, services
- No create, update, delete permissions
- Monitoring-only access to cluster resources

```bash
# View service account
kubectl get serviceaccount -n erlmcp
kubectl describe sa erlmcp -n erlmcp

# View role bindings
kubectl get rolebinding -n erlmcp
kubectl describe rolebinding erlmcp-rolebinding -n erlmcp
```

### Secret Management

Best practices:
1. **Never commit secrets to Git**
2. **Use Sealed Secrets** or **Vault** for encryption
3. **Rotate secrets** regularly
4. **Use Secret rotation** with operator

```bash
# Using Sealed Secrets
kubectl apply -f https://github.com/bitnami-labs/sealed-secrets/releases/download/v0.18.0/controller.yaml -n kube-system

# Create sealed secret
echo -n "password" | kubectl create secret generic erlmcp-secrets \
  --dry-run=client \
  --from-literal=db-password=- \
  -n erlmcp -o yaml | \
  kubeseal -f - > k8s/secret-sealed.yaml

# Apply sealed secret
kubectl apply -f k8s/secret-sealed.yaml -n erlmcp
```

### TLS/HTTPS

```bash
# Using cert-manager
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.0/cert-manager.yaml

# Create ClusterIssuer
kubectl apply -f - << 'EOF'
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: ops@example.com
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
    - http01:
        ingress:
          class: nginx
EOF

# Ingress automatically gets TLS certificate
```

---

## Troubleshooting

### Common Issues

#### Pod won't start
```bash
# Check pod status
kubectl describe pod erlmcp-0 -n erlmcp

# Check events
kubectl get events -n erlmcp --sort-by='.lastTimestamp'

# Check logs
kubectl logs erlmcp-0 -n erlmcp
kubectl logs --previous erlmcp-0 -n erlmcp

# Common causes:
# - Image pull error: Check image name and registry credentials
# - CrashLoopBackOff: Check logs for startup errors
# - Pending: Check resource availability and node constraints
```

#### Health check failing
```bash
# Check health manually
kubectl exec erlmcp-0 -n erlmcp -- /opt/erlmcp/bin/erlmcp ping

# Check health endpoint
kubectl exec erlmcp-0 -n erlmcp -- curl -s http://localhost:8080/health

# Check probe configuration
kubectl get pod erlmcp-0 -n erlmcp -o yaml | grep -A 20 "livenessProbe"
```

#### High memory usage
```bash
# Check memory metrics
kubectl top pod erlmcp-0 -n erlmcp
kubectl top nodes

# Check Erlang process memory
kubectl exec erlmcp-0 -n erlmcp -- /opt/erlmcp/bin/erlmcp \
  eval 'erlang:memory().'

# Increase memory limit
kubectl patch deployment erlmcp-tcps -p \
  '{"spec":{"template":{"spec":{"containers":[{"name":"erlmcp","resources":{"limits":{"memory":"4Gi"}}}]}}}}' \
  -n erlmcp
```

#### Connection refused
```bash
# Check service
kubectl get svc erlmcp -n erlmcp

# Check endpoints
kubectl get endpoints erlmcp -n erlmcp

# Test connectivity
kubectl run -it --rm debug --image=busybox --restart=Never -n erlmcp -- \
  sh -c "wget -O- http://erlmcp:8080/health"
```

#### Network policy blocking traffic
```bash
# Check network policies
kubectl get networkpolicy -n erlmcp

# Temporarily disable (for testing)
kubectl delete networkpolicy --all -n erlmcp

# Check logs for network errors
kubectl logs erlmcp-0 -n erlmcp | grep -i network
```

### Debugging Commands

```bash
# Interactive shell in container
kubectl exec -it erlmcp-0 -n erlmcp -- /bin/bash

# Get debug logs
kubectl logs erlmcp-0 -n erlmcp | grep -i error

# Port forwarding for tools
kubectl port-forward erlmcp-0 9100:9100 -n erlmcp &
# Connect to remote shell: erl -name debug@localhost -remsh erlmcp@erlmcp-0.erlmcp-headless.erlmcp.svc.cluster.local

# Resource usage
kubectl describe node <node-name>
kubectl top pod erlmcp-0 -n erlmcp --containers

# Kubernetes events
kubectl describe events -n erlmcp

# Check controller status
kubectl get controllerrevisions -n erlmcp
```

### Performance Tuning

```bash
# Adjust resource limits
kubectl patch deployment erlmcp-tcps -p \
  '{"spec":{"template":{"spec":{"containers":[{"name":"erlmcp","resources":{"requests":{"cpu":"1000m","memory":"1Gi"},"limits":{"cpu":"4000m","memory":"4Gi"}}}]}}}}' \
  -n erlmcp

# Enable HPA debug
kubectl get hpa erlmcp-hpa -n erlmcp -w

# Check pod scheduling
kubectl get pods erlmcp-0 -n erlmcp -o yaml | grep -A 10 affinity
```

---

## Production Checklist

Before deploying to production:

- [ ] Update image version in manifests
- [ ] Review and update resource limits
- [ ] Configure database credentials
- [ ] Set up TLS certificates
- [ ] Configure DNS/Ingress hostname
- [ ] Enable monitoring and alerting
- [ ] Set up log aggregation
- [ ] Configure backup strategy
- [ ] Test disaster recovery
- [ ] Review security policies
- [ ] Load test deployment
- [ ] Document runbooks
- [ ] Set up on-call rotation
- [ ] Configure incident response

---

## References

- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [erlang/OTP Documentation](https://www.erlang.org/doc/)
- [Prometheus Monitoring](https://prometheus.io/)
- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Container Security](https://kubernetes.io/docs/concepts/security/)

