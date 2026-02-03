# erlmcp v3.0.0 - Worldwide Production Deployment Guide

**Version:** 3.0.0
**Last Updated:** 2026-02-02
**Constitution:** DOCKER-ONLY CONSTITUTION - All execution MUST run via Docker

---

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Quick Start Deployment](#quick-start-deployment)
4. [Docker Deployment](#docker-deployment)
5. [Docker Swarm Deployment](#docker-swarm-deployment)
6. [Kubernetes Deployment](#kubernetes-deployment)
7. [Helm Chart Deployment](#helm-chart-deployment)
8. [Environment Configuration](#environment-configuration)
9. [Rollback Procedures](#rollback-procedures)
10. [Health Monitoring](#health-monitoring)
11. [Troubleshooting](#troubleshooting)
12. [Disaster Recovery](#disaster-recovery)

---

## Overview

erlmcp v3 is an enterprise-grade Erlang/OTP implementation of the Model Context Protocol (MCP) SDK. This guide covers worldwide production deployment across multiple platforms:

- **Docker** - Single-node deployment
- **Docker Swarm** - Multi-node cluster deployment
- **Kubernetes** - Cloud-native orchestration
- **Helm** - Package-based deployment

### Architecture Components

| Component | Description | Default Port |
|-----------|-------------|--------------|
| **erlmcp Core** | Main MCP server with registry and session management | 8080 |
| **Transports** | STDIO, TCP, HTTP, WebSocket, SSE transports | 8080-8083 |
| **Metrics** | Prometheus metrics exporter | 9100 |
| **Health** | Health check endpoints | 9090 |
| **Distribution** | EPMD-less Erlang distribution | 9100-9200 |

### Deployment Architecture

```
                    +-------------------+
                    |   Load Balancer   |
                    +---------+---------+
                              |
          +-------------------+-------------------+
          |                   |                   |
    +-----+-----+       +-----+-----+       +-----+-----+
    |  Region 1  |       |  Region 2  |       |  Region 3  |
    |   (US-E)   |       |   (US-W)   |       |   (EU-W)   |
    +-----+-----+       +-----+-----+       +-----+-----+
          |                   |                   |
    +-----+-----+       +-----+-----+       +-----+-----+
    |  K8s Cluster|     | Swarm Cluster|    | Docker Nodes|
    |  3+ replicas|     |  5+ replicas |    |  HA Pair    |
    +-----+-----+       +-----+-----+       +-----+-----+
```

---

## Prerequisites

### System Requirements

#### Minimum Requirements (Per Node)

| Resource | Minimum | Recommended | Production |
|----------|---------|-------------|------------|
| CPU | 2 cores | 4 cores | 8+ cores |
| RAM | 2 GB | 4 GB | 16+ GB |
| Storage | 20 GB | 50 GB | 100+ GB SSD |
| Network | 100 Mbps | 1 Gbps | 10+ Gbps |

#### Software Requirements

| Software | Version | Required |
|----------|---------|----------|
| Docker | 20.10+ | Yes |
| Docker Compose | 2.0+ | Yes (for compose deployment) |
| Kubernetes | 1.25+ | Yes (for K8s deployment) |
| Helm | 3.10+ | Yes (for Helm deployment) |
| Erlang/OTP | 28.3.1 | Built into image |

### Network Requirements

- **Open Ports:** 8080-8083 (transports), 9090 (health), 9100 (metrics), 9100-9200 (distribution)
- **EPMD-less Clustering:** Port 4369 is NOT exposed - uses fixed port range 9100-9200
- **Firewall Rules:** Allow intra-cluster communication on distribution ports

---

## Quick Start Deployment

### 1. Clone Repository

```bash
git clone https://github.com/seanchatmangpt/erlmcp.git
cd erlmcp
```

### 2. Build Docker Image

```bash
# Build with build metadata
docker build \
  --build-arg BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ') \
  --build-arg VCS_REF=$(git rev-parse --short HEAD) \
  --build-arg VERSION=3.0.0 \
  -t erlmcp:3.0.0 \
  -f Dockerfile \
  .
```

### 3. Run Container

```bash
docker run -d \
  --name erlmcp \
  -p 8080:8080 \
  -p 9100:9100 \
  -p 9090:9090 \
  -e ERLMCP_ENV=production \
  -e ERLANG_COOKIE=$(openssl rand -base64 48 | tr -d '\n') \
  erlmcp:3.0.0
```

### 4. Verify Deployment

```bash
# Health check
curl http://localhost:9090/health

# Metrics
curl http://localhost:9100/metrics

# Ping
docker exec erlmcp /opt/erlmcp/bin/erlmcp ping
```

---

## Docker Deployment

### Single Node Production Deployment

#### 1. Create Environment File

```bash
cp .env.prod.template .env.prod
# Edit .env.prod with your configuration
```

#### 2. Create Required Secrets

```bash
# Generate secure Erlang cookie
openssl rand -base64 48 | tr -d '\n' > .erlang_cookie
chmod 600 .erlang_cookie

# Generate JWT secret
openssl rand -hex 32 > .jwt_secret
chmod 600 .jwt_secret
```

#### 3. Deploy with Docker Compose

```bash
# Production deployment
docker compose --env-file .env.prod \
  -f docker-compose.yml \
  -f docker-compose.prod.yml \
  up -d

# Scale for high availability
docker compose --env-file .env.prod up -d --scale erlmcp=3
```

#### 4. Verify Service Health

```bash
# Check service status
docker compose ps

# View logs
docker compose logs -f erlmcp

# Health check
curl http://localhost:9090/health
```

### Docker Compose Quality Gates

The docker-compose.yml includes quality lane services for CI/CD:

```bash
# Compile gate
docker compose run --rm erlmcp-build make compile

# Unit test gate
docker compose run --rm erlmcp-unit make eunit

# Integration test gate
docker compose run --rm erlmcp-ct make ct

# Quality analysis gate
docker compose run --rm erlmcp-check make check

# Performance benchmark gate
docker compose run --rm erlmcp-bench make benchmark
```

---

## Docker Swarm Deployment

### Initialize Swarm Cluster

#### 1. Initialize Swarm (First Manager)

```bash
# On first manager node
docker swarm init --advertise-addr $(hostname -I | awk '{print $1}')

# Get join tokens
docker swarm join-token worker
docker swarm join-token manager
```

#### 2. Join Worker Nodes

```bash
# On worker nodes
docker swarm join --token WORKER_TOKEN MANAGER_IP:2377
```

#### 3. Deploy Stack

```bash
# Use deployment script
./docker-swarm/deploy-swarm.sh init
./docker-swarm/deploy-swarm.sh deploy
```

### Manual Swarm Deployment

#### 1. Create Overlay Network

```bash
docker network create \
  --driver overlay \
  --attachable \
  --subnet 172.28.0.0/16 \
  erlmcp-overlay
```

#### 2. Create Secrets

```bash
# Erlang cookie (required for clustering)
openssl rand -base64 48 | tr -d '\n' | \
  docker secret create erlmcp-erlang-cookie -

# Database password
echo "your-secure-db-password" | \
  docker secret create erlmcp-db-password -

# JWT secret
openssl rand -hex 32 | \
  docker secret create erlmcp-jwt-secret -

# TLS certificate
cat your-tls.crt | \
  docker secret create erlmcp-tls-cert -

# TLS private key
cat your-tls.key | \
  docker secret create erlmcp-tls-key -
```

#### 3. Deploy Stack

```bash
docker stack deploy \
  -c docker-compose.yml \
  -c docker-compose.swarm.yml \
  --resolve-image=always \
  erlmcp
```

#### 4. Verify Deployment

```bash
# Check stack services
docker stack services erlmcp

# Check service tasks
docker service ps erlmcp_erlmcp

# View logs
docker service logs -f erlmcp_erlmcp

# Scale service
docker service scale erlmcp_erlmcp=5
```

### Swarm Service Configuration

```yaml
# docker-compose.swarm.yml
version: '3.8'

services:
  erlmcp:
    image: erlmcp:3.0.0
    secrets:
      - erlmcp-erlang-cookie
      - erlmcp-db-password
      - erlmcp-jwt-secret
      - erlmcp-tls-cert
      - erlmcp-tls-key
    environment:
      ERLMCP_ENV: production
      ERLANG_COOKIE_FILE: /run/secrets/erlmcp-erlang-cookie
      ERL_AFLAGS: "-proto_dist inet_tls"
      ERL_DIST_PORT: "9100"
      ERLANG_DISTRIBUTION_PORT_RANGE: "9100-9200"
    networks:
      - erlmcp-overlay
    deploy:
      replicas: 5
      placement:
        max_replicas_per_node: 1
        preferences:
          - spread: node.labels.zone
      update_config:
        parallelism: 1
        delay: 30s
        failure_action: rollback
        order: start-first
      rollback_config:
        parallelism: 1
        delay: 10s
      restart_policy:
        condition: on-failure
        delay: 5s
        max_attempts: 3
        window: 120s
      resources:
        limits:
          cpus: '2.0'
          memory: 4G
        reservations:
          cpus: '0.5'
          memory: 1G
    healthcheck:
      test: ["CMD", "/opt/erlmcp/bin/healthcheck.sh"]
      interval: 15s
      timeout: 10s
      retries: 3
      start_period: 45s
```

---

## Kubernetes Deployment

### Namespace Creation

```bash
# Create namespace
kubectl create namespace erlmcp

# Set default namespace
kubectl config set-context --current --namespace=erlmcp
```

### Create Secrets

```bash
# Erlang cookie
kubectl create secret generic erlmcp-secrets \
  --from-literal=erlang-cookie=$(openssl rand -base64 48 | tr -d '\n')

# Database credentials
kubectl create secret generic erlmcp-db \
  --from-literal=db-host=postgres.prod.svc.cluster.local \
  --from-literal=db-name=erlmcp_prod \
  --from-literal=db-user=erlmcp \
  --from-literal=db-password=your-secure-password

# TLS certificate
kubectl create secret tls erlmcp-tls \
  --cert=tls.crt \
  --key=tls.key
```

### Deploy StatefulSet

```bash
# Apply manifests
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/rbac.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/statefulset.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/ingress.yaml
kubectl apply -f k8s/hpa.yaml
kubectl apply -f k8s/pdb.yaml
kubectl apply -f k8s/network-policy.yaml
kubectl apply -f k8s/monitoring.yaml
```

### Verify Deployment

```bash
# Check pods
kubectl get pods -l app=erlmcp

# Check StatefulSet
kubectl get statefulset erlmcp-cluster

# Check services
kubectl get svc

# Describe pod for troubleshooting
kubectl describe pod erlmcp-cluster-0

# View logs
kubectl logs -f erlmcp-cluster-0

# Exec into pod
kubectl exec -it erlmcp-cluster-0 -- /bin/sh
```

### Rolling Update

```bash
# Update image
kubectl set image statefulset/erlmcp-cluster \
  erlmcp=erlmcp:3.0.1 \
  --namespace=erlmcp

# Check rollout status
kubectl rollout status statefulset/erlmcp-cluster

# Rollback if needed
kubectl rollout undo statefulset/erlmcp-cluster
```

---

## Helm Chart Deployment

### Install Helm Chart

#### 1. Add Helm Repository (if applicable)

```bash
helm repo add erlmcp https://charts.erlmcp.io
helm repo update
```

#### 2. Create Values File

```bash
# Copy and customize values
cp helm/erlmcp/values.yaml helm/erlmcp/values-prod.yaml
# Edit values-prod.yaml
```

#### 3. Install Chart

```bash
helm install erlmcp ./helm/erlmcp \
  --namespace erlmcp \
  --create-namespace \
  --values helm/erlmcp/values-prod.yaml \
  --set image.tag=3.0.0 \
  --set erlmcp.erlang.cookie=$(openssl rand -base64 48 | tr -d '\n') \
  --timeout 10m
```

#### 4. Upgrade Chart

```bash
helm upgrade erlmcp ./helm/erlmcp \
  --namespace erlmcp \
  --values helm/erlmcp/values-prod.yaml \
  --set image.tag=3.0.1 \
  --reuse-values \
  --wait
```

### Helm Chart Values

```yaml
# values-prod.yaml
global:
  environment: production
  region: us-east-1

image:
  registry: ghcr.io
  repository: your-org/erlmcp
  tag: "3.0.0"
  pullPolicy: IfNotPresent

erlmcp:
  replicaCount: 5
  podManagementPolicy: OrderedReady
  upgradeType: RollingUpdate
  maxUnavailable: 0
  maxSurge: 1

  erlang:
    cookie: "change-me-to-secure-random-value"
    distribution: "inet_tls"
    distPort: 9100
    portRange: "9100-9200"

  resources:
    requests:
      cpu: 1000m
      memory: 2Gi
    limits:
      cpu: 4000m
      memory: 8Gi

  nodeSelector:
    node.kubernetes.io/instance-type: m5.2xlarge

  affinity:
    podAntiAffinity:
      preferredDuringSchedulingIgnoredDuringExecution:
        - weight: 100
          podAffinityTerm:
            labelSelector:
              matchExpressions:
                - key: app
                  operator: In
                  values:
                    - erlmcp
            topologyKey: topology.kubernetes.io/zone

  tolerations:
    - key: "workload"
      operator: "Equal"
      value: "erlmcp"
      effect: "NoSchedule"

logging:
  level: info
  format: json
  output: stdout

storage:
  sessions:
    size: 50Gi
    storageClass: fast-ssd
    accessMode: ReadWriteOnce
  logs:
    size: 20Gi
    storageClass: standard-ssd
    accessMode: ReadWriteOnce

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 20
  targetCPUUtilizationPercentage: 70
  targetMemoryUtilizationPercentage: 80

podDisruptionBudget:
  enabled: true
  minAvailable: 2
```

---

## Environment Configuration

### Required Environment Variables

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `ERLMCP_ENV` | Environment (production/staging/dev) | production | Yes |
| `ERLANG_COOKIE` | Erlang distribution cookie | - | Yes (cluster) |
| `ERL_AFLAGS` | Erlang VM flags | -proto_dist inet_tls | Yes |
| `ERL_DIST_PORT` | Distribution port | 9100 | Yes |
| `ERLMCP_NODE_NAME` | Node name | erlmcp@hostname | Yes |
| `ERLMCP_PORT` | HTTP API port | 8080 | Yes |
| `METRICS_PORT` | Prometheus metrics port | 9100 | Yes |
| `HEALTH_PORT` | Health check port | 9090 | Yes |

### Optional Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `ERLMCP_LOG_LEVEL` | Log level (debug/info/warn/error) | info |
| `ERL_MAX_PORTS` | Max Erlang ports | 65536 |
| `ERL_MAX_ETS_TABLES` | Max ETS tables | 50000 |
| `ERL_FULLSWEEP_AFTER` | GC fullsweep after | 0 |
| `ERLMCP_DB_HOST` | Database host | localhost |
| `ERLMCP_DB_PORT` | Database port | 5432 |
| `ERLMCP_DB_NAME` | Database name | erlmcp |
| `REDIS_HOST` | Redis host | localhost |
| `REDIS_PORT` | Redis port | 6379 |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | OTEL endpoint | - |

### Production Configuration Template

```bash
# .env.prod
ERLMCP_ENV=production
ERLMCP_VERSION=3.0.0
ERLMCP_IMAGE=erlmcp:3.0.0
ERLMCP_REPLICAS=5

# Distribution (REQUIRED)
ERLANG_COOKIE_FILE=/run/secrets/erlang.cookie
ERL_AFLAGS=-proto_dist inet_tls
ERL_DIST_PORT=9100
ERL_DIST_PORT_MIN=9100
ERL_DIST_PORT_MAX=9200
ERLMCP_NODE_NAME=erlmcp@${HOSTNAME}

# Ports
ERLMCP_PORT=8080
METRICS_PORT=9100
HEALTH_PORT=9090

# Logging
ERLMCP_LOG_LEVEL=warn

# Database
ERLMCP_DB_HOST=postgres.prod.svc.cluster.local
ERLMCP_DB_PORT=5432
ERLMCP_DB_NAME=erlmcp_prod
ERLMCP_DB_USER=erlmcp
ERLMCP_DB_PASSWORD_FILE=/run/secrets/db_password

# Redis
REDIS_HOST=redis.prod.svc.cluster.local
REDIS_PORT=6379
REDIS_PASSWORD_FILE=/run/secrets/redis_password

# OpenTelemetry
OTEL_ENABLED=true
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
OTEL_SERVICE_NAME=erlmcp
OTEL_SAMPLING_RATE=0.01

# Security
TLS_ENABLED=true
RATE_LIMIT_ENABLED=true
RATE_LIMIT_PER_SECOND=1000

# Resources
ERLMCP_CPU_LIMIT=2.0
ERLMCP_MEMORY_LIMIT=4G
ERLMCP_CPU_RESERVATION=0.5
ERLMCP_MEMORY_RESERVATION=1G
```

---

## Rollback Procedures

### Docker Rollback

```bash
# Stop current container
docker stop erlmcp

# Remove current container
docker rm erlmcp

# Start previous version
docker run -d \
  --name erlmcp \
  -p 8080:8080 \
  -p 9100:9100 \
  -e ERLMCP_ENV=production \
  erlmcp:3.0.0-previous

# Verify health
curl http://localhost:9090/health
```

### Docker Compose Rollback

```bash
# Revert to previous image tag
sed -i 's/erlmcp:3.0.1/erlmcp:3.0.0/g' docker-compose.yml

# Redeploy
docker compose down
docker compose up -d

# Verify
docker compose ps
docker compose logs -f
```

### Docker Swarm Rollback

```bash
# Rollback service to previous spec
docker service rollback erlmcp_erlmcp

# Verify rollback
docker service ps erlmcp_erlmcp

# Monitor logs during rollback
docker service logs -f erlmcp_erlmcp
```

### Kubernetes Rollback

```bash
# Check rollout history
kubectl rollout history statefulset/erlmcp-cluster

# Rollback to previous revision
kubectl rollout undo statefulset/erlmcp-cluster

# Rollback to specific revision
kubectl rollout undo statefulset/erlmcp-cluster --to-revision=2

# Verify rollback
kubectl rollout status statefulset/erlmcp-cluster

# Check pod status
kubectl get pods -l app=erlmcp -w
```

### Helm Rollback

```bash
# List releases
helm list -n erlmcp

# Check history
helm history erlmcp -n erlmcp

# Rollback to previous release
helm rollback erlmcp -n erlmcp

# Rollback to specific revision
helm rollback erlmcp 5 -n erlmcp

# Verify rollback
helm status erlmcp -n erlmcp
```

### Automated Rollback Script

```bash
#!/bin/bash
# scripts/rollback.sh

set -euo pipefail

DEPLOYMENT_TYPE=${1:-"docker"}
ENVIRONMENT=${2:-"production"}

echo "Initiating rollback for $DEPLOYMENT_TYPE in $ENVIRONMENT"

case $DEPLOYMENT_TYPE in
  docker)
    docker stop erlmcp
    docker rm erlmcp
    docker run -d --name erlmcp -p 8080:8080 erlmcp:3.0.0-previous
    ;;
  swarm)
    docker service rollback erlmcp_erlmcp
    ;;
  kubernetes)
    kubectl rollout undo statefulset/erlmcp-cluster -n erlmcp
    kubectl rollout status statefulset/erlmcp-cluster -n erlmcp
    ;;
  helm)
    helm rollback erlmcp -n erlmcp
    ;;
  *)
    echo "Unknown deployment type: $DEPLOYMENT_TYPE"
    exit 1
    ;;
esac

echo "Rollback completed. Verify health:"
echo "  curl http://localhost:9090/health"
```

---

## Health Monitoring

### Health Check Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Overall health status |
| `/ready` | GET | Readiness probe |
| `/metrics` | GET | Prometheus metrics |
| `/status` | GET | Detailed status |

### Health Check Script

```bash
#!/bin/bash
# scripts/health-check.sh

set -euo pipefail

ENDPOINT=${1:-"http://localhost:9090/health"}
TIMEOUT=${2:-5}

check_health() {
  local response=$(curl -sf --max-time "$TIMEOUT" "$ENDPOINT" || echo "failed")

  if [[ "$response" == *"healthy"* ]] || [[ "$response" == *"OK"* ]]; then
    echo "Health check passed"
    return 0
  else
    echo "Health check failed: $response"
    return 1
  fi
}

check_metrics() {
  local metrics_endpoint="${ENDPOINT/health/metrics}"
  local response=$(curl -sf --max-time "$TIMEOUT" "$metrics_endpoint" || echo "failed")

  if [[ "$response" == *"erlmcp_up"* ]]; then
    echo "Metrics endpoint accessible"
    return 0
  else
    echo "Metrics check failed"
    return 1
  fi
}

check_distribution() {
  # Check if Erlang distribution is working
  if docker exec erlmcp /opt/erlmcp/bin/erlmcp ping &>/dev/null; then
    echo "Distribution: Node responding"
    return 0
  else
    echo "Distribution: Node not responding"
    return 1
  fi
}

# Run checks
check_health
check_metrics
check_distribution

echo "All health checks passed"
```

### Prometheus Metrics

Key metrics to monitor:

```
# Service availability
erlmcp_up - Gauge indicating if service is up
erlmcp_ready - Gauge indicating if service is ready

# Request metrics
erlmcp_requests_total - Counter of total requests
erlmcp_request_duration_seconds - Histogram of request durations
erlmcp_request_errors_total - Counter of request errors

# Session metrics
erlmcp_sessions_active - Gauge of active sessions
erlmcp_sessions_created_total - Counter of sessions created
erlmcp_sessions_closed_total - Counter of sessions closed

# Transport metrics
erlmcp_transport_connections_total - Counter per transport type
erlmcp_transport_messages_sent_total - Counter of messages sent
erlmcp_transport_messages_received_total - Counter of messages received

# Erlang VM metrics
erlmcp_memory_total - Gauge of total memory used
erlmcp_memory_processes - Gauge of process memory
erlmcp_memory_ets - Gauge of ETS memory
erlmcp_process_count - Gauge of process count
erlmcp_reductions_total - Counter of reductions

# Cluster metrics
erlmcp_cluster_nodes - Gauge of connected nodes
erlmcp_cluster_sync_status - Gauge of sync status
```

### Alerting Rules

```yaml
# alerting_rules.yml
groups:
  - name: erlmcp_alerts
    interval: 30s
    rules:
      # Service down
      - alert: ErlmcpServiceDown
        expr: up{job="erlmcp"} == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "erlmcp service is down"
          description: "{{ $labels.instance }} is down"

      # High error rate
      - alert: ErlmcpHighErrorRate
        expr: rate(erlmcp_request_errors_total[5m]) > 0.01
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High error rate detected"
          description: "Error rate is {{ $value }} errors/sec"

      # High latency
      - alert: ErlmcpHighLatency
        expr: histogram_quantile(0.99, rate(erlmcp_request_duration_seconds_bucket[5m])) > 1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High request latency"
          description: "P99 latency is {{ $value }}s"

      # Memory pressure
      - alert: ErlmcpMemoryPressure
        expr: erlmcp_memory_total / erlmcp_memory_limit > 0.8
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High memory usage"
          description: "Memory usage is {{ $value }}%"

      # Cluster partition
      - alert: ErlmcpClusterPartition
        expr: erlmcp_cluster_nodes < on() erlmcp_cluster_nodes_expected
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Cluster partition detected"
          description: "Only {{ $value }} nodes connected"
```

---

## Troubleshooting

### Common Issues and Solutions

#### 1. Container Fails to Start

**Symptoms:** Container exits immediately or fails health checks

**Diagnosis:**
```bash
# Check container logs
docker logs erlmcp

# Check health check output
docker inspect erlmcp --format='{{json .State.Health}}' | jq

# Check if port is already in use
netstat -tuln | grep -E '8080|9090|9100'
```

**Solutions:**
- Verify environment variables are set correctly
- Check Erlang cookie matches across cluster nodes
- Ensure ports are not already in use
- Review memory limits and available resources

#### 2. Cluster Nodes Not Connecting

**Symptoms:** Nodes unable to form cluster

**Diagnosis:**
```bash
# Check node name
docker exec erlmcp env | grep ERLMCP_NODE_NAME

# Check Erlang cookie
docker exec erlmcp cat /var/run/secrets/erlang.cookie

# Test distribution connectivity
docker exec erlmep erl -name test@localhost -setcookie secret -remsh erlmcp@node1
```

**Solutions:**
- Ensure all nodes use the same Erlang cookie
- Verify ERL_DIST_PORT is accessible (firewall rules)
- Check node names follow `name@host` format
- Verify EPMD-less configuration: `-proto_dist inet_tls`

#### 3. High Memory Usage

**Symptoms:** Container memory usage grows continuously

**Diagnosis:**
```bash
# Check memory metrics
curl http://localhost:9100/metrics | grep erlmcp_memory

# Check Erlang memory breakdown
docker exec erlmcp erl -noshell -eval 'erlang:display(erlang:memory()), halt().'

# Check for memory leaks
docker exec erlmcp observer # if debug image
```

**Solutions:**
- Increase memory limits
- Check for ETS table leaks
- Review session cleanup logic
- Enable garbage collection tuning
- Restart service periodically during maintenance windows

#### 4. Database Connection Failures

**Symptoms:** Application unable to connect to database

**Diagnosis:**
```bash
# Check database connectivity
docker exec erlmcp nc -zv postgres 5432

# Check DNS resolution
docker exec erlmcp nslookup postgres

# Review database logs
kubectl logs -l app=postgres -n database
```

**Solutions:**
- Verify database credentials
- Check network policies allow traffic
- Ensure database is ready before starting erlmcp
- Review SSL/TLS configuration

#### 5. Performance Degradation

**Symptoms:** Slow response times, high latency

**Diagnosis:**
```bash
# Check request metrics
curl http://localhost:9100/metrics | grep erlmcp_request_duration

# Check process count
curl http://localhost:9100/metrics | grep erlmcp_process_count

# Check reduction rate (CPU usage indicator)
curl http://localhost:9100/metrics | grep erlmcp_reductions_total
```

**Solutions:**
- Increase replica count
- Enable horizontal pod autoscaling
- Optimize Erlang VM parameters
- Review resource limits
- Check for message queue buildup

### Diagnostic Commands

```bash
# System information
docker exec erlmcp /opt/erlmcp/bin/erlmcp versions

# Node status
docker exec erlmcp /opt/erlmcp/bin/erlmcp ping

# Process info
docker exec erlmcp /opt/erlmcp/bin/erlmcp getpid

# Full status
docker exec erlmcp /opt/erlmcp/bin/erlmcp status

# Remote shell (for debugging)
docker exec -it erlmcp /opt/erlmcp/bin/erlmcp remote_console

# Trace debugging
docker exec erlmcp erl -noshell -setcookie cookie -name debug@localhost \
  -remsh erlmcp@erlmcp -eval "erlang:display(process_info(whereis(http_server))), halt()."
```

### Log Analysis

```bash
# View recent logs
docker logs --tail 100 erlmcp

# Follow logs
docker logs -f erlmcp

# Search for errors
docker logs erlmcp 2>&1 | grep -i error

# Search for specific pattern
docker logs erlmcp 2>&1 | grep "connection refused"

# Kubernetes logs
kubectl logs -f erlmcp-cluster-0 --tail=100

# Multiple pods logs
kubectl logs -l app=erlmcp --tail=50 --all-containers=true
```

---

## Disaster Recovery

### Backup Strategy

#### 1. Configuration Backups

```bash
#!/bin/bash
# scripts/backup-config.sh

BACKUP_DIR="/backup/erlmcp/config"
DATE=$(date +%Y%m%d-%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR/$DATE"

# Backup environment files
cp .env.prod "$BACKUP_DIR/$DATE/"

# Backup Kubernetes configs
kubectl get configmap -n erlmcp -o yaml > "$BACKUP_DIR/$DATE/configmaps.yaml"
kubectl get secret -n erlmcp -o yaml > "$BACKUP_DIR/$DATE/secrets.yaml"

# Backup Helm values
helm get values erlmcp -n erlmcp > "$BACKUP_DIR/$DATE/helm-values.yaml"

# Retain last 30 days
find "$BACKUP_DIR" -type d -mtime +30 -exec rm -rf {} +
```

#### 2. Data Backups

```bash
#!/bin/bash
# scripts/backup-data.sh

# Backup persistent volumes
kubectl exec -n erlmcp erlmcp-cluster-0 -- \
  tar czf /tmp/erlmcp-data-backup.tar.gz /var/lib/erlmcp

# Copy backup to local
kubectl cp -n erlmcp erlmcp-cluster-0:/tmp/erlmcp-data-backup.tar.gz \
  /backup/erlmcp/data/erlmcp-data-$(date +%Y%m%d-%H%M%S).tar.gz

# Upload to cloud storage (optional)
# gsutil cp /backup/erlmcp/data/*.tar.gz gs://erlmcp-backups/
```

### Restore Procedures

#### 1. Restore Configuration

```bash
# Restore from backup
cp /backup/erlmcp/config/20250102-120000/.env.prod .env.prod

# Apply Kubernetes configs
kubectl apply -f /backup/erlmcp/config/20250102-120000/configmaps.yaml
kubectl apply -f /backup/erlmcp/config/20250102-120000/secrets.yaml
```

#### 2. Restore Data

```bash
# Copy backup to pod
kubectl cp -n erlmcp /backup/erlmcp/data/erlmcp-data-20250102-120000.tar.gz \
  erlmcp-cluster-0:/tmp/restore.tar.gz

# Extract backup
kubectl exec -n erlmcp erlmcp-cluster-0 -- \
  tar xzf /tmp/restore.tar.gz -C /

# Restart pod
kubectl delete pod -n erlmcp erlmcp-cluster-0
```

### Multi-Region Disaster Recovery

#### Primary Region Setup

```bash
# Deploy to primary region (us-east-1)
kubectl config use-context us-east-1
helm install erlmcp ./helm/erlmcp \
  --namespace erlmcp \
  --values helm/erlmcp/values-us-east.yaml
```

#### DR Region Setup

```bash
# Deploy to DR region (us-west-2)
kubectl config use-context us-west-2
helm install erlmcp ./helm/erlmcp \
  --namespace erlmcp \
  --values helm/erlmcp/values-us-west.yaml
```

#### Failover Procedure

```bash
#!/bin/bash
# scripts/failover.sh

PRIMARY_REGION="us-east-1"
DR_REGION="us-west-2"

echo "Initiating failover from $PRIMARY_REGION to $DR_REGION"

# Update DNS to point to DR region
# Example using Route53
aws route53 change-resource-record-sets \
  --hosted-zone-id ZEXAMPLE123 \
  --change-batch file://failover-change.json

# Verify DR region is serving traffic
kubectl config use-context $DR_REGION
kubectl get pods -n erlmcp

echo "Failover completed. Monitoring DR region..."
```

### Graceful Shutdown

```bash
#!/bin/bash
# scripts/graceful-shutdown.sh

# Kubernetes graceful shutdown
kubectl delete pod erlmcp-cluster-0 \
  --namespace=erlmcp \
  --grace-period=60

# Wait for pod termination
kubectl wait --for=delete pod/erlmcp-cluster-0 \
  --namespace=erlmcp \
  --timeout=120s

echo "Pod terminated gracefully"
```

---

## Appendix

### A. Port Reference

| Port | Protocol | Description | External |
|------|----------|-------------|----------|
| 8080 | TCP | HTTP API / TCP Transport | Yes |
| 8081 | TCP | HTTP Transport | Yes |
| 8082 | TCP | WebSocket Transport | Yes |
| 8083 | TCP | SSE Transport | Yes |
| 9090 | TCP | Health Checks | Yes |
| 9100 | TCP | Metrics / Distribution | Yes |
| 9100-9200 | TCP | Erlang Distribution | Yes |
| 4369 | TCP | EPMD (NOT USED) | No |

### B. File Locations

| Path | Description |
|------|-------------|
| `/opt/erlmcp` | Application root |
| `/opt/erlmcp/bin` | Executables |
| `/opt/erlmcp/etc` | Configuration |
| `/opt/erlmcp/releases` | Release files |
| `/opt/erlmcp/log` | Runtime logs |
| `/var/lib/erlmcp` | Persistent data |
| `/var/log/erlmcp` | Log files |
| `/var/run/erlmcp` | Runtime files |

### C. Useful Links

- **Documentation:** https://erlmcp.dev/docs
- **GitHub:** https://github.com/seanchatmangpt/erlmcp
- **Issues:** https://github.com/seanchatmangpt/erlmcp/issues
- **Docker Hub:** https://hub.docker.com/r/erlmcp/erlmcp

### D. Support

For deployment issues:
1. Check troubleshooting section above
2. Review logs: `docker logs erlmcp` or `kubectl logs -f deployment/erlmcp`
3. Search existing GitHub issues
4. Create new issue with logs and environment details

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-02
**Maintained By:** erlmcp Team
