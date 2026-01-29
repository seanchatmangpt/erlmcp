# Deployment Guide

**Estimated read time: 15 minutes**

This document describes how to deploy erlmcp to production, with emphasis on GCP Cloud Run and Kubernetes.

## Pre-Deployment Checklist

```bash
# 1. Verify code quality
make workspace-check
# Expected: All targets pass, 0 errors

# 2. Verify tests pass
make workspace-test
# Expected: All tests green, <90s

# 3. Build production release
make workspace-release
# Expected: Both erlmcp and taiea releases created

# 4. Verify release integrity
ls -lh _build/prod/rel/erlmcp/
ls -lh taiea/_build/prod/rel/taiea/
# Expected: All directories present
```

## Deployment Models

### Model 1: GCP Cloud Run (Recommended for Startups)

#### Advantages
- Serverless (no infrastructure management)
- Auto-scaling (0 to thousands)
- Pay only for what you use
- HTTP/2 built-in

#### Setup Steps

**1. Create Docker Image**

```dockerfile
FROM erlang:25-slim

WORKDIR /app

# Copy releases
COPY _build/prod/rel/erlmcp /app/erlmcp
COPY taiea/_build/prod/rel/taiea /app/taiea
COPY config/sys.config /app/sys.config

# Health check
HEALTHCHECK --interval=10s --timeout=5s \
    CMD /app/erlmcp/bin/erlmcp pid || exit 1

# Expose port
EXPOSE 8080

# Start app
CMD ["/app/erlmcp/bin/erlmcp", "foreground"]
```

**2. Build and Push Image**

```bash
# Configure gcloud
gcloud auth login
gcloud config set project YOUR_PROJECT_ID

# Build image
docker build -t gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0 .

# Push to Container Registry
docker push gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0

# Or use Cloud Build
gcloud builds submit --tag gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0
```

**3. Deploy to Cloud Run**

```bash
gcloud run deploy erlmcp \
    --image gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0 \
    --platform managed \
    --region us-central1 \
    --port 8080 \
    --memory 1Gi \
    --cpu 1 \
    --timeout 3600 \
    --max-instances 100 \
    --allow-unauthenticated
```

**4. Verify Deployment**

```bash
# Get service URL
gcloud run services describe erlmcp \
    --platform managed \
    --region us-central1

# Test health endpoint
curl https://erlmcp-xxxx.run.app/health
```

### Model 2: GKE (Kubernetes Engine)

#### Advantages
- Full Kubernetes ecosystem
- Multi-zone deployment
- Custom configuration
- Enterprise features

#### Setup Steps

**1. Create GKE Cluster**

```bash
gcloud container clusters create erlmcp-cluster \
    --zone us-central1-a \
    --num-nodes 3 \
    --machine-type n1-standard-2 \
    --enable-autoscaling \
    --min-nodes 3 \
    --max-nodes 10 \
    --enable-ip-alias \
    --network erlmcp-network
```

**2. Create Kubernetes Manifest**

```yaml
# erlmcp-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
  labels:
    app: erlmcp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
    spec:
      containers:
      - name: erlmcp
        image: gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 5005
          name: erlang-dist
        resources:
          requests:
            cpu: "500m"
            memory: "512Mi"
          limits:
            cpu: "1000m"
            memory: "1Gi"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        env:
        - name: ERLANG_COOKIE
          valueFrom:
            secretKeyRef:
              name: erlmcp-secret
              key: erlang-cookie
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName

---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-service
spec:
  selector:
    app: erlmcp
  ports:
  - protocol: TCP
    port: 80
    targetPort: 8080
    name: http
  - protocol: TCP
    port: 5005
    targetPort: 5005
    name: erlang-dist
  type: LoadBalancer

---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: erlmcp-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

**3. Deploy to GKE**

```bash
# Get cluster credentials
gcloud container clusters get-credentials erlmcp-cluster \
    --zone us-central1-a

# Create secret
kubectl create secret generic erlmcp-secret \
    --from-literal=erlang-cookie=your-secret-cookie

# Deploy
kubectl apply -f erlmcp-deployment.yaml

# Verify deployment
kubectl get deployments
kubectl get pods
kubectl get services
```

**4. Monitor Deployment**

```bash
# Watch rollout
kubectl rollout status deployment/erlmcp

# View logs
kubectl logs -f deployment/erlmcp

# Get service endpoint
kubectl get service erlmcp-service
```

### Model 3: Docker Compose (Development/Staging)

#### Setup

```yaml
# docker-compose.yml
version: '3.8'

services:
  erlmcp:
    build:
      context: .
      dockerfile: Dockerfile
    ports:
      - "8080:8080"
      - "5005:5005"
    environment:
      ERLANG_COOKIE: dev_cookie
      LOG_LEVEL: info
    volumes:
      - ./config/sys.config:/app/sys.config:ro
      - ./logs:/var/log/erlmcp
    healthcheck:
      test: ["CMD", "/app/erlmcp/bin/erlmcp", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5
    restart: unless-stopped

  taiea:
    build:
      context: ./taiea
      dockerfile: Dockerfile
    ports:
      - "8081:8080"
    environment:
      ERLANG_COOKIE: dev_cookie
    depends_on:
      erlmcp:
        condition: service_healthy
    restart: unless-stopped
```

**Run:**
```bash
docker-compose up -d
docker-compose logs -f
docker-compose down
```

## Configuration for Production

### Environment Variables

```bash
# Required
export ERLANG_COOKIE=production_secret_cookie
export NODE_NAME=erlmcp@node1.example.com

# VM Resource Limits (CRITICAL for high-concurrency)
# +Q 65536: VM port limit (internal, must match ERL_MAX_PORTS)
# +P 262144: Maximum processes (connections, workers, supervisors)
# ERL_MAX_PORTS 65536: Maximum ports (TCP sockets, files, external programs)
# Note: Both +Q flag and ERL_MAX_PORTS must be set to same value
export ERLANG_FLAGS="+Q 65536 +P 262144 +K true +A 8"
export ERL_MAX_PORTS=65536

# Optional
export LOG_LEVEL=warning
export OTEL_EXPORTER_OTLP_ENDPOINT=http://collector:4318
export DATADOG_AGENT_HOST=datadog-agent
```

### VM Resource Limits

Erlang VM has hard limits on concurrent resources. For production deployments:

| Resource | Default | Production | Notes |
|----------|---------|------------|-------|
| **Ports** (`+Q`) | 24575 | **65536** | TCP sockets, files, external ports (2.67x increase) |
| **Processes** (`+P`) | 262,144 | **262,144** | Connections, workers, supervisors |
| **ETS Tables** | 1400 | **50000** | In-memory storage tables |

**Connection Capacity:**
- **Before** (default 24575): ~12K concurrent connections
- **After** (+Q 65536): ~32K concurrent connections (with headroom)
- **Improvement**: 2.67x increase in connection capacity
- **Realistic allocation** (40% for TCP, 20% files, 20% external ports, 20% headroom):
  - TCP connections: ~26K
  - Files: ~13K
  - External ports: ~13K
  - Headroom: ~13K |

**To verify limits:**
```bash
# In Erlang shell
erlang:system_info(port_limit).        % Should return 65536
erlang:system_info(process_limit).     % Should return 262144
erlang:system_info(ets_limit).         % Should return 50000
```

**To monitor usage:**
```bash
# Check current usage
erlang:system_info(port_count()).      % Current ports in use
erlang:system_info(process_count()).   % Current processes in use
erlang:system_info(ets_count()).       % Current ETS tables
```

**Symptoms of hitting limits:**
- Port limit exceeded: `emfile` errors, "too many open files"
- Process limit exceeded: `system_limit` errors, spawn failures
- ETS limit exceeded: `system_limit` errors when creating tables

### System Configuration (sys.config)

```erlang
[
    % erlmcp
    {erlmcp, [
        {listen_port, 8080},
        {transport, {tcp, [{port, 5005}]}},
        {workers, 20},
        {connection_timeout, 30000},
        {max_connections, 50000}  % Increased: default 10K → 50K (with +Q 65536)
    ]},

    % TAIEA Governor
    {taiea, [
        {governor_enabled, true},
        {max_concurrent_calls, 1000},
        {rate_limit_per_second, 10000}
    ]},

    % Lager logging
    {lager, [
        {log_root, "/var/log/erlmcp"},
        {handlers, [
            {lager_console_backend, [
                {level, warning},
                {formatter_config, [
                    {syslog, [
                        {facility, local0},
                        {level, warning}
                    ]}
                ]}
            ]},
            {lager_file_backend, [
                {file, "/var/log/erlmcp/erlmcp.log"},
                {level, info},
                {size, 10485760},
                {date, "$D0"},
                {count, 7}
            ]}
        ]},
        {crash_log, "/var/log/erlmcp/crash.log"}
    ]},

    % Kernel
    {kernel, [
        {logger_level, warning}
    ]}
].
```

## Release Management

### Version Tagging

```bash
# Tag release
git tag -a v0.6.0 -m "Release v0.6.0"
git push origin v0.6.0

# Build release for tag
git checkout v0.6.0
make workspace-release
docker build -t erlmcp:v0.6.0 .
```

### Zero-Downtime Deployment

**Rolling Update (Kubernetes):**
```bash
# Update image
kubectl set image deployment/erlmcp \
    erlmcp=gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.1 \
    --record

# Monitor rollout
kubectl rollout status deployment/erlmcp

# Rollback if needed
kubectl rollout undo deployment/erlmcp
```

**Blue-Green (Docker Compose):**
```bash
# Deploy new version as "green"
docker run --name erlmcp-green \
    -p 8080:8080 \
    erlmcp:v0.6.1

# Test green
curl http://localhost:8080/health

# Switch traffic (update load balancer)
# Stop blue
docker stop erlmcp
docker rename erlmcp-green erlmcp
```

### Canary Deployment

```bash
# Deploy 10% traffic to new version
kubectl patch service erlmcp-service \
    -p '{"spec":{"traffic":[{"revision":"erlmcp-v0.6.1","percent":10},{"revision":"erlmcp-v0.6.0","percent":90}]}}'

# Monitor metrics for 30 minutes
# If good, increase to 50%
# If good, increase to 100%
```

## Health Checks & Monitoring

### Health Endpoints

Implement health checks in your application:

```erlang
% HTTP health endpoint
-module(erlmcp_health).

health_check() ->
    {
        <<"status">>,
        <<"up">>,
        <<"erlmcp_ready">>,
        erlmcp_server:is_ready(),
        <<"taiea_ready">>,
        taiea:is_ready()
    }.

ready_check() ->
    erlmcp_server:is_ready() andalso taiea:is_ready().
```

Configure in sys.config:
```erlang
{erlmcp, [
    {health_endpoint, "/health"},
    {ready_endpoint, "/ready"}
]}.
```

### Monitoring Metrics

Key metrics to track:

```
Application Metrics:
  - erlmcp_requests_total (counter)
  - erlmcp_request_duration_seconds (histogram)
  - erlmcp_errors_total (counter)
  - erlmcp_active_connections (gauge)

System Metrics:
  - erlang_vm_memory_bytes (gauge)
  - erlang_vm_processes_count (gauge)
  - erlang_vm_statistics_run_queue_length (gauge)

Business Metrics:
  - tools_called_total (counter)
  - resources_accessed_total (counter)
  - subscriptions_active (gauge)
```

### Logging Strategy

Structured logging:
```erlang
lager:info("Request received", [
    {request_id, RequestId},
    {client, Client},
    {method, Method},
    {duration_ms, Duration}
]).
```

Log aggregation (ELK Stack):
```erlang
{lager_elasticsearch_backend, [
    {endpoint, "https://elasticsearch:9200"},
    {index, "erlmcp-{date}"},
    {type, "logs"},
    {level, info}
]}
```

## Scaling Strategy

### Horizontal Scaling

1. **Add more instances**: Update replica count
2. **Load balance**: Use Cloud Load Balancer or Nginx
3. **Session persistence**: Not needed (stateless)
4. **Database**: Scale separately (not in erlmcp)

### Vertical Scaling

1. **Increase CPU/Memory**: Update container resources
2. **Tune VM arguments**: Update ERL_FLAGS environment
3. **Connection limits**: Update +P (processes) and ERL_MAX_PORTS in vm.args or ERL_FLAGS
4. **Worker pools**: Update {workers, N} in sys.config

## Rollback Procedures

### Kubernetes Rollback
```bash
# View rollout history
kubectl rollout history deployment/erlmcp

# Rollback to previous version
kubectl rollout undo deployment/erlmcp

# Rollback to specific revision
kubectl rollout undo deployment/erlmcp --to-revision=3
```

### Cloud Run Rollback
```bash
gcloud run deploy erlmcp \
    --image gcr.io/YOUR_PROJECT_ID/erlmcp:v0.6.0 \
    --region us-central1
```

### Manual Rollback
```bash
# Stop current version
docker stop erlmcp

# Remove container
docker rm erlmcp

# Start previous version
docker run -d \
    --name erlmcp \
    -p 8080:8080 \
    erlmcp:v0.6.0
```

## Disaster Recovery

### Backup Strategy

```bash
# Backup configuration
tar czf config-backup-$(date +%Y%m%d-%H%M%S).tar.gz config/

# Backup release
tar czf release-backup-$(date +%Y%m%d-%H%M%S).tar.gz \
    _build/prod/rel/

# Store in GCS
gsutil cp *.tar.gz gs://erlmcp-backups/
```

### Restore Procedure

```bash
# Retrieve backup
gsutil cp gs://erlmcp-backups/config-backup-20260126.tar.gz .

# Extract
tar xzf config-backup-20260126.tar.gz

# Restart deployment
kubectl rollout restart deployment/erlmcp
```

### Disaster Recovery Drill

Monthly test:
1. Backup production config
2. Destroy all pods
3. Verify automatic recovery via HPA
4. Verify health checks pass
5. Test data consistency

## Compliance & Security

### TLS/SSL Configuration

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: erlmcp-ingress
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
spec:
  tls:
  - hosts:
    - erlmcp.example.com
    secretName: erlmcp-tls
  rules:
  - host: erlmcp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: erlmcp-service
            port:
              number: 80
```

### Secrets Management

```bash
# Create secret
kubectl create secret generic erlmcp-secret \
    --from-literal=erlang-cookie=$ERLANG_COOKIE \
    --from-literal=db-password=$DB_PASSWORD

# Use in deployment
env:
- name: ERLANG_COOKIE
  valueFrom:
    secretKeyRef:
      name: erlmcp-secret
      key: erlang-cookie
```

## Next Steps

- **To configure monitoring**: See [FOR_OPERATORS.md](FOR_OPERATORS.md)
- **To troubleshoot issues**: See [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **For GCP-specific setup**: See [GCP_SETUP.md](GCP_SETUP.md)
- **For architecture details**: See [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)

---

**Last Updated**: 2026-01-26
**Status**: Production-ready
**Target Platforms**: GCP Cloud Run, GKE, Docker Compose
