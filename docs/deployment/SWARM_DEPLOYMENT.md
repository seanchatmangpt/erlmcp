# Docker Swarm Deployment Guide for erlmcp v3

This guide covers production deployment of erlmcp v3 using Docker Swarm with rolling updates, health checks, secrets management, overlay networks, service placement constraints, log aggregation, and auto-scaling.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [Network Setup](#network-setup)
- [Secrets Management](#secrets-management)
- [Service Deployment](#service-deployment)
- [Health Checks](#health-checks)
- [Rolling Updates](#rolling-updates)
- [Service Placement](#service-placement)
- [Log Aggregation](#log-aggregation)
- [Auto-Scaling](#auto-scaling)
- [Monitoring](#monitoring)
- [Troubleshooting](#troubleshooting)
- [Disaster Recovery](#disaster-recovery)

## Prerequisites

### Hardware Requirements

Minimum per node:
- CPU: 4 cores
- RAM: 8 GB
- Disk: 100 GB SSD

Recommended per node:
- CPU: 8 cores
- RAM: 16 GB
- Disk: 200 GB SSD

### Software Requirements

- Docker Engine 24.0+
- Docker Swarm mode enabled
- At least 3 manager nodes for quorum
- At least 3 worker nodes for HA

```bash
# Check Docker version
docker --version

# Initialize Swarm (on first manager)
docker swarm init --advertise-addr <MANAGER_IP>

# Join worker nodes
docker swarm join --token <WORKER_TOKEN> <MANAGER_IP>:2377
```

## Quick Start

### 1. Clone and Configure

```bash
# Clone repository
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# Copy environment template
cp .env.prod.template .env.prod

# Edit environment variables
vim .env.prod
```

### 2. Create Overlay Networks

```bash
# Create encrypted overlay networks
docker network create --driver overlay --attachable --opt encrypted erlmcp-overlay
docker network create --driver overlay --attachable --opt encrypted monitoring-overlay
```

### 3. Create Secrets

```bash
# Generate random secrets
openssl rand -base64 32 | docker secret create erlmcp-erlang-cookie -
openssl rand -base64 32 | docker secret create erlmcp-db-password -
openssl rand -base64 32 | docker secret create erlmcp-redis-password -

# Create TLS certificates (replace with your own)
openssl req -x509 -newkey rsa:4096 -keyout tls.key -out tls.crt -days 365 -nodes
docker secret create erlmcp-tls-cert tls.crt
docker secret create erlmcp-tls-key tls.key
rm tls.key tls.crt
```

### 4. Label Nodes

```bash
# Label nodes for service placement
docker node ls --format '{{.Hostname}}' | while read node; do
    docker node update --label-add erlmcp.enabled=true $node
done

# Label specific nodes for database
docker node update --label-add erlmcp.database=true <DATABASE_NODE>

# Label specific nodes for monitoring
docker node update --label-add erlmcp.monitoring=true <MONITORING_NODE>

# Label nodes with zones for spread
docker node update --label-add zone=a <NODE1>
docker node update --label-add zone=b <NODE2>
docker node update --label-add zone=c <NODE3>
```

### 5. Deploy Stack

```bash
# Deploy the stack
docker stack deploy -c docker/docker-stack.yml erlmcp

# Verify deployment
docker stack services erlmcp
docker stack ps erlmcp
```

## Network Setup

### Overlay Networks

The stack uses two overlay networks:

- **erlmcp-overlay**: For erlmcp services and dependencies (Redis, PostgreSQL)
- **monitoring-overlay**: For monitoring services (Prometheus, Grafana, Loki)

Both networks are encrypted for security:

```bash
# Verify encryption
docker network inspect erlmcp-overlay | grep encrypted
```

### Port Exposure

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| erlmcp  | 8080 | HTTP     | JSON-RPC API |
| erlmcp  | 9100 | HTTP     | Prometheus metrics |
| erlmcp  | 9090 | HTTP     | Health checks |
| erlmcp  | 4369 | TCP      | EPMD (Erlang) |
| redis   | 6379 | TCP      | Redis protocol |
| postgres| 5432 | TCP      | PostgreSQL |
| grafana | 3000 | HTTP     | Dashboard |
| prometheus | 9090 | HTTP | Metrics UI |

## Secrets Management

### Available Secrets

| Secret | Purpose | File Location |
|--------|---------|---------------|
| erlmcp-erlang-cookie | Erlang distribution cookie | /run/secrets/erlang.cookie |
| erlmcp-tls-cert | TLS certificate | /run/secrets/tls.crt |
| erlmcp-tls-key | TLS private key | /run/secrets/tls.key |
| erlmcp-db-password | PostgreSQL password | /run/secrets/db.password |
| erlmcp-redis-password | Redis password | /run/secrets/redis.password |

### Rotating Secrets

```bash
# To rotate a secret:
echo "new-secret-value" | docker secret create erlmcp-erlang-cookie-v2 -

# Update service to use new secret
docker service update \
  --secret-rm erlmcp-erlang-cookie \
  --secret-add source=erlmcp-erlang-cookie-v2,target=erlang.cookie \
  erlmcp_erlmcp

# Force restart to pick up new secret
docker service update --force erlmcp_erlmcp

# Remove old secret
docker secret rm erlmcp-erlang-cookie
```

## Service Deployment

### Initial Deployment

```bash
# Deploy with custom environment file
docker stack deploy -c docker/docker-stack.yml \
  --env-file .env.prod \
  erlmcp
```

### Scaling Services

```bash
# Scale erlmcp service
docker service scale erlmcp_erlmcp=10

# Scale Redis (for cluster mode)
docker service scale erlmcp_redis=3
```

### Updating Services

```bash
# Update service image
docker service update \
  --image ghcr.io/banyan-platform/erlmcp:3.0.1 \
  erlmcp_erlmcp

# Update environment variables
docker service update \
  --env-add NEW_VAR=value \
  erlmcp_erlmcp
```

## Health Checks

### Health Endpoints

The erlmcp service exposes several health endpoints:

| Endpoint | Purpose | Response Codes |
|----------|---------|----------------|
| GET /health | Overall health | 200 (healthy), 503 (unhealthy) |
| GET /ready | Readiness probe | 200 (ready), 503 (not ready) |
| GET /live | Liveness probe | 200 (alive) |
| GET /metrics | Prometheus metrics | 200 |

### Health Check Configuration

```yaml
healthcheck:
  test: ["CMD", "/opt/erlmcp/bin/healthcheck.sh"]
  interval: 15s
  timeout: 5s
  retries: 3
  start_period: 60s
```

### Monitoring Health

```bash
# Check service health
docker service ps erlmcp_erlmcp --no-trunc

# Check container health
docker ps --filter name=erlmcp --format "table {{.Names}}\t{{.Status}}"

# View health check logs
docker inspect <CONTAINER_ID> --format='{{json .State.Health}}' | jq
```

## Rolling Updates

### Update Configuration

The stack is configured for zero-downtime rolling updates:

```yaml
update_config:
  parallelism: 1          # Update one task at a time
  delay: 30s              # Wait 30s between updates
  failure_action: rollback  # Rollback on failure
  monitor: 60s            # Monitor for 60s after update
  max_failure_ratio: 0.2  # Rollback if 20% fail
  order: start-first      # Start new before stopping old
```

### Performing Rolling Updates

```bash
# Trigger rolling update
docker service update \
  --image ghcr.io/banyan-platform/erlmcp:3.0.1 \
  erlmcp_erlmcp

# Monitor update progress
docker service ps erlmcp_erlmcp --no-trunc

# Pause update if needed
docker service update erlmcp_erlmcp --pause

# Resume paused update
docker service update erlmcp_erlmcp --resume

# Rollback update
docker service rollback erlmcp_erlmcp
```

## Service Placement

### Placement Constraints

Services are placed using constraints and preferences:

```yaml
placement:
  constraints:
    - node.labels.erlmcp.enabled == true
    - node.arch == x86_64
  preferences:
    - spread: node.labels.zone
    - spread: node.labels.rack
```

### Viewing Placement

```bash
# See which nodes tasks are running on
docker service ps erlmcp_erlmcp --format "table {{.Node}}\t{{.CurrentState}}"

# View node labels
docker node inspect --format '{{ .Spec.Labels }}' <NODE_NAME>
```

## Log Aggregation

### Logging Configuration

Services use JSON file logging with Loki aggregation:

```yaml
logging:
  driver: "json-file"
  options:
    max-size: "10m"
    max-file: "3"
    labels: "com.erlmcp.environment,com.erlmcp.version"
    tag: "{{.Name}}/{{.ID}}"
```

### Viewing Logs

```bash
# View service logs
docker service logs erlmcp_erlmcp -f

# View logs for specific task
docker logs <TASK_ID> -f

# Query logs in Grafana/Loki
# Navigate to Grafana -> Explore -> Loki
```

### Log Locations

- Container logs: `docker logs <container>`
- Persistent logs: `/var/log/erlmcp/` (on host mount)
- Crash dumps: `/var/log/erlmcp/erl_crash.dump`

## Auto-Scaling

### Auto-Scaling Configuration

The stack includes an autoscaler service based on Prometheus metrics:

```yaml
environment:
  - SWARM_AUTOSCALER_TARGET_SERVICE=erlmcp_erlmcp
  - SWARM_AUTOSCALER_MIN_REPLICAS=3
  - SWARM_AUTOSCALER_MAX_REPLICAS=10
  - SWARM_AUTOSCALER_SCALE_UP_THRESHOLD=80
  - SWARM_AUTOSCALER_SCALE_DOWN_THRESHOLD=30
  - SWARM_AUTOSCALER_COOLDOWN_SECONDS=300
```

### Scaling Metrics

The autoscaler monitors:
- CPU usage per container
- Memory usage per container
- Request rate (HTTP requests/sec)
- Response time percentiles

### Manual Scaling Override

```bash
# Disable autoscaler temporarily
docker service scale erlmcp_autoscaler=0

# Scale manually
docker service scale erlmcp_erlmcp=15

# Re-enable autoscaler
docker service scale erlmcp_autoscaler=1
```

## Monitoring

### Grafana Dashboards

Access Grafana at `http://<MANAGER_IP>:3000`

Default credentials (change in production):
- Username: `admin`
- Password: Set via `GRAFANA_ADMIN_PASSWORD` environment variable

### Key Metrics to Monitor

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| erlmcp_process_count | Number of Erlang processes | > 250000 |
| erlmcp_memory_bytes | Total memory usage | > 4GB |
| erlmcp_ets_count | Number of ETS tables | > 40000 |
| http_request_duration | Request latency | p95 > 500ms |
| http_requests_total | Total requests | N/A |

### Setting Up Alerts

Create alert rules in `/config/docker/prometheus/alerts/`:

```yaml
groups:
  - name: erlmcp_alerts
    rules:
      - alert: HighMemoryUsage
        expr: erlmcp_memory_bytes > 4000000000
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High memory usage on {{ $labels.instance }}"
```

## Troubleshooting

### Service Not Starting

```bash
# Check service status
docker service ps erlmcp_erlmcp --no-trunc

# Check container logs
docker logs <TASK_ID>

# Check for resource issues
docker service ps erlmcp_erlmcp --format "{{.Error}}"
```

### Distribution Issues

```bash
# Check EPMD is running
docker exec <CONTAINER_ID> epmd -names

# Test node connectivity
docker exec <CONTAINER_ID> erl -name test@localhost -setcookie <cookie> -remsh erlmcp@<node>

# Check distribution ports
netstat -tuln | grep -E '4369|9100|9200'
```

### Health Check Failures

```bash
# Run health check manually
docker exec <CONTAINER_ID> /opt/erlmcp/bin/healthcheck.sh

# Check health endpoint
curl http://<CONTAINER_IP>:8080/health

# Check readiness
curl http://<CONTAINER_IP>:8080/ready
```

### Performance Issues

```bash
# Check resource limits
docker service inspect erlmcp_erlmcp --format '{{.Spec.TaskTemplate.Resources}}'

# View container stats
docker stats <CONTAINER_ID>

# Check Erlang scheduler utilization
docker exec <CONTAINER_ID> erl -noshell -eval 'erlang:display(erlang:statistics(scheduler_wall_time)), halt().'
```

## Disaster Recovery

### Backup Procedures

```bash
# Backup volumes (example for PostgreSQL)
docker run --rm \
  -v erlmcp_postgres-data:/data \
  -v $(pwd):/backup \
  alpine tar czf /backup/postgres-backup-$(date +%Y%m%d).tar.gz -C /data .

# Backup secrets
docker secret ls --format '{{.Name}}' | while read secret; do
    docker secret inspect $secret > backup/$secret.json
done

# Backup configuration
cp .env.prod backup/.env.prod.$(date +%Y%m%d)
cp docker/docker-stack.yml backup/docker-stack.yml.$(date +%Y%m%d)
```

### Recovery Procedures

```bash
# Restore volumes
docker run --rm \
  -v erlmcp_postgres-data:/data \
  -v $(pwd):/backup \
  alpine tar xzf /backup/postgres-backup-20240101.tar.gz -C /data

# Restore secrets
cat backup/erlmcp-erlang-cookie.json | jq -r '.[0].Spec.Data' | base64 -d | \
  docker secret create erlmcp-erlang-cookie -

# Redeploy stack
docker stack deploy -c docker/docker-stack.yml erlmcp
```

### Node Failure Recovery

```bash
# Remove failed node from swarm
docker node rm <NODE_NAME>

# Add new node
# On new node: docker swarm join --token <WORKER_TOKEN> <MANAGER_IP>:2377

# Rebalance services
docker service update erlmcp_erlmcp --force
```

### Split-Brain Recovery

If the cluster experiences split-brain:

```bash
# Identify partition with majority of managers
docker node ls

# On minority partition: leave swarm
docker swarm leave

# On majority partition: remove failed nodes
docker node rm <FAILED_NODE>

# Rejoin nodes after fixing network issues
```

## Appendix

### Docker Compose vs Docker Stack

| Feature | Docker Compose | Docker Stack |
|---------|----------------|--------------|
| Multi-host | No | Yes |
| Rolling updates | Manual | Automatic |
| Secrets | File-based | Native |
| Health checks | Basic | Advanced |
| Load balancing | None | Built-in |
| Auto-scaling | No | Yes (with external) |

### Security Checklist

- [ ] All secrets use Docker secrets (not environment variables)
- [ ] TLS enabled for all inter-service communication
- [ ] Overlay networks encrypted
- [ ] Non-root user in containers
- [ ] Read-only root filesystem
- [ ] No privileged containers
- [ ] Resource limits configured
- [ ] Security scanning enabled in CI/CD
- [ ] Regular secret rotation
- [ ] Audit logging enabled

### Performance Tuning

| Setting | Default | Production | Description |
|---------|---------|------------|-------------|
| ERL_MAX_PORTS | 65536 | 65536 | Max ports/sockets |
| ERL_MAX_ETS_TABLES | 50000 | 100000 | Max ETS tables |
| +P | 262144 | 524288 | Max processes |
| +Q | 65536 | 131072 | Port limit |
| Pool size | 50 | 100 | Connection pool |
| Replicas | 5 | 10+ | Service instances |
