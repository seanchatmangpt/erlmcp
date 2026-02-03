# erlmcp v3 Docker Swarm Setup

Enterprise-grade Docker Swarm orchestration for erlmcp targeting Fortune 500 deployments.

## Overview

This Docker Swarm setup provides:

- **Multi-Container Architecture**: Core, transports, observability services
- **High Availability**: 3-node manager cluster with automated failover
- **Rolling Updates**: Zero-downtime deployments with rollback capabilities
- **Enterprise Security**: Secrets management, TLS encryption, network isolation
- **Monitoring & Alerting**: Prometheus, Grafana, ELK stack integration
- **Auto-Scaling**: Resource-aware scaling policies
- **Load Balancing**: Traefik ingress with SSL termination
- **Volume Persistence**: Persistent storage for data and logs

## Architecture

### Network Topology
```
┌─────────────────────────────────────────────────────────────┐
│                  erlmcp-swarm Cluster                       │
├─────────────────┬─────────────────┬───────────────────────┤
│  Public Network │ Overlay Network │ Management Network    │
│    (172.22.0.0) │   (172.20.0.0) │   (172.21.0.0)      │
│  - HTTP/HTTPS   │  - Core/Transp │  - Monitoring        │
│  - Services     │  - Database    │  - Internal         │
└─────────────────┴─────────────────┴───────────────────────┘
```

### Service Components
- **erlmcp-core**: 3 replicas on managers
- **erlmcp-transports**: 13 total replicas across stdio, tcp, http
- **erlmcp-observability**: OpenTelemetry collector
- **Prometheus/Grafana**: Monitoring stack
- **Elasticsearch/Logstash/Kibana**: Logging stack

## Prerequisites

- Docker Engine 20.10+
- Docker Swarm initialized
- 3+ manager nodes
- 5+ worker nodes
- Minimum 8GB RAM per node
- 50GB free disk space

## Quick Start

### 1. Initialize Swarm
```bash
# On manager node 1
./deploy-swarm.sh init
```

### 2. Deploy Services
```bash
./deploy-swarm.sh deploy
```

### 3. Monitor Services
```bash
./monitor-alert.sh monitor
```

## Detailed Deployment

### Step 1: Swarm Initialization

```bash
# Initialize Docker Swarm on first manager
docker swarm init --advertise-addr MANAGER_IP:2377

# Join other managers
docker swarm join --token TOKEN MANAGER_IP:2377

# Join workers
docker swarm join --token TOKEN MANAGER_IP:2377
```

### Step 2: Node Labeling

Label nodes for service placement:
```bash
# Manager nodes
docker node update --label-add erlmcp-role=manager NODE_ID

# Worker nodes
docker node update --label-add erlmcp-role=core NODE_ID
docker node update --label-add erlmcp-role=transport NODE_ID
docker node update --label-add erlmcp-role=observability NODE_ID
docker node update --label-add erlmcp-role=database NODE_ID
```

### Step 3: Secret Management

Create secure secrets:
```bash
# Generate secrets
openssl rand -hex 32 > secrets/jwt-secret
openssl rand -base64 32 > secrets/db-password

# Create Docker secrets
for secret in secrets/*; do
  docker secret create erlmcp-$(basename $secret) $secret
done
```

### Step 4: Service Deployment

Deploy the stack:
```bash
docker stack deploy -c docker-compose.yml erlmcp-swarm
```

## Configuration

### Environment Variables
```bash
# In .env file
ERLMCP_VERSION=v3.0.0
ERLMCP_CLUSTER_SIZE=3
ERLMCP_CORE_REPLICAS=3
ERLMCP_TCP_REPLICAS=5
ERLMCP_HTTP_REPLICAS=5
ERLMCP_OTEL_ENABLED=true
```

### Scaling Services
```bash
# Scale core service
docker service scale erlmcp-swarm_erlmcp-core=5

# Scale transport services
docker service scale erlmcp-swarm_erlmcp-transport-tcp=10
```

### Rolling Updates

Update services with zero downtime:
```bash
# Update service image
docker service update erlmcp-swarm_erlmcp-core --image erlmcp/erlmcp-core:v3.0.1

# Monitor update progress
docker service ps erlmcp-swarm_erlmcp-core
```

## Monitoring

### Grafana Dashboards

Access at `http://localhost:3000`

Default credentials: `admin/admin` (change immediately!)

Key dashboards:
- **System Overview**: Node metrics, service health
- **Application Metrics**: erlmcp-specific metrics
- **Network**: Traffic, connections, throughput
- **Performance**: CPU, memory, disk I/O

### Prometheus Queries

Service Health:
```promql
up{job="erlmcp"}
```

CPU Usage:
```promql
100 - (avg by (service) (rate(container_cpu_usage_seconds_total[5m])) * 100)
```

Memory Usage:
```promql
(container_memory_usage_bytes / container_spec_memory_limit_bytes) * 100
```

### Alerting

Alerts configured for:
- High CPU usage (>80%)
- High memory usage (>85%)
- Service down
- High error rate
- Slow response time
- Connection pool exhausted

Send alerts to Slack or email.

## Security

### Network Security

- **Overlay Network**: Internal communication encryption
- **Public Network**: External traffic isolation
- **Management Network**: Internal monitoring traffic

### Secrets Management

- Docker Swarm secrets for sensitive data
- Encrypted at rest and in transit
- Limited access to service containers

### Container Security

- Read-only filesystems where possible
- Minimal base images
- Regular security scanning
- CIS Docker Benchmark compliance

## Backup and Recovery

### Data Backup

```bash
# Backup databases
docker exec erlmcp-swarm_erlmcp-session-db-1 mariabackup --backup --target-dir=/backup

# Backup logs
docker exec erlmcp-swarm_logstash-1 tar -czf /backup/logs.tar.gz -C /usr/share/logstash data

# Backup metrics
docker exec erlmcp-swarm_prometheus-1 tar -czf /backup/metrics.tar.gz -C /prometheus data
```

### Disaster Recovery

1. **Restore data from backups**
2. **Deploy stack with new configuration**
3. **Verify service health**
4. **Update DNS records**
5. **Monitor traffic**

## Troubleshooting

### Common Issues

**Service not starting:**
```bash
# Check service logs
docker service logs erlmcp-swarm_erlmcp-core

# Check node constraints
docker node ls
```

**High CPU usage:**
```bash
# Identify service
docker stats --no-stream

# Scale horizontally
docker service scale erlmcp-swarm_erlmcp-core=5
```

**Network issues:**
```bash
# Check network connectivity
docker network inspect erlmcp-overlay

# Test service communication
docker exec -it erlmcp-swarm_erlmcp-core-1 ping erlmcp-registry
```

### Health Checks

**Service Health:**
```bash
# Core service
curl http://localhost:8080/health

# Transport services
curl http://localhost:9101/health
curl http://localhost:9102/health
```

**System Health:**
```bash
# Node health
docker node inspect NODE_ID

# Resource usage
docker stats
```

## Performance Tuning

### Resource Limits

```yaml
# Adjust in docker-compose.yml
resources:
  limits:
    cpus: '4.0'
    memory: 8G
  reservations:
    cpus: '2.0'
    memory: 4G
```

### Optimization Strategies

1. **Horizontal Scaling**: Add more service replicas
2. **Vertical Scaling**: Increase container resources
3. **Load Balancing**: Distribute traffic across nodes
4. **Caching**: Implement Redis cache
5. **Connection Pooling**: Tune database connections

## Maintenance

### Regular Tasks

- **Daily**: Check logs, monitor metrics
- **Weekly**: Rotate logs, update dependencies
- **Monthly**: Security patches, performance review
- **Quarterly**: Backup testing, disaster recovery drill

### Upgrade Process

1. **Backup current state**
2. **Update service images**
3. **Roll out updates**
4. **Monitor performance**
5. **Rollback if needed**

## Support

### Documentation

- Architecture diagrams: `/docs/architecture.md`
- API reference: `/docs/api-reference.md`
- Configuration guide: `/docs/configuration.md`

### Issue Reporting

Create issues with:
- Service name and version
- Error messages
- Relevant logs
- Steps to reproduce

### Support Contacts

- **Support**: support@erlmcp.com
- **Enterprise**: enterprise@erlmcp.com
- **Security**: security@erlmcp.com

## License

Enterprise License required for Fortune 500 deployments.

---

*This setup meets all requirements for Fortune 500 enterprise deployments including high availability, security, monitoring, and compliance.*