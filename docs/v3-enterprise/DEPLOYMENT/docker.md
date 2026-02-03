# Docker Deployment Guide

## Overview

This guide covers deploying erlmcp v3 using Docker containers. Docker provides a consistent, portable deployment method suitable for development, testing, and production environments.

## Prerequisites

- Docker 20.10+
- Docker Compose 2.0+
- At least 2GB RAM per container
- 4GB free disk space
- Ports: 8080 (HTTP), 8081 (Metrics), 8082 (Dashboard)

## Quick Start

### 1. Basic Deployment

```bash
# Clone the repository
git clone https://github.com/erlmcp/erlmcp.git
cd erlmcp

# Build the Docker image
docker build -t erlmcp:v3 -f Dockerfile .

# Run a single instance
docker run -d \
  --name erlmcp-node1 \
  -p 8080:8080 \
  -p 8081:8081 \
  -p 8082:8082 \
  -e ERLMCP_MODE=production \
  -e ERLMCP_NODE_NAME=erlmcp@172.17.0.1 \
  -e ERLMCP_COOKIE=secret \
  erlmcp:v3
```

### 2. Multi-Container Setup

```yaml
# docker-compose.yml
version: '3.8'

services:
  erlmcp1:
    image: erlmcp:v3
    environment:
      - ERLMCP_MODE=production
      - ERLMCP_NODE_NAME=erlmcp@erlmcp1
      - ERLMCP_COOKIE=secret123
    ports:
      - "8080:8080"
      - "8081:8081"
      - "8082:8082"
    networks:
      - erlmcp-net
    depends_on:
      - redis

  erlmcp2:
    image: erlmcp:v3
    environment:
      - ERLMCP_MODE=production
      - ERLMCP_NODE_NAME=erlmcp@erlmcp2
      - ERLMCP_COOKIE=secret123
    ports:
      - "8081:8080"  # Different port for second instance
    networks:
      - erlmcp-net
    depends_on:
      - redis

  redis:
    image: redis:7-alpine
    networks:
      - erlmcp-net

networks:
  erlmcp-net:
    driver: bridge
```

```bash
# Start the cluster
docker-compose up -d
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `ERLMCP_MODE` | Runtime mode (dev/test/prod) | dev |
| `ERLMCP_NODE_NAME` | Erlang node name | erlmcp@localhost |
| `ERLMCP_COOKIE` | Distributed Erlang cookie | erlmcp |
| `ERLMCP_TRANSPORT` | Transport protocol (http|ws|stdio) | http |
| `ERLMCP_PORT` | Service port | 8080 |
| `ERLMCP_METRICS_PORT` | Metrics port | 8081 |
| `ERLMCP_DASHBOARD_PORT` | Dashboard port | 8082 |
| `ERLMCP_SESSION_BACKEND` | Session backend (ets/dets/mnesia) | ets |
| `ERLMCP_CLUSTER_NODES` | Cluster node names | [] |
| `ERLMCP_LOG_LEVEL` | Log level (debug/info/warn/error) | info |
| `ERLMCP_SSL_ENABLED` | Enable SSL | false |
| `ERLMCP_SSL_CERT` | SSL certificate path | |
| `ERLMCP_SSL_KEY` | SSL private key path | |

### Configuration Files

#### 1. Production Configuration

```bash
# Create production config
mkdir -p /opt/erlmcp/config
cat > /opt/erlmcp/config/vm.args << EOF
-name erlmcp@$(hostname -i)
-setcookie secret
-kernel inet_dist_use_interface {192.168.1.0,24}
-heart
-setenv ERLMCP_MODE=production
EOF

cat > /opt/erlmcp/config/sys.config << EOF
[{erlmcp, [
    {mode, production},
    {session_backend, dets},
    {transport, http},
    {port, 8080},
    {metrics_port, 8081},
    {dashboard_port, 8082},
    {ssl_enabled, false},
    {log_level, info},
    {cluster_nodes, ['erlmcp@node1', 'erlmcp@node2']}
]}].
EOF
```

#### 2. Docker Volume Mount

```bash
docker run -d \
  --name erlmcp-prod \
  -v /opt/erlmcp/config:/app/config \
  -v /opt/erlmcp/data:/app/data \
  -e ERLMCP_MODE=production \
  erlmcp:v3
```

## Production Deployment

### 1. Multi-Node Cluster

```yaml
# docker-compose.prod.yml
version: '3.8'

services:
  erlmcp1:
    image: erlmcp:v3
    environment:
      - ERLMCP_MODE=production
      - ERLMCP_NODE_NAME=erlmcp@erlmcp1
      - ERLMCP_COOKIE=secret123
      - ERLMCP_CLUSTER_NODES=erlmcp@erlmcp1,erlmcp@erlmcp2,erlmcp@erlmcp3
    ports:
      - "8080:8080"
      - "8081:8081"
      - "8082:8082"
    networks:
      - erlmcp-net
    volumes:
      - /opt/erlmcp/data/erlmcp1:/app/data
      - /var/log/erlmcp:/app/log
    restart: unless-stopped

  erlmcp2:
    image: erlmcp:v3
    environment:
      - ERLMCP_MODE=production
      - ERLMCP_NODE_NAME=erlmcp@erlmcp2
      - ERLMCP_COOKIE=secret123
      - ERLMCP_CLUSTER_NODES=erlmcp@erlmcp1,erlmcp@erlmcp2,erlmcp@erlmcp3
    ports:
      - "8081:8080"  # Different port
    networks:
      - erlmcp-net
    volumes:
      - /opt/erlmcp/data/erlmcp2:/app/data
      - /var/log/erlmcp:/app/log
    restart: unless-stopped

  erlmcp3:
    image: erlmcp:v3
    environment:
      - ERLMCP_MODE=production
      - ERLMCP_NODE_NAME=erlmcp@erlmcp3
      - ERLMCP_COOKIE=secret123
      - ERLMCP_CLUSTER_NODES=erlmcp@erlmcp1,erlmcp@erlmcp2,erlmcp@erlmcp3
    ports:
      - "8082:8080"  # Different port
    networks:
      - erlmcp-net
    volumes:
      - /opt/erlmcp/data/erlmcp3:/app/data
      - /var/log/erlmcp:/app/log
    restart: unless-stopped

  haproxy:
    image: haproxy:2.4-alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg
    networks:
      - erlmcp-net
    depends_on:
      - erlmcp1
      - erlmcp2
      - erlmcp3
    restart: unless-stopped

networks:
  erlmcp-net:
    driver: bridge
```

### 2. HAProxy Configuration

```bash
# haproxy.cfg
global
    log /dev/log local0
    log /dev/log local1 notice
    maxconn 4000
    user haproxy
    group haproxy
    daemon

defaults
    mode http
    timeout connect 5000
    timeout client 50000
    timeout server 50000
    option httplog
    option dontlognull
    option http-server-close
    option forwardfor except 127.0.0.0/8
    option redispatch

frontend http-in
    bind *:80
    default_backend erlmcp-backend

backend erlmcp-backend
    balance roundrobin
    option httpchk GET /health
    http-check expect status 200
    server erlmcp1 erlmcp1:8080 check
    server erlmcp2 erlmcp2:8080 check
    server erlmcp3 erlmcp3:8080 check
```

## Monitoring

### 1. Health Checks

```bash
# Check health
curl http://localhost:8080/health

# Check metrics
curl http://localhost:8081/metrics

# Check dashboard
curl http://localhost:8082/dashboard
```

### 2. Log Collection

```bash
# View logs
docker logs erlmcp1
docker logs -f erlmcp1  # Follow logs

# Aggregate logs with Fluentd
```

### 3. Resource Monitoring

```bash
# Check container resources
docker stats erlmcp1

# Monitor memory usage
docker exec erlmcp1 top -b -n 1 | grep beam
```

## Scaling

### 1. Horizontal Scaling

```bash
# Add new node
docker run -d \
  --name erlmcp4 \
  -p 8083:8080 \
  -e ERLMCP_MODE=production \
  -e ERLMCP_NODE_NAME=erlmcp@erlmcp4 \
  -e ERLMCP_COOKIE=secret123 \
  -e ERLMCP_CLUSTER_NODES=erlmcp@erlmcp1,erlmcp@erlmcp2,erlmcp@erlmcp3,erlmcp@erlmcp4 \
  --network erlmcp-net \
  erlmcp:v3
```

### 2. Auto-scaling with Kubernetes

See the Kubernetes deployment guide for advanced auto-scaling configurations.

## Security Considerations

### 1. Container Security

```bash
# Run as non-root user
docker run --user 1000:1000 erlmcp:v3

# Read-only root filesystem
docker run --read-only erlmcp:v3

# Security profiles
docker run --security-opt no-new-privileges erlmcp:v3
```

### 2. Network Security

```bash
# Private network
docker run --network erlmcp-net erlmcp:v3

# Port restrictions
docker run -p 127.0.0.1:8080:8080 erlmcp:v3
```

### 3. Secret Management

```bash
# Use Docker secrets
echo "secret123" | docker secret create erlmcp-cookie -
docker run --secret erlmcp-cookie erlmcp:v3
```

## Backup and Recovery

### 1. Data Backup

```bash
# Backup data directory
docker exec erlmcp1 tar czf - -C /app/data . > erlmcp1-backup.tar.gz

# Backup configuration
docker cp erlmcp1:/app/config /backup/erlmcp-config
```

### 2. Disaster Recovery

```bash
# Restore from backup
docker stop erlmcp1
docker rm erlmcp1
docker run -d \
  --name erlmcp1-restore \
  -v /backup/erlmcp-data:/app/data \
  erlmcp:v3
```

## Troubleshooting

### 1. Common Issues

**Cluster Not Forming**
```bash
# Check cookie consistency
docker exec erlmcp1 grep cookie /app/config/vm.args
docker exec erlmcp2 grep cookie /app/config/vm.args

# Check network connectivity
docker exec erlmcp1 ping erlmcp2
```

**Memory Issues**
```bash
# Check memory usage
docker stats --no-stream erlmcp1
docker exec erlmcp1 erl -eval 'io:format("~p~n", [erlang:memory(total)]), halt().'
```

**Port Conflicts**
```bash
# Check port usage
netstat -tulpn | grep 8080
```

### 2. Debug Mode

```bash
# Run with debug logging
docker run -e ERLMCP_LOG_LEVEL=debug erlmcp:v3

# Interactive debugging
docker run --rm -it --entrypoint /bin/bash erlmcp:v3
```

## Best Practices

1. **Use production-ready images** from official registry
2. **Implement proper health checks** in orchestration
3. **Use volume mounts** for persistent data
4. **Monitor resource usage** and set limits
5. **Regular security updates** for base images
6. **Backup data regularly** and test recovery
7. **Use orchestration tools** for multi-node deployments
8. **Implement logging aggregation** for troubleshooting

## Next Steps

For advanced deployments, see:
- [Kubernetes Deployment](../kubernetes.md)
- [Cloud Deployment](../cloud.md)
- [On-Premise Deployment](../on-premises.md)