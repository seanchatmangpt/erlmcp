# Docker Swarm Quick Reference - erlmcp v3

## Quick Start Commands

```bash
# Initialize Swarm (first time only)
docker swarm init

# Create overlay networks
docker network create --driver overlay --attachable --opt encrypted erlmcp-overlay
docker network create --driver overlay --attachable --opt encrypted monitoring-overlay

# Create secrets
openssl rand -base64 32 | docker secret create erlmcp-erlang-cookie -
openssl rand -base64 32 | docker secret create erlmcp-db-password -
openssl rand -base64 32 | docker secret create erlmcp-redis-password -

# Generate TLS cert
openssl req -x509 -newkey rsa:2048 -keyout tls.key -out tls.crt -days 365 -nodes -subj "/CN=erlmcp"
docker secret create erlmcp-tls-cert tls.crt
docker secret create erlmcp-tls-key tls.key
rm tls.key tls.crt

# Deploy stack
docker stack deploy -c docker/docker-stack.yml --env-file .env.prod erlmcp

# Or use the deployment script
./scripts/deploy-swarm.sh init
./scripts/deploy-swarm.sh secrets
./scripts/deploy-swarm.sh deploy
```

## Common Operations

```bash
# Check stack status
docker stack services erlmcp
docker stack ps erlmcp

# Check service logs
docker service logs erlmcp_erlmcp -f

# Scale service
docker service scale erlmcp_erlmcp=10

# Update service image
docker service update --image ghcr.io/banyan-platform/erlmcp:3.0.1 erlmcp_erlmcp

# Rollback update
docker service rollback erlmcp_erlmcp

# Remove stack
docker stack rm erlmcp
```

## Health Check Endpoints

```bash
# Check health
curl http://localhost:8080/health

# Check readiness
curl http://localhost:8080/ready

# Check liveness
curl http://localhost:8080/live

# Get metrics
curl http://localhost:9100/metrics
```

## Troubleshooting

```bash
# Service not starting
docker service ps erlmcp_erlmcp --no-trunc
docker logs <task_id>

# Distribution issues
docker exec <container_id> epmd -names

# High memory
docker stats
```

## Secrets

```bash
# List secrets
docker secret ls

# Inspect secret
docker secret inspect erlmcp-erlang-cookie

# Rotate secret
echo "new-value" | docker secret create erlmcp-erlang-cookie-v2 -
docker service update \
  --secret-rm erlmcp-erlang-cookie \
  --secret-add source=erlmcp-erlang-cookie-v2,target=erlang.cookie \
  erlmcp_erlmcp
```

## Files Reference

| File | Purpose |
|------|---------|
| `docker/docker-stack.yml` | Main Swarm configuration |
| `docker/Dockerfile.production` | Production Dockerfile |
| `config/docker/sys.config.swarm` | Erlang sys.config |
| `config/docker/vm.args.swarm` | VM args |
| `config/docker/prometheus.swarm.yml` | Prometheus config |
| `config/docker/otel-collector.yaml` | OTEL config |
| `.env.prod.template` | Environment template |
| `scripts/deploy-swarm.sh` | Deployment script |

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ERLMCP_IMAGE` | `ghcr.io/...:3.0.0` | Container image |
| `ERLMCP_REPLICAS` | `5` | Number of replicas |
| `ERLMCP_ENV` | `production` | Environment |
| `GRAFANA_ADMIN_PASSWORD` | `admin` | Grafana password |

## Node Labels

```bash
# Enable node for erlmcp
docker node update --label-add erlmcp.enabled=true <node>

# Database node
docker node update --label-add erlmcp.database=true <node>

# Monitoring node
docker node update --label-add erlmcp.monitoring=true <node>

# Zone labels
docker node update --label-add zone=a <node>
docker node update --label-add zone=b <node>
docker node update --label-add zone=c <node>
```
