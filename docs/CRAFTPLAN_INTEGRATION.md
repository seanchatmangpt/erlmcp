# Craftplan ERP Integration

Self-hosted ERP for artisanal manufacturers, integrated with erlmcp Docker Swarm.

## Overview

**Craftplan** is an open-source ERP built with Elixir/Phoenix for small-scale artisanal manufacturers. It includes:

- **Catalog & BOM** - Product catalog with versioned bills of materials
- **Orders & Invoices** - Customer order processing with scheduling
- **Production** - Production batching with material consumption tracking
- **Inventory** - Raw material management with lot traceability
- **Purchasing** - Purchase orders and supplier management
- **CRM** - Customer and supplier database
- **API** - JSON:API and GraphQL endpoints

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Traefik Ingress                          │
│                    (TLS Termination, Routing)                     │
└───────────────────────────┬─────────────────────────────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        │                   │                   │
        ▼                   ▼                   ▼
┌───────────────┐  ┌─────────────────┐  ┌──────────────┐
│   Craftplan   │  │  erlmcp Services│  │  Grafana     │
│  (Phoenix)    │  │   (Erlang/OTP)  │  │  (Metrics)   │
└───────┬───────┘  └─────────────────┘  └──────────────┘
        │
        ├─────────────┬──────────────────┐
        ▼             ▼                  ▼
┌───────────────┐ ┌──────────────┐ ┌──────────────┐
│   PostgreSQL  │ │   MinIO S3   │ │   OTEL       │
│   (Database)  │ │  (Storage)   │ │ (Tracing)    │
└───────────────┘ └──────────────┘ └──────────────┘
```

## Quick Start

### 1. Prerequisites

```bash
# Ensure Docker Swarm is active
docker info --format '{{.Swarm.LocalNodeState}}'  # Should output: active

# Create overlay networks (if not exists)
docker network create --driver overlay --attachable erlmcp-frontend
docker network create --driver overlay --attachable erlmcp-backend
docker network create --driver overlay --attachable erlmcp-mgmt
```

### 2. Generate Secrets

```bash
./scripts/craftplan-secrets-setup.sh
```

This generates:
- `SECRET_KEY_BASE` - Phoenix session encryption
- `TOKEN_SIGNING_SECRET` - API authentication
- `CLOAK_KEY` - Field encryption (AES-256)
- `POSTGRES_PASSWORD` - Database password

### 3. Configure Environment

```bash
cp .env.craftplan.example .env.craftplan
# Edit .env.craftplan with your settings
```

Key settings:
```bash
CRAFTPLAN_HOST=craftplan.local  # Change to your domain
MINIO_ROOT_PASSWORD=changeme    # Set strong password
```

### 4. Deploy

```bash
./scripts/craftplan-deploy.sh
```

Or manually:
```bash
docker stack deploy -c docker-compose.craftplan.yml craftplan
```

### 5. Access

| Service | URL | Credentials |
|---------|-----|-------------|
| Web UI | http://localhost:4000 | test@test.com / Aa123123123123 |
| Via Traefik | https://craftplan.local | Same |
| MinIO Console | http://localhost:9001 | minioadmin / (your password) |
| API | https://craftplan.local/api | API key from settings |

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `CRAFTPLAN_HOST` | craftplan.local | Public hostname |
| `CRAFTPLAN_PORT` | 4000 | Internal port |
| `CRAFTPLAN_S3_BUCKET` | craftplan | S3 bucket name |
| `MINIO_ROOT_USER` | minioadmin | S3 admin user |
| `MINIO_ROOT_PASSWORD` | - | S3 admin password (required) |
| `CRAFTPLAN_BACKUP_SCHEDULE` | @daily | Backup cron |
| `CRAFTPLAN_BACKUP_RETENTION` | 30 | Backup retention days |

### Docker Secrets

| Secret | Description | Generation |
|--------|-------------|------------|
| `craftplan_secret_key_base` | Phoenix sessions | 48 bytes base64 |
| `craftplan_token_signing_secret` | API tokens | 48 bytes base64 |
| `craftplan_cloak_key` | Field encryption | 32 bytes base64 |
| `craftplan_db_password` | PostgreSQL | 32 bytes base64 |

## Service Placement

Nodes are labeled for optimal placement:

```bash
# View node labels
docker node ls --format '{{.Hostname}}: {{.Spec.Labels}}'

# Manually label nodes
docker node update --label-add craftplan=true node-1
docker node update --label-add craftplan-db=true node-1
docker node update --label-add craftplan-storage=true node-2
```

## Backup & Restore

### Automated Backups

Backups run daily (configurable via `CRAFTPLAN_BACKUP_SCHEDULE`):

```bash
# View backup files
ls -la /var/lib/craftplan/backups/

# Restore backup
docker exec -it $(docker ps -q -f name=craftplan-db) \
  psql -U postgres -d craftplan < /backups/craftplan_YYYYMMDD_HHMMSS.sql
```

### Manual Backup

```bash
# Database dump
docker exec $(docker ps -q -f name=craftplan-db) \
  pg_dump -U postgres craftplan > craftplan_backup.sql

# MinIO data
mc mirror craftplan-s3/data ./minio_backup/
```

## Scaling

```bash
# Scale application
docker service scale craftplan_craftplan=3

# Scale storage
docker service scale craftplan_craftplan-s3=2
```

Note: PostgreSQL runs as single replica (use Postgres Patroni for HA).

## Monitoring

Craftplan integrates with erlmcp observability stack:

```bash
# View logs
docker service logs craftplan_craftplan -f

# Metrics in Prometheus
curl http://localhost:9090/api/v1/query?query=up{job="craftplan"}

# Traces in Jaeger
open http://localhost:16686
```

## Troubleshooting

### Service Not Starting

```bash
# Check service status
docker stack ps craftplan --no-trunc

# Check logs
docker service logs craftplan_craftplan --tail 100
```

### Database Connection Issues

```bash
# Verify database is ready
docker exec $(docker ps -q -f name=craftplan-db) pg_isready -U postgres

# Check database logs
docker service logs craftplan_craftplan-db
```

### Secret Issues

```bash
# List secrets
docker secret ls --filter name=craftplan_

# Regenerate secrets
./scripts/craftplan-secrets-setup.sh --force
```

## Security

### Production Checklist

- [ ] Change default MinIO credentials
- [ ] Set strong database password
- [ ] Enable TLS via Traefik (Let's Encrypt)
- [ ] Configure firewall rules
- [ ] Enable audit logging
- [ ] Set up backup to external storage
- [ ] Review user permissions in Craftplan UI
- [ ] Configure email provider for notifications

### TLS Configuration

Traefik handles TLS automatically:

```yaml
labels:
  - "traefik.http.routers.craftplan.tls.certresolver=default"
  - "traefik.http.routers.craftplan.tls.domains[0].main=craftplan.local"
```

## API Access

Generate API key in Craftplan Settings → API:

```bash
# Example JSON:API request
curl -X GET https://craftplan.local/api/products \
  -H "Authorization: Bearer YOUR_API_KEY" \
  -H "Accept: application/vnd.api+json"
```

## Maintenance

### Updates

```bash
# Pull latest image
docker service update --image ghcr.io/puemos/craftplan:latest craftplan_craftplan

# Rollback if needed
docker service rollback craftplan_craftplan
```

### Database Maintenance

```bash
# Run vacuum
docker exec $(docker ps -q -f name=craftplan-db) \
  psql -U postgres -d craftplan -c "VACUUM ANALYZE;"

# Reindex
docker exec $(docker ps -q -f name=craftplan-db) \
  psql -U postgres -d craftplan -c "REINDEX DATABASE craftplan;"
```

## References

- [Craftplan Documentation](https://github.com/puemos/craftplan)
- [Phoenix Framework](https://www.phoenixframework.org/)
- [Ash Framework](https://ash-hq.org/)
- [Docker Swarm](https://docs.docker.com/engine/swarm/)
