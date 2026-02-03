# GCP Simulation Stack

Docker-based simulation of Google Cloud Platform services for local development and testing.

## Overview

This stack provides local alternatives to GCP services, allowing development without GCP credentials or costs:

| GCP Service | Simulation | Endpoint |
|-------------|------------|----------|
| Cloud SQL | PostgreSQL 16 | `localhost:5432` |
| Cloud Memorystore | Redis 7 | `localhost:6379` |
| Cloud Storage | MinIO | `localhost:9000` (API), `localhost:9001` (Console) |
| Secret Manager | HashiCorp Vault | `localhost:8200` |
| Cloud KMS | Vault Transit | `localhost:8200` |
| Artifact Registry | Docker Registry | `localhost:5000` |
| Pub/Sub | GCP Emulator | `localhost:8085` |
| Firestore | GCP Emulator | `localhost:8086` |
| Cloud Monitoring | Prometheus | `localhost:9090` |
| Cloud Logging | Loki | `localhost:3100` |
| Cloud Trace | OTEL Collector | `localhost:4317` |
| Cloud Load Balancer | Traefik | `localhost:80` |
| GKE | K3s (optional) | `localhost:6443` |

## Quick Start

```bash
# Start core services
./scripts/start.sh

# Start with ErlMCP application
./scripts/start.sh --app

# Start full stack with Kubernetes
./scripts/start.sh --full

# Check status
./scripts/status.sh

# View logs
./scripts/logs.sh

# Stop services
./scripts/stop.sh

# Stop and remove all data
./scripts/stop.sh --clean
```

## Credentials

| Service | Username | Password |
|---------|----------|----------|
| PostgreSQL | `erlmcp` | `erlmcp_secret` |
| Redis | - | `erlmcp_redis` |
| MinIO | `erlmcp_storage` | `erlmcp_storage_secret` |
| Vault | - | `erlmcp_vault_token` |
| Grafana | `admin` | `erlmcp_grafana` |

## Environment Variables

Set these to use the simulation stack:

```bash
export PUBSUB_EMULATOR_HOST=localhost:8085
export FIRESTORE_EMULATOR_HOST=localhost:8086
export VAULT_ADDR=http://localhost:8200
export VAULT_TOKEN=erlmcp_vault_token
export STORAGE_ENDPOINT=http://localhost:9000
export DATABASE_URL=postgresql://erlmcp:erlmcp_secret@localhost:5432/erlmcp_dev
```

## Terraform Integration

Initialize Terraform with local providers:

```bash
cd terraform
terraform init
terraform apply
```

This creates:
- MinIO buckets (simulating GCS)
- PostgreSQL databases (simulating Cloud SQL)
- Vault secrets (simulating Secret Manager)
- Vault transit keys (simulating Cloud KMS)
- Environment configuration files

## Service Details

### PostgreSQL (Cloud SQL)

Pre-configured with:
- Extensions: `uuid-ossp`, `pg_stat_statements`, `pgcrypto`
- Databases: `erlmcp_dev`, `erlmcp_staging`, `erlmcp_prod`
- Audit logging schema

### MinIO (Cloud Storage)

Buckets created automatically:
- `erlmcp-terraform-state` - Terraform state
- `erlmcp-artifacts` - Build artifacts
- `erlmcp-backups` - Database backups
- `erlmcp-logs` - Application logs

### Vault (Secret Manager + KMS)

Secrets available at `erlmcp/*`:
- `erlmcp/database-url`
- `erlmcp/api-keys`
- `erlmcp/jwt-secret`

Transit encryption key: `erlmcp-app-key`

### Prometheus (Cloud Monitoring)

Pre-configured to scrape:
- ErlMCP application metrics
- PostgreSQL metrics
- Redis metrics
- MinIO metrics
- Traefik metrics

Alerting rules configured for:
- High error rates
- High latency
- Service availability
- Resource utilization

### Grafana (Dashboards)

Pre-provisioned datasources:
- Prometheus
- Loki
- PostgreSQL

Pre-built dashboard: ErlMCP Overview

## Docker-Only Constitution

All operations must run within Docker. The simulation stack enforces this by:

1. All services run as Docker containers
2. Terraform provisions resources via container APIs
3. No host binaries are executed
4. Network isolation via Docker bridge

## Profiles

- Default: Core services (postgres, redis, minio, vault, monitoring)
- `--app`: Include ErlMCP application container
- `--full`: Include K3s for Kubernetes testing

## Troubleshooting

### Services not starting

```bash
# Check Docker daemon
docker info

# View container logs
./scripts/logs.sh -f postgres

# Check health status
./scripts/status.sh
```

### Port conflicts

If ports are already in use, modify `docker-compose.yml` or stop conflicting services.

### Reset all data

```bash
./scripts/stop.sh --clean
./scripts/start.sh
```

## Architecture

```
                                    ┌─────────────────┐
                                    │     Traefik     │
                                    │  (Load Balancer)│
                                    └────────┬────────┘
                                             │
                    ┌────────────────────────┼────────────────────────┐
                    │                        │                        │
            ┌───────▼───────┐       ┌───────▼───────┐       ┌───────▼───────┐
            │    ErlMCP     │       │    Grafana    │       │    MinIO      │
            │  Application  │       │   Dashboard   │       │   Console     │
            └───────┬───────┘       └───────┬───────┘       └───────────────┘
                    │                       │
    ┌───────────────┼───────────────────────┼───────────────────────┐
    │               │                       │                       │
┌───▼───┐     ┌─────▼─────┐          ┌──────▼──────┐         ┌──────▼──────┐
│ Redis │     │ PostgreSQL │          │ Prometheus  │         │    Loki     │
└───────┘     └───────────┘          └─────────────┘         └─────────────┘
                                           │
                                    ┌──────▼──────┐
                                    │    Vault    │
                                    └─────────────┘
```
