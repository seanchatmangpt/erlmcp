# erlmcp - GCP AI Marketplace Deployment

Enterprise-grade Erlang/OTP implementation of the Model Context Protocol (MCP) SDK for Google Cloud.

## Overview

erlmcp provides fault-tolerant, distributed MCP servers and clients with support for multiple transport protocols. Built on the BEAM VM's legendary reliability, erlmcp enables seamless integration between AI models and enterprise systems.

## Deployment Options

### 1. Cloud Run (Serverless)

Recommended for most use cases. Auto-scaling, pay-per-use, minimal operations.

```bash
cd terraform
terraform init
terraform apply -var="project_id=YOUR_PROJECT" -var="deployment_types=[\"cloud_run\"]"
```

### 2. Compute Engine (VM-based)

For workloads requiring persistent connections, EPMD clustering, or specific VM configurations.

```bash
# Build VM image first
cd packer
packer build -var 'project_id=YOUR_PROJECT' erlmcp-gcp.pkr.hcl

# Deploy with Terraform
cd ../terraform
terraform apply -var="project_id=YOUR_PROJECT" -var="deployment_types=[\"gce\"]"
```

### 3. GKE (Kubernetes)

For complex deployments requiring custom orchestration, sidecars, or service mesh integration.

```bash
cd terraform
terraform apply \
  -var="project_id=YOUR_PROJECT" \
  -var="deployment_types=[\"gke\"]" \
  -var="gke_autopilot=true"
```

## Quick Start

### Prerequisites

- Google Cloud SDK (`gcloud`)
- Terraform >= 1.5.0
- Docker (for local testing)
- Packer (for VM image builds)

### Deploy to Cloud Run

```bash
# Authenticate
gcloud auth application-default login

# Set project
export PROJECT_ID=your-project-id
gcloud config set project $PROJECT_ID

# Enable APIs
gcloud services enable \
  run.googleapis.com \
  secretmanager.googleapis.com \
  artifactregistry.googleapis.com

# Build and push container
docker build -t gcr.io/$PROJECT_ID/erlmcp:latest .
docker push gcr.io/$PROJECT_ID/erlmcp:latest

# Deploy with Terraform
cd gcp/marketplace/terraform
terraform init
terraform apply \
  -var="project_id=$PROJECT_ID" \
  -var="deployment_types=[\"cloud_run\"]" \
  -var="cloud_run_image=gcr.io/$PROJECT_ID/erlmcp:latest"
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `ERLMCP_ENVIRONMENT` | Environment name | `production` |
| `ERLMCP_HTTP_PORT` | HTTP/WebSocket port | `8080` |
| `ERLMCP_TCP_PORT` | TCP transport port | `9090` |
| `ERLMCP_LOG_LEVEL` | Log level | `info` |
| `ERLMCP_COOKIE` | Erlang distribution cookie | (generated) |

### Terraform Variables

See `terraform/variables.tf` for complete variable documentation.

Key variables:

```hcl
# General
project_id       = "your-project-id"
region           = "us-central1"
environment      = "production"
deployment_types = ["cloud_run"]

# Cloud Run specific
cloud_run_min_instances = 1
cloud_run_max_instances = 100
cloud_run_cpu           = "2"
cloud_run_memory        = "2Gi"

# GCE specific
gce_machine_type = "e2-standard-4"
gce_min_instances = 2
gce_max_instances = 10

# GKE specific
gke_autopilot = true
gke_replicas  = 3
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Load Balancer                             │
│                   (Cloud Load Balancing)                         │
└─────────────────────────┬───────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          │               │               │
          ▼               ▼               ▼
    ┌──────────┐   ┌──────────┐   ┌──────────┐
    │ erlmcp   │   │ erlmcp   │   │ erlmcp   │
    │ Instance │   │ Instance │   │ Instance │
    │   (1)    │   │   (2)    │   │   (n)    │
    └────┬─────┘   └────┬─────┘   └────┬─────┘
         │              │              │
         └──────────────┼──────────────┘
                        │
    ┌───────────────────┼───────────────────┐
    │                   │                   │
    ▼                   ▼                   ▼
┌────────┐        ┌──────────┐        ┌─────────┐
│ Secret │        │ Cloud    │        │ Memory- │
│ Manager│        │ SQL      │        │ store   │
└────────┘        └──────────┘        └─────────┘
```

## Monitoring

### Cloud Monitoring Dashboard

After deployment, access the monitoring dashboard:

```
https://console.cloud.google.com/monitoring/dashboards?project=YOUR_PROJECT
```

### Alerts

Default alerts are configured for:
- High error rate (> 5%)
- High latency (P99 > 2s)
- Instance down
- High memory usage (> 85%)

### Metrics

Prometheus metrics available at `/metrics`:
- `erlmcp_request_total` - Total requests
- `erlmcp_request_latency` - Request latency histogram
- `erlmcp_active_sessions` - Active MCP sessions
- `erlmcp_vm_process_count` - Erlang process count
- `erlmcp_vm_memory_bytes` - Memory usage by type

## Security

### Secret Manager Integration

Sensitive configuration is stored in Secret Manager:
- `erlmcp-cookie` - Erlang distribution cookie
- `erlmcp-db-password` - Database password (if Cloud SQL enabled)
- `erlmcp-db-url` - Database connection URL

### IAM Roles

The service account requires:
- `roles/logging.logWriter`
- `roles/monitoring.metricWriter`
- `roles/cloudtrace.agent`
- `roles/secretmanager.secretAccessor`

### Network Security

- VPC with private subnets
- Cloud NAT for outbound traffic
- Firewall rules restrict access
- IAP for SSH access (optional)

## Compliance

- **Encryption at rest**: All data encrypted with Google-managed keys
- **Encryption in transit**: TLS 1.3 for all connections
- **Audit logging**: Cloud Audit Logs enabled
- **Data residency**: Configurable region deployment

## Troubleshooting

### Health Check Failures

```bash
# Check service logs
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=erlmcp"

# Check health endpoint
curl https://YOUR_SERVICE_URL/health
```

### Connection Issues

```bash
# Verify firewall rules
gcloud compute firewall-rules list --filter="name~erlmcp"

# Check VPC connector
gcloud compute networks vpc-access connectors describe erlmcp-connector --region=us-central1
```

### Performance Issues

```bash
# Check metrics
gcloud monitoring dashboards list --filter="displayName~erlmcp"

# Scale manually (Cloud Run)
gcloud run services update erlmcp --min-instances=5 --region=us-central1
```

## Support

- **Documentation**: https://github.com/seanchatmangpt/erlmcp
- **Issues**: https://github.com/seanchatmangpt/erlmcp/issues
- **Enterprise Support**: Contact for SLA-backed support

## License

Apache 2.0 - See [LICENSE](../../LICENSE)
