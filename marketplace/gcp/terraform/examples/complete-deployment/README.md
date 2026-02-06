# erlmcp v3 Complete GCP Deployment

Production-grade, multi-platform deployment integrating GKE, Cloud Run, Compute Engine, and full observability stack.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    Global Load Balancer                         │
│                  (Cloud Armor + SSL/TLS)                        │
└────────────┬────────────────┬────────────────┬─────────────────┘
             │                │                │
    ┌────────▼──────┐  ┌─────▼──────┐  ┌─────▼──────┐
    │  Cloud Run    │  │    GKE     │  │    GCE     │
    │  (Serverless) │  │ (Kubernetes)│  │    (VMs)   │
    └────────┬──────┘  └─────┬──────┘  └─────┬──────┘
             │                │                │
             └────────────────┼────────────────┘
                              │
        ┌─────────────────────┴─────────────────────┐
        │          Shared Infrastructure             │
        ├────────────────────────────────────────────┤
        │ • VPC Network (Private, Multi-Region)      │
        │ • Secret Manager (Encrypted Secrets)       │
        │ • Cloud SQL (PostgreSQL HA)                │
        │ • Artifact Registry (Container Images)     │
        │ • Cloud Monitoring (Metrics + Logs)        │
        │ • Cloud Trace (Distributed Tracing)        │
        └────────────────────────────────────────────┘
```

## Platform Components

### 1. **GKE (Google Kubernetes Engine)**
- Kubernetes 1.30+ with autopilot or standard mode
- Private cluster with authorized networks
- Multi-zone node pools (primary + spot)
- Workload Identity for secure service access
- Binary Authorization for image security
- Managed Prometheus for metrics
- Helm chart deployment

**Use Case**: Stateful workloads, microservices, batch processing

### 2. **Cloud Run (Serverless)**
- Gen2 execution environment
- Auto-scaling 1-100 instances
- Direct VPC integration
- Session affinity support
- Custom domain mapping
- Built-in TLS termination

**Use Case**: Stateless API, event-driven, variable traffic

### 3. **Compute Engine (VMs)**
- Shielded VMs with secure boot
- Automated startup scripts
- Cloud Monitoring agent
- Managed instance groups
- Load balancer integration
- SSH via IAP tunnel

**Use Case**: Legacy apps, specific hardware requirements, persistent workloads

### 4. **Global Load Balancer**
- Path-based routing to different platforms
  - `/` → Cloud Run (default)
  - `/gke/*` → GKE backend
  - `/vm/*` → GCE backend
- Cloud Armor DDoS protection
- SSL/TLS termination
- Global anycast IP

### 5. **Shared Services**
- **VPC**: Private network with Cloud NAT
- **Cloud SQL**: PostgreSQL 15 with HA
- **Secret Manager**: Encrypted configuration
- **Artifact Registry**: Private container images
- **Observability**: Unified monitoring, logging, tracing

## Prerequisites

1. **GCP Project** with billing enabled
2. **Required APIs** (enabled automatically):
   - compute.googleapis.com
   - container.googleapis.com
   - run.googleapis.com
   - secretmanager.googleapis.com
   - cloudresourcemanager.googleapis.com
   - monitoring.googleapis.com
   - logging.googleapis.com

3. **Permissions**:
   - `roles/owner` or equivalent
   - `roles/container.admin`
   - `roles/compute.admin`
   - `roles/run.admin`

4. **Tools**:
   ```bash
   # Terraform 1.5+
   terraform version

   # gcloud CLI
   gcloud version

   # kubectl
   kubectl version --client

   # helm
   helm version
   ```

## Quick Start

### 1. Configure Authentication

```bash
# Authenticate with GCP
gcloud auth application-default login

# Set project
export PROJECT_ID="your-gcp-project-id"
gcloud config set project $PROJECT_ID
```

### 2. Prepare Configuration

```bash
# Copy example configuration
cp terraform.tfvars.example terraform.tfvars

# Edit configuration
vi terraform.tfvars
```

**Minimum required configuration**:
```hcl
project_id  = "your-gcp-project-id"
region      = "us-central1"
environment = "production"
image_tag   = "v3.0.0"
```

### 3. Deploy Infrastructure

```bash
# Initialize Terraform
terraform init

# Review deployment plan
terraform plan

# Deploy (15-25 minutes)
terraform apply
```

### 4. Verify Deployment

```bash
# Get outputs
terraform output

# Configure kubectl for GKE
$(terraform output -raw gke.kubectl_command)

# Check GKE pods
kubectl get pods -n erlmcp

# Test Cloud Run
curl $(terraform output -raw cloud_run.service_url)/health

# Test load balancer
curl https://erlmcp.example.com/health
```

## Configuration Guide

### Network Topology

```hcl
# Primary subnet for GKE and Cloud Run
primary_subnet_cidr = "10.0.0.0/24"

# Secondary subnet for GCE and failover
secondary_subnet_cidr = "10.10.0.0/24"

# GKE pod and service ranges
gke_pods_cidr     = "10.1.0.0/16"  # 65k IPs
gke_services_cidr = "10.2.0.0/16"  # 65k IPs
```

### Security Hardening

```hcl
# Private cluster configuration
gke_private_endpoint     = true
gke_master_global_access = false

# Restrict SSH access (use IAP tunnel)
ssh_source_ranges = []

# Enable security features
enable_binary_authorization = true
enable_cloud_armor         = true
enable_tls                 = true
```

### High Availability

```hcl
# GKE multi-zone deployment
gke_min_nodes = 3  # Minimum 3 nodes across zones

# Cloud Run auto-scaling
cloud_run_min_instances = 1
cloud_run_max_instances = 100

# Cloud SQL regional replication
cloud_sql_availability_type = "REGIONAL"

# GCE instance groups
gce_instance_count = 2
```

### Cost Optimization

```hcl
# Enable spot nodes (60-90% discount)
enable_spot_nodes     = true
gke_spot_min_nodes    = 0
gke_spot_max_nodes    = 5

# Cloud Run idle CPU allocation
cloud_run_cpu_idle = true

# Budget alerts
enable_budget_alerts  = true
monthly_budget_amount = 5000
```

## Platform Selection Guide

| Workload Type | Recommended Platform | Reasoning |
|---------------|---------------------|-----------|
| REST API (stateless) | Cloud Run | Auto-scaling, zero cold starts, cost-effective |
| WebSocket server | GKE | Persistent connections, session affinity |
| Batch processing | GKE (spot nodes) | Cost-effective, fault-tolerant |
| Database migration | GCE | Persistent disk, dedicated resources |
| Real-time stream | GKE | Low latency, stateful processing |
| Scheduled jobs | Cloud Run Jobs | Serverless, pay per execution |
| Legacy monolith | GCE | VM compatibility, gradual migration |

## Observability

### Dashboards

Access via Cloud Console:
```bash
# Performance dashboard
terraform output observability.dashboards.performance

# Erlang-specific metrics
terraform output observability.dashboards.erlang

# Security events
terraform output observability.dashboards.security
```

### Alerts

Pre-configured alerts:
- **Error Rate**: > 5% error rate for 5 minutes
- **Latency**: p99 > 1000ms for 5 minutes
- **Memory**: > 90% usage for 10 minutes
- **CPU**: > 80% usage for 15 minutes
- **Health Check**: 3 consecutive failures

### Logs

```bash
# Cloud Run logs
gcloud logging read "resource.type=cloud_run_revision" --limit 50

# GKE logs
kubectl logs -n erlmcp -l app=erlmcp --tail=100

# GCE logs
gcloud compute ssh erlmcp-vm-production-1 -- "sudo journalctl -u erlmcp -n 100"

# Aggregate logs (all platforms)
gcloud logging read "labels.app=erlmcp" --limit 100
```

### Traces

```bash
# View traces in Cloud Console
gcloud alpha monitoring dashboards list | grep erlmcp

# Sample trace query
gcloud logging read "trace=projects/$PROJECT_ID/traces/*" --limit 10
```

## Operations

### Deployment

```bash
# Deploy new version
terraform apply -var="image_tag=v3.1.0"

# Targeted update (Cloud Run only)
terraform apply -target=module.cloud_run

# Dry run
terraform plan -out=tfplan
```

### Scaling

```bash
# Scale GKE nodes
terraform apply -var="gke_max_nodes=20"

# Scale Cloud Run
terraform apply -var="cloud_run_max_instances=200"

# Scale GCE instances
terraform apply -var="gce_instance_count=5"
```

### Rollback

```bash
# Rollback to previous version
terraform apply -var="image_tag=v3.0.0"

# GKE rollback via kubectl
kubectl rollout undo deployment/erlmcp -n erlmcp

# Cloud Run rollback
gcloud run services update-traffic erlmcp-service-production \
  --to-revisions=erlmcp-service-production-v1=100
```

### Disaster Recovery

```bash
# Export Terraform state
gsutil cp gs://erlmcp-terraform-state/complete-deployment/state/* ./backup/

# Backup Cloud SQL
gcloud sql backups create --instance=erlmcp-production-db

# Export secrets
./scripts/export-secrets.sh

# Restore from backup
terraform import google_sql_database_instance.erlmcp erlmcp-production-db
```

## Troubleshooting

### GKE Issues

```bash
# Check cluster health
kubectl get nodes
kubectl get pods -n erlmcp

# View events
kubectl get events -n erlmcp --sort-by='.lastTimestamp'

# Describe pod
kubectl describe pod <pod-name> -n erlmcp

# Pod logs
kubectl logs -f <pod-name> -n erlmcp
```

### Cloud Run Issues

```bash
# Check service status
gcloud run services describe erlmcp-service-production

# View logs
gcloud logging read "resource.type=cloud_run_revision" --limit 50

# Test locally
docker run -p 8080:8080 us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp-server:v3.0.0
```

### GCE Issues

```bash
# SSH to instance
gcloud compute ssh erlmcp-vm-production-1

# Check service status
sudo systemctl status erlmcp

# View startup script logs
sudo journalctl -u google-startup-scripts

# Docker logs
sudo docker logs erlmcp
```

### Network Issues

```bash
# Test connectivity
gcloud compute ssh erlmcp-vm-production-1 -- "curl -v http://localhost:8080/health"

# Check firewall rules
gcloud compute firewall-rules list --filter="network=erlmcp-vpc"

# Verify NAT
gcloud compute routers get-status erlmcp-cloud-router --region=us-central1
```

## Cost Estimation

**Monthly costs** (us-central1, production configuration):

| Component | Configuration | Monthly Cost (USD) |
|-----------|--------------|-------------------|
| GKE | 3x n2-standard-4 nodes | ~$350 |
| Cloud Run | 1-100 instances, 2 vCPU, 2GB | ~$150-300 |
| GCE | 2x n2-standard-4 instances | ~$300 |
| Cloud SQL | db-custom-2-7680, regional | ~$250 |
| Load Balancer | Global, 1TB traffic | ~$50 |
| Cloud Armor | 1M requests | ~$25 |
| Networking | 1TB egress | ~$120 |
| Storage | 500GB logs, backups | ~$25 |
| **Total** | | **~$1,270-1,420/mo** |

Add 20-30% for monitoring, logging, and overhead.

**Cost optimization tips**:
1. Enable spot nodes: Save 60-90%
2. Use committed use discounts: Save 37-55%
3. Optimize Cloud Run min instances
4. Enable auto-scaling everywhere
5. Set up budget alerts

## Security Best Practices

1. **Network Isolation**
   - Private GKE cluster
   - VPC Service Controls
   - Cloud NAT for egress

2. **Authentication**
   - Workload Identity
   - Service account per platform
   - Least privilege IAM

3. **Encryption**
   - TLS in transit
   - CMEK for data at rest
   - Secret Manager for credentials

4. **Compliance**
   - Binary Authorization
   - Container scanning
   - Audit logging enabled

5. **Monitoring**
   - Security dashboard
   - Anomaly detection
   - Real-time alerts

## Cleanup

```bash
# Destroy all resources
terraform destroy

# Verify deletion
gcloud compute instances list
gcloud run services list
gcloud container clusters list

# Remove state bucket (optional)
gsutil rm -r gs://erlmcp-terraform-state
```

## Support

- **Documentation**: https://github.com/your-org/erlmcp
- **Issues**: https://github.com/your-org/erlmcp/issues
- **Slack**: #erlmcp-support

## License

Copyright 2026 - Apache 2.0 License
