# Google Cloud AI Marketplace Deployment for erlmcp

This directory contains production-grade infrastructure and deployment code for deploying erlmcp on Google Cloud AI Marketplace.

## Overview

The erlmcp Marketplace deployment provides three deployment modes:

1. **GCE VM** - Compute Engine instances with Docker and auto-scaling
2. **Cloud Run** - Serverless container service with automatic scaling
3. **GKE Enhanced** - Kubernetes with marketplace-specific annotations

### Deployment Options

| Option | Use Case | SLA | Cost | Complexity |
|--------|----------|-----|------|------------|
| **Cloud Run** | Dev/Test, low traffic | 99.9% | $ (pay-per-use) | Low |
| **GKE Regional** | Production, HA | 99.95% | $$ | Medium |
| **GKE Multi-Region** | Enterprise, global | 99.99% | $$$ | High |
| **Compute Engine** | Legacy migration | 99.9% | $$ | Low-Medium |

## Prerequisites

### Google Cloud Setup

1. **Create a GCP Project**
   ```bash
   gcloud projects create erlmcp-production --name="erlmcp Production"
   gcloud config set project erlmcp-production
   ```

2. **Enable Required APIs**
   ```bash
   gcloud services enable \
     compute.googleapis.com \
     container.googleapis.com \
     artifactregistry.googleapis.com \
     secretmanager.googleapis.com \
     run.googleapis.com \
     cloudbuild.googleapis.com \
     clouddeploy.googleapis.com \
     monitoring.googleapis.com \
     logging.googleapis.com
   ```

3. **Configure Authentication**
   ```bash
   gcloud auth login
   gcloud auth configure-docker us-central1-docker.pkg.dev
   ```

### Local Tools

- Terraform >= 1.5.0
- Packer >= 1.9.0
- Helm >= 3.12.0
- gcloud CLI >= 440.0.0
- Docker >= 24.0.0

## Quick Start

### Option 1: Cloud Run (Fastest)

```bash
cd marketplace/gcp/terraform/examples/cloud-run-deployment

# Copy and edit variables
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars with your values

# Deploy
terraform init
terraform apply
```

### Option 2: GKE (Recommended for Production)

```bash
cd marketplace/gcp/terraform/examples/gke-deployment

# Copy and edit variables
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars with your values

# Deploy
terraform init
terraform apply

# Get credentials
gcloud container clusters get-credentials erlmcp-cluster \
  --region us-central1 \
  --project erlmcp-production
```

### Option 3: Compute Engine

```bash
cd marketplace/gcp/terraform/examples/gce-deployment

# Copy and edit variables
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars with your values

# Deploy
terraform init
terraform apply
```

## Building Custom VM Image

To create a custom GCE image with erlmcp pre-installed:

```bash
cd marketplace/gcp/packer

# Set variables
export PROJECT_ID=$(gcloud config get-value project)
export IMAGE_FAMILY=erlmcp-3
export IMAGE_NAME=erlmcp-3.0.0

# Build image
packer build \
  -var "project_id=${PROJECT_ID}" \
  -var "image_family=${IMAGE_FAMILY}" \
  gce-image.pkr.hcl
```

## Secret Management

### Required Secrets

Create these secrets in Google Secret Manager before deployment:

```bash
# 1. Erlang Cookie (generate secure value)
openssl rand -base64 48 | tr -d '\n' | \
  gcloud secrets create erlmcp-erlang-cookie --data-file=-

# 2. Database Password
echo "your-db-password" | \
  gcloud secrets create erlmcp-db-password --data-file=-

# 3. Redis Password
echo "your-redis-password" | \
  gcloud secrets create erlmcp-redis-password --data-file=-

# 4-11. TLS and other certificates
gcloud secrets create erlmcp-tls-cert --data-file=tls.crt
gcloud secrets create erlmcp-tls-key --data-file=tls.key
gcloud secrets create erlmcp-ca-bundle --data-file=ca.crt
gcloud secrets create erlmcp-jwt-private-key --data-file=jwt-private.pem
gcloud secrets create erlmcp-jwt-public-key --data-file=jwt-public.pem
gcloud secrets create erlmcp-grafana-password --data-file=-
gcloud secrets create erlmcp-backup-key --data-file=-
gcloud secrets create erlmcp-otel-ca-cert --data-file=otel-ca.crt
```

## Verification

### Health Checks

```bash
# Cloud Run
curl $(gcloud run services describe erlmcp --region=us-central1 --format='value(status.url)')/health

# GKE
kubectl get pods -n erlmcp
kubectl get svc -n erlmcp
curl http://<load-balancer-ip>/health

# Compute Engine
gcloud compute instances describe erlmcp-server --zone=us-central1-a
curl http://<external-ip>/health
```

### Metrics and Monitoring

- **Cloud Monitoring**: https://console.cloud.google.com/monitoring
- **Cloud Logging**: https://console.cloud.google.com/logs
- **Metrics Endpoint**: `:9100/metrics`

## Disaster Recovery

### Backup

```bash
# Backup configuration
gcloud secrets versions access latest --secret=erlmcp-erlang-cookie > backup/cookie.txt

# Backup state
terraform output -json > backup/terraform-state.json
```

### Restore

```bash
# Restore secrets
cat backup/cookie.txt | gcloud secrets versions add erlmcp-erlang-cookie --data-file=-

# Re-deploy
terraform apply
```

## Cost Optimization

### Right-Sizing Resources

| Component | Small | Medium | Large |
|-----------|-------|--------|-------|
| GKE Nodes | e2-standard-2 | e2-standard-4 | e2-standard-8 |
| Memory | 2Gi | 4Gi | 8Gi |
| Max Replicas | 3 | 5 | 10 |

### Cost Controls

- Set budget alerts in GCP Billing
- Use preemptible VMs for non-critical workloads
- Enable auto-scaling with minimum replicas
- Use Cloud Run for dev/test environments

## Security Hardening

### Network Security

```bash
# Restrict API access
gcloud services enable accessapproval.googleapis.com

# Configure VPC Service Controls
gcloud access-context-manager policies create \
  --title "erlmcp Policy" \
  --organization=YOUR_ORG_ID
```

### IAM Best Practices

- Use least privilege IAM roles
- Implement Workload Identity for GKE
- Rotate secrets regularly
- Enable Audit Logs

## Troubleshooting

### Common Issues

**Pods not starting**
```bash
kubectl describe pod -n erlmcp
kubectl logs -n erlmcp deployment/erlmcp --previous
```

**Secret access denied**
```bash
gcloud secrets get-iam-policy erlmcp-erlang-secret
gcloud secrets add-iam-policy-binding erlmcp-erlang-secret \
  --member="serviceAccount:erlmcp@project.iam.gserviceaccount.com" \
  --role="roles/secretmanager.secretAccessor"
```

**Network policies blocking**
```bash
kubectl get networkpolicy -n erlmcp
kubectl describe networkpolicy -n erlmcp
```

## Support

- **Documentation**: https://erlmcp.dev/docs
- **Issues**: https://github.com/banyan-platform/erlmcp/issues
- **Community**: https://discord.gg/erlmcp

## License

Apache License 2.0 - See LICENSE file for details
