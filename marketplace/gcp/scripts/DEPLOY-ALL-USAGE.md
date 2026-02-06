# deploy-all.sh - Multi-Platform GCP Deployment Guide

## Overview

Automated deployment script for erlmcp across all GCP Marketplace deployment paths:
- **GKE** (Google Kubernetes Engine) - Container orchestration
- **Cloud Run** - Serverless containers
- **GCE** (Google Compute Engine) - VM instances

## WHY

Production deployment deadline requires automated orchestration across all GCP Marketplace deployment paths with comprehensive validation and health checks. This script ensures consistent, repeatable, and verified deployments.

## WHAT

Bash script that:
1. Builds Docker image (DOCKER-ONLY compliant)
2. Pushes to Google Artifact Registry
3. Deploys to GKE, Cloud Run, and/or GCE
4. Runs comprehensive health checks
5. Generates deployment evidence

## HOW

Uses `docker build` for image creation, `kubectl apply` for GKE, `gcloud` for Cloud Run/GCE.

## Quick Start

### Prerequisites

```bash
# Set your GCP project
export PROJECT_ID="your-gcp-project-id"

# Optional: customize deployment
export REGION="us-central1"
export IMAGE_TAG="v3.0.0"
```

### Basic Usage

```bash
# Deploy to all platforms
cd /home/user/erlmcp/marketplace/gcp/scripts
./deploy-all.sh
```

### Selective Deployment

```bash
# Deploy only to Cloud Run
DEPLOY_GKE=false DEPLOY_GCE=false ./deploy-all.sh

# Deploy only to GKE
DEPLOY_CLOUDRUN=false DEPLOY_GCE=false ./deploy-all.sh

# Deploy to Cloud Run and GCE (skip GKE)
DEPLOY_GKE=false ./deploy-all.sh
```

### Parallel Deployment

```bash
# Deploy all platforms in parallel (faster but more resource intensive)
PARALLEL_DEPLOY=true ./deploy-all.sh
```

## Configuration

### Required Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `PROJECT_ID` | Yes | - | GCP project ID |

### Optional Environment Variables

#### General Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `REGION` | `us-central1` | GCP region for deployments |
| `ZONE` | `us-central1-a` | GCP zone for GCE instances |
| `DEPLOY_ID` | `prod-<timestamp>` | Unique deployment identifier |
| `IMAGE_TAG` | `latest` | Docker image tag |

#### Deployment Flags

| Variable | Default | Description |
|----------|---------|-------------|
| `DEPLOY_GKE` | `true` | Deploy to GKE |
| `DEPLOY_CLOUDRUN` | `true` | Deploy to Cloud Run |
| `DEPLOY_GCE` | `true` | Deploy to GCE |
| `PARALLEL_DEPLOY` | `false` | Deploy platforms in parallel |
| `SKIP_HEALTH_CHECK` | `false` | Skip health checks |

#### GKE Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `GKE_CLUSTER_NAME` | `erlmcp-<deploy-id>` | GKE cluster name |
| `GKE_MACHINE_TYPE` | `e2-standard-2` | Node machine type |
| `GKE_NUM_NODES` | `3` | Initial node count |
| `GKE_MIN_NODES` | `1` | Minimum nodes for autoscaling |
| `GKE_MAX_NODES` | `5` | Maximum nodes for autoscaling |

#### Cloud Run Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `CLOUDRUN_SERVICE_NAME` | `erlmcp-<deploy-id>` | Cloud Run service name |
| `CLOUDRUN_CPU` | `1` | CPU allocation |
| `CLOUDRUN_MEMORY` | `512Mi` | Memory allocation |
| `CLOUDRUN_MIN_INSTANCES` | `0` | Minimum instances |
| `CLOUDRUN_MAX_INSTANCES` | `10` | Maximum instances |
| `CLOUDRUN_TIMEOUT` | `300` | Request timeout (seconds) |
| `CLOUDRUN_CONCURRENCY` | `80` | Requests per instance |

#### GCE Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `GCE_INSTANCE_NAME` | `erlmcp-<deploy-id>` | GCE instance name |
| `GCE_MACHINE_TYPE` | `e2-medium` | Machine type |
| `GCE_IMAGE_FAMILY` | `cos-stable` | Container-Optimized OS image |
| `GCE_BOOT_DISK_SIZE` | `20GB` | Boot disk size |

#### Timeouts

| Variable | Default | Description |
|----------|---------|-------------|
| `BUILD_TIMEOUT` | `1800` | Docker build timeout (seconds) |
| `DEPLOY_TIMEOUT` | `1800` | Deployment timeout (seconds) |
| `HEALTH_CHECK_TIMEOUT` | `600` | Health check timeout (seconds) |
| `HEALTH_CHECK_INTERVAL` | `10` | Health check retry interval (seconds) |

## Usage Examples

### Production Deployment

```bash
export PROJECT_ID="erlmcp-prod"
export REGION="us-central1"
export IMAGE_TAG="v3.0.0"
export DEPLOY_ID="prod-release-v3"

# Sequential deployment with full validation
./deploy-all.sh
```

### Staging Deployment

```bash
export PROJECT_ID="erlmcp-staging"
export REGION="us-west1"
export IMAGE_TAG="staging-latest"
export GKE_MIN_NODES=1
export GKE_MAX_NODES=3
export CLOUDRUN_MAX_INSTANCES=5

# Parallel deployment for faster iteration
PARALLEL_DEPLOY=true ./deploy-all.sh
```

### Development/Testing

```bash
export PROJECT_ID="erlmcp-dev"
export IMAGE_TAG="dev-$(git rev-parse --short HEAD)"
export GKE_NUM_NODES=1
export CLOUDRUN_MIN_INSTANCES=0
export CLOUDRUN_MAX_INSTANCES=2

# Deploy only to Cloud Run for quick testing
DEPLOY_GKE=false DEPLOY_GCE=false ./deploy-all.sh
```

### Multi-Region Deployment

```bash
# Deploy to multiple regions
for region in us-central1 europe-west1 asia-east1; do
  export REGION="$region"
  export DEPLOY_ID="prod-${region}-$(date +%Y%m%d)"
  ./deploy-all.sh
done
```

### Cost-Optimized Deployment

```bash
export PROJECT_ID="erlmcp-prod"
export GKE_MACHINE_TYPE="e2-small"
export GKE_NUM_NODES=1
export CLOUDRUN_CPU=1
export CLOUDRUN_MEMORY="256Mi"
export GCE_MACHINE_TYPE="e2-micro"

# Deploy with minimal resources
./deploy-all.sh
```

## Health Checks

The script performs comprehensive health checks for each platform:

### GKE Health Check
- Waits for LoadBalancer IP assignment
- HTTP GET to `http://<loadbalancer-ip>/health`
- Validates response status 200

### Cloud Run Health Check
- Retrieves service URL
- HTTP GET to `<service-url>/health`
- Validates response status 200

### GCE Health Check
- Waits for instance startup (30s + checks)
- HTTP GET to `http://<instance-ip>:8080/health`
- Validates response status 200

Health checks retry with exponential backoff until `HEALTH_CHECK_TIMEOUT` is reached.

## Evidence Collection

Deployment evidence is saved to:
```
marketplace/gcp/test-evidence/deploy-all-<deploy-id>/
├── deployment-summary.json
├── api-enablement.log
├── docker-build.log
├── docker-push.log
├── image-digest.txt
├── gke/
│   ├── cluster-create.log
│   ├── helm-deploy.log or kubectl-apply.log
│   ├── deployment.yaml
│   ├── resources.txt
│   ├── loadbalancer-ip.txt
│   └── health-check-*.json
├── cloudrun/
│   ├── deploy.log
│   ├── service-url.txt
│   ├── service-describe.yaml
│   └── health-check-*.json
└── gce/
    ├── startup-script.sh
    ├── instance-create.log
    ├── instance-ip.txt
    ├── instance-describe.yaml
    └── health-check-*.json
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Deployment successful |
| 1 | Deployment or health check failed |

## Deployment Flow

### Sequential Mode (default)

```
1. Prerequisites check
2. Enable GCP APIs
3. Build Docker image
4. Push to Artifact Registry
5. Deploy Cloud Run
6. Deploy GCE
7. Deploy GKE
8. Health check Cloud Run
9. Health check GCE
10. Health check GKE
11. Generate summary
```

### Parallel Mode

```
1. Prerequisites check
2. Enable GCP APIs
3. Build Docker image
4. Push to Artifact Registry
5. Deploy Cloud Run, GCE, GKE (parallel)
6. Health check all (parallel)
7. Generate summary
```

## DOCKER-ONLY Constitution Compliance

This script adheres to the DOCKER-ONLY constitution:

✅ **Compliant Operations:**
- `docker build` - All image builds use Docker
- `docker push` - Registry operations via Docker
- `docker run` - GCE instances run containers via Docker
- `kubectl apply` - Explicitly allowed for GKE
- `gcloud` - Cloud infrastructure management (deployment, not Erlang compilation)

❌ **Forbidden Operations (NOT PRESENT):**
- No direct `rebar3` execution
- No direct `erl` execution
- No `make` for Erlang compilation
- No `ct_run` for testing
- All Erlang compilation happens inside Docker build

## Troubleshooting

### Prerequisites Failed

```bash
# Authenticate with gcloud
gcloud auth login

# Set default project
gcloud config set project $PROJECT_ID

# Verify authentication
gcloud auth list
```

### Docker Build Failed

```bash
# Check Dockerfile
ls -la /home/user/erlmcp/Dockerfile

# Verify Docker is running
docker info

# Check available disk space
df -h
```

### GKE Deployment Failed

```bash
# Check cluster status
gcloud container clusters describe $GKE_CLUSTER_NAME --region=$REGION

# View kubectl context
kubectl config current-context

# Check node status
kubectl get nodes

# View pod logs
kubectl logs -n erlmcp -l app=erlmcp
```

### Cloud Run Deployment Failed

```bash
# Check service status
gcloud run services describe $CLOUDRUN_SERVICE_NAME --region=$REGION

# View logs
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=$CLOUDRUN_SERVICE_NAME" --limit=50 --format=json
```

### GCE Deployment Failed

```bash
# Check instance status
gcloud compute instances describe $GCE_INSTANCE_NAME --zone=$ZONE

# View serial port output
gcloud compute instances get-serial-port-output $GCE_INSTANCE_NAME --zone=$ZONE

# SSH to instance
gcloud compute ssh $GCE_INSTANCE_NAME --zone=$ZONE

# Check Docker on instance
gcloud compute ssh $GCE_INSTANCE_NAME --zone=$ZONE --command="docker ps"
```

### Health Check Failed

```bash
# Manual health check - GKE
LOADBALANCER_IP=$(kubectl get svc erlmcp -n erlmcp -o jsonpath='{.status.loadBalancer.ingress[0].ip}')
curl -v http://$LOADBALANCER_IP/health

# Manual health check - Cloud Run
SERVICE_URL=$(gcloud run services describe $CLOUDRUN_SERVICE_NAME --region=$REGION --format='value(status.url)')
curl -v $SERVICE_URL/health

# Manual health check - GCE
INSTANCE_IP=$(gcloud compute instances describe $GCE_INSTANCE_NAME --zone=$ZONE --format='get(networkInterfaces[0].accessConfigs[0].natIP)')
curl -v http://$INSTANCE_IP:8080/health
```

## Cleanup

To remove deployed resources:

```bash
# GKE
gcloud container clusters delete $GKE_CLUSTER_NAME --region=$REGION --quiet

# Cloud Run
gcloud run services delete $CLOUDRUN_SERVICE_NAME --region=$REGION --quiet

# GCE
gcloud compute instances delete $GCE_INSTANCE_NAME --zone=$ZONE --quiet
gcloud compute firewall-rules delete erlmcp-allow-http --quiet
```

## Security Considerations

- All images are built from source (no untrusted base images)
- Artifact Registry authentication required
- Cloud Run can be configured with authentication
- GCE firewall rules created for port 8080
- GKE workload identity enabled
- Startup scripts use official Docker installation
- No privileged containers
- Secrets externalized (not in startup scripts)

## Performance

Typical deployment times:

| Platform | Sequential | Parallel |
|----------|-----------|----------|
| Cloud Run | 2-5 min | 2-5 min |
| GCE | 5-10 min | 5-10 min |
| GKE | 15-25 min | 15-25 min |
| **Total** | **22-40 min** | **15-25 min** |

*Times include image build, push, deployment, and health checks*

## Support

Evidence location: `marketplace/gcp/test-evidence/deploy-all-<deploy-id>/`

For issues, check:
1. Deployment logs in evidence directory
2. GCP Cloud Console for service status
3. kubectl/gcloud CLI for real-time status

## License

Part of erlmcp v3 - Erlang/OTP MCP SDK
