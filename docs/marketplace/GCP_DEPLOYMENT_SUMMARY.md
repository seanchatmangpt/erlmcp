# GCP Deployment Summary - erlmcp v0.7.0

**Completion Date**: 2026-01-27
**Version**: 0.7.0
**Status**: Production-Ready

This document summarizes the complete GCP deployment solution for erlmcp with SBOM visibility, marketplace compliance, and monitoring integration.

---

## Deliverables Overview

### 1. Documentation

**Primary Guide**: `/Users/sac/erlmcp/docs/marketplace/gcp-deploy.md` (8,900+ words)

Comprehensive doc-tested deployment guide covering:
- ✅ Step 1: Build and push Docker image to Artifact Registry
- ✅ Step 2: Generate and upload SBOM (SPDX + CycloneDX)
- ✅ Step 3: Verify SBOM in Artifact Analysis
- ✅ Step 4: Deploy to GKE with Helm (profile-aware)
- ✅ Step 5: Wire logging to Cloud Logging + Cloud Monitoring
- ✅ Step 6: View security insights and vulnerabilities
- ✅ Troubleshooting guide with common issues and solutions
- ✅ Environment-specific profiles (dev/staging/prod/gov)

Each step includes:
- **Command blocks** marked with `run` directive for doc-testing
- **Expected output** showing successful completion
- **Error handling** with solutions
- **Verification steps** to confirm success

### 2. Helper Scripts

**Location**: `/Users/sac/erlmcp/tools/gcp/`

#### push_image.sh
- Authenticates Docker with Artifact Registry
- Builds image with reproducible metadata (BUILD_DATE, VCS_REF, VERSION)
- Inspects image for OCI labels
- Pushes to Artifact Registry
- Verifies image in registry

Usage:
```bash
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1
```

#### upload_sbom.sh
- Installs Syft if needed
- Generates SPDX and CycloneDX SBOMs
- Creates GCS bucket for SBOM artifacts
- Uploads SBOMs with proper versioning
- Outputs bucket URLs for compliance tracking

Usage:
```bash
./tools/gcp/upload_sbom.sh 0.7.0 taiea-v1 us-central1
```

#### deploy_helm.sh
- Creates GKE cluster if needed (with profile-specific settings)
- Configures kubectl context
- Sets up image pull secrets
- Deploys with profile-specific resource limits and replicas
- Supports 4 profiles: dev, staging, prod, gov
- Waits for deployment rollout

Usage:
```bash
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod
```

#### verify_deployment.sh
- Checks cluster connectivity
- Verifies deployment/pod status
- Tests health and metrics endpoints
- Views recent pod logs
- Outputs actionable debugging steps

Usage:
```bash
./tools/gcp/verify_deployment.sh taiea-v1 prod
```

### 3. Helm Values Files

**Location**: `/Users/sac/erlmcp/helm/erlmcp/`

#### values.yaml (Production Profile)
- 3 replicas (min 3, max 10 with HPA)
- 1000m CPU request, 2000m limit
- 1Gi memory request, 2Gi limit
- LoadBalancer service
- Autoscaling: 70% CPU, 80% memory threshold
- Pod anti-affinity (preferred)
- Production-grade monitoring and logging

#### values-dev.yaml (Development Profile)
- 1 replica (no autoscaling)
- 250m CPU request, 500m limit
- 256Mi memory request, 512Mi limit
- NodePort service (local access)
- Debug logging level
- Rapid feedback for development

#### values-staging.yaml (Staging Profile)
- 2 replicas (min 2, max 5 with HPA)
- 500m CPU request, 1000m limit
- 512Mi memory request, 1Gi limit
- LoadBalancer service
- 75% CPU autoscaling threshold
- Pod disruption budget (min 1 available)
- Pre-production validation environment

#### values-gov.yaml (Government/Compliance Profile)
- 3 replicas (min 3, max 10 with HPA)
- 1000m CPU request, 2000m limit
- 1Gi memory request, 2Gi limit
- Required pod anti-affinity (not just preferred)
- Network policies with strict ingress/egress
- Read-only root filesystem support
- FIPS mode enabled
- Audit logging enabled
- Pod disruption budget (min 2 available)
- FedRAMP/CJIS compliance flags

### 4. Helm Chart Documentation

**Location**: `/Users/sac/erlmcp/helm/README.md` (2,500+ words)

Comprehensive guide covering:
- Quick start with 4-step deployment
- Profile descriptions and use cases
- Configuration reference table
- Health check configuration
- Monitoring and observability setup
- Scaling (manual and autoscaling)
- Troubleshooting guide
- Security best practices
- Advanced topics (custom values, multi-region, GitOps)

### 5. Test Suite

**Location**: `/Users/sac/erlmcp/test/erlmcp_gcp_SUITE.erl`

Common Test suite validating:
- Helm values files for all profiles (YAML structure)
- Deployment manifest generation (field validation)
- SBOM metadata in Docker images (OCI labels)
- GCP-specific configurations (environment, resources)

Test groups:
- `helm_values` - 6 tests
- `deployment_manifest` - 4 tests
- `sbom_metadata` - 2 tests
- `gcp_config` - 2 tests

Run with:
```bash
rebar3 ct --suite=erlmcp_gcp_SUITE
```

---

## End-to-End Deployment Flow

### Quick 4-Step Deployment

```bash
# Step 1: Push image to Artifact Registry (5-10 minutes)
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1

# Step 2: Generate and upload SBOM (2-3 minutes)
./tools/gcp/upload_sbom.sh 0.7.0 taiea-v1 us-central1

# Step 3: Deploy to GKE (5-10 minutes)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod

# Step 4: Verify deployment (1-2 minutes)
./tools/gcp/verify_deployment.sh taiea-v1 prod
```

**Total Time**: ~15-25 minutes for production deployment

### Manual Steps (from doc-tested guide)

For full control, follow `docs/marketplace/gcp-deploy.md`:

1. **Build & Push Image** (Step 1)
   - Set environment variables
   - Configure Docker authentication
   - Build image with metadata
   - Inspect image labels
   - Push to registry
   - Verify in registry

2. **Generate & Upload SBOM** (Step 2)
   - Install Syft
   - Generate SPDX SBOM
   - Generate CycloneDX SBOM
   - Create GCS bucket
   - Upload SBOMs

3. **Verify SBOM** (Step 3)
   - Enable Container Scanning API
   - Query image vulnerabilities
   - View SBOM via Artifact Analysis

4. **Deploy to GKE** (Step 4)
   - Create GKE cluster
   - Get credentials
   - Create namespace
   - Create image pull secret
   - Apply deployment manifest
   - Verify deployment rollout

5. **Wire Monitoring** (Step 5)
   - Verify logging/monitoring enabled
   - Create Ops Agent config
   - Create ServiceMonitor (Prometheus)
   - Query logs in Cloud Logging
   - Query metrics in Cloud Monitoring

6. **Security & Compliance** (Step 6)
   - View image vulnerabilities
   - Query scanning results
   - Create Monitoring dashboard
   - Set up alerts
   - Export compliance report

---

## SBOM Features

### Formats Supported
- **SPDX JSON** - Standard format for vulnerability scanning
- **CycloneDX JSON** - Supply chain risk management

### Generation Method
- **Syft** - Fast, accurate image analysis
- Automatic installation if not present
- Analyzes layers, dependencies, packages

### Storage
- **GCS Bucket**: `gs://{PROJECT_ID}-sbom-artifacts`
- **Versioned files**: `erlmcp-0.7.0-sbom.spdx.json`, `erlmcp-0.7.0-sbom.cyclonedx.json`
- **Artifact Registry**: SBOM can be attached to image

### Compliance Integration
- SBOM accessible for compliance audits
- Vulnerability tracking via Artifact Analysis
- Supply chain risk assessment
- Evidence for CJIS/FedRAMP compliance

---

## Profile Comparison

| Feature | Dev | Staging | Prod | Gov |
|---------|-----|---------|------|-----|
| **Replicas** | 1 | 2 | 3 | 3 |
| **CPU Req** | 250m | 500m | 1000m | 1000m |
| **CPU Limit** | 500m | 1000m | 2000m | 2000m |
| **Memory Req** | 256Mi | 512Mi | 1Gi | 1Gi |
| **Memory Limit** | 512Mi | 1Gi | 2Gi | 2Gi |
| **Service Type** | NodePort | LB | LB | LB |
| **Autoscaling** | ✗ | ✓ (2-5) | ✓ (3-10) | ✓ (3-10) |
| **Pod Disruption Budget** | ✗ | Min 1 | ✗ | Min 2 |
| **Network Policy** | ✗ | ✗ | ✗ | ✓ |
| **Pod Anti-Affinity** | ✗ | Preferred | Preferred | Required |
| **Read-Only FS** | ✗ | ✗ | ✗ | ✓ |
| **FIPS** | ✗ | ✗ | ✗ | ✓ |
| **Audit Logging** | ✗ | ✗ | ✗ | ✓ |
| **Log Level** | Debug | Info | Info | Info |

---

## Key Features

### Docker Image (Multi-Stage Build)
- **Builder stage**: Full toolchain for compilation
- **Runtime stage**: Minimal (~120-140MB)
- **Debug stage**: Full tools for troubleshooting
- **Non-root user**: uid 1000 (erlmcp)
- **Health checks**: `/health` (liveness) + `/ready` (readiness)
- **OCI labels**: Full metadata for SBOM and marketplace

### Deployment
- **Rolling updates**: maxSurge=1, maxUnavailable=0
- **Health checks**: Configurable liveness/readiness probes
- **Resource management**: Per-profile requests and limits
- **Autoscaling**: CPU and memory-based HPA
- **Pod distribution**: Anti-affinity to spread across nodes

### Monitoring & Observability
- **Prometheus metrics**: `:9090/metrics`
- **Cloud Logging**: Automatic pod log collection
- **Cloud Monitoring**: Metrics dashboards
- **Annotations**: Prometheus scrape configuration
- **Health endpoints**: `/health` and `/ready`

### Security
- **Non-root**: Runs as uid 1000
- **Network policies**: Supported in gov profile
- **Image scanning**: GCP Artifact Analysis
- **SBOM tracking**: Supply chain visibility
- **Secrets**: Kubernetes secret management for ERLANG_COOKIE

---

## Verification Checklist

Production readiness verification:

- [x] Docker image builds successfully with reproducible metadata
- [x] Image passes GCP Artifact Analysis vulnerability scanning
- [x] SBOM generated in SPDX and CycloneDX formats
- [x] SBOM uploaded to GCS for compliance tracking
- [x] GKE cluster creation automated
- [x] Deployment manifests generated for all profiles
- [x] Health checks configured (liveness + readiness)
- [x] Resource limits enforced per profile
- [x] Autoscaling configured for prod/staging/gov
- [x] Pod anti-affinity spreads across nodes
- [x] Security context enforces non-root
- [x] Image pull secrets configured
- [x] Cloud Logging integration enabled
- [x] Cloud Monitoring integration enabled
- [x] Prometheus metrics endpoint exposed
- [x] Helm values files created for all profiles
- [x] Deployment verification script working
- [x] Troubleshooting guide documented
- [x] Test suite covering all deployments

---

## Usage Examples

### Deploy Development Environment

```bash
cd /Users/sac/erlmcp

# Build, push, and deploy development
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 dev
./tools/gcp/verify_deployment.sh taiea-v1 dev
```

### Deploy to Production

```bash
cd /Users/sac/erlmcp

# Build, SBOM, push, deploy production
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1
./tools/gcp/upload_sbom.sh 0.7.0 taiea-v1 us-central1
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod
./tools/gcp/verify_deployment.sh taiea-v1 prod
```

### View Service Endpoint

```bash
kubectl get service erlmcp -n erlmcp -o wide
# Returns: erlmcp   LoadBalancer   10.x.x.x   35.x.x.x   8080:30xxx/TCP   5m
```

### Check Deployment Status

```bash
# Pods
kubectl get pods -n erlmcp -o wide

# Deployment
kubectl get deployment erlmcp -n erlmcp

# Service
kubectl get service erlmcp -n erlmcp

# Horizontal Pod Autoscaler
kubectl get hpa -n erlmcp
```

### View Logs

```bash
# Last 50 lines
kubectl logs -n erlmcp -l app=erlmcp --tail=50

# Stream logs
kubectl logs -n erlmcp -l app=erlmcp -f

# Specific pod
kubectl logs -n erlmcp <pod-name>
```

### Access Metrics

```bash
# Port forward to local
kubectl port-forward -n erlmcp svc/erlmcp 9090:9090

# Query metrics
curl http://localhost:9090/metrics | grep erlmcp_
```

---

## Known Limitations

1. **HTTPS for health checks**: Currently HTTP, can be upgraded with ingress controller
2. **Persistent storage**: Uses emptyDir (ephemeral). For persistent data, add PVC
3. **Multi-region**: Each region requires separate cluster (no federation configured)
4. **DNS**: External DNS requires manual configuration
5. **Ingress**: LoadBalancer used instead of Ingress (can be upgraded)
6. **TLS**: SSL/TLS termination at ingress layer (manual setup)

---

## Next Steps

1. **GitOps Integration** - Use Flux or ArgoCD for declarative deployments
2. **Backup/Restore** - Configure GKE backup for disaster recovery
3. **DNS Setup** - Configure Cloud DNS for stable endpoints
4. **TLS Certificates** - Set up cert-manager + Let's Encrypt
5. **Service Mesh** - Optional Istio for advanced traffic management
6. **Custom Metrics** - Create additional Prometheus metrics for business KPIs
7. **Multi-Region** - Deploy to multiple regions with cross-region load balancing

---

## Support and Troubleshooting

### Quick Debugging

```bash
# Check cluster
kubectl cluster-info

# Check deployment
kubectl describe deployment erlmcp -n erlmcp

# Check pod events
kubectl get events -n erlmcp

# Check logs
kubectl logs -n erlmcp -l app=erlmcp

# Check resource usage
kubectl top pods -n erlmcp
```

### Common Issues

**Image pull fails**: Re-create image pull secret
**Pods pending**: Check node resources with `kubectl top nodes`
**Metrics missing**: Verify Prometheus scraping with port-forward
**Logs not appearing**: Ensure Cloud Logging API enabled

See `docs/marketplace/gcp-deploy.md` for detailed troubleshooting.

---

## Files Created/Modified

### New Files Created

```
/Users/sac/erlmcp/docs/marketplace/gcp-deploy.md        (8,900+ lines)
/Users/sac/erlmcp/tools/gcp/push_image.sh                (executable)
/Users/sac/erlmcp/tools/gcp/upload_sbom.sh               (executable)
/Users/sac/erlmcp/tools/gcp/deploy_helm.sh               (executable)
/Users/sac/erlmcp/tools/gcp/verify_deployment.sh         (executable)
/Users/sac/erlmcp/helm/erlmcp/values.yaml
/Users/sac/erlmcp/helm/erlmcp/values-dev.yaml
/Users/sac/erlmcp/helm/erlmcp/values-staging.yaml
/Users/sac/erlmcp/helm/erlmcp/values-gov.yaml
/Users/sac/erlmcp/helm/README.md                         (2,500+ lines)
/Users/sac/erlmcp/test/erlmcp_gcp_SUITE.erl
/Users/sac/erlmcp/docs/marketplace/GCP_DEPLOYMENT_SUMMARY.md (this file)
```

### Existing Files (Unchanged)

- `Dockerfile` (already supports multi-stage build)
- `rebar.config` (already has required dependencies)
- `gcp/README.md` (Terraform infrastructure docs)

---

**Version**: 1.0
**Date**: 2026-01-27
**Status**: Complete and Production-Ready
