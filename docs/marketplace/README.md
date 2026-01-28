# erlmcp Marketplace Deployment Documentation

This directory contains production-ready deployment documentation for erlmcp on Google Cloud Platform with SBOM visibility, compliance tracking, and marketplace readiness.

## Quick Navigation

### For First-Time Deployment

1. **Start Here**: [GCP Deployment Guide](gcp-deploy.md) (8,900+ lines)
   - Complete step-by-step instructions
   - Doc-tested with expected outputs
   - Covers all 6 deployment steps
   - Troubleshooting guide included

2. **Use Automated Scripts**: 
   ```bash
   ./tools/gcp/push_image.sh       # Push to Artifact Registry
   ./tools/gcp/upload_sbom.sh      # Generate SBOM
   ./tools/gcp/deploy_helm.sh      # Deploy to GKE
   ./tools/gcp/verify_deployment.sh # Verify health
   ```

### For Ongoing Operations

- **Helm Chart Reference**: [helm/README.md](../helm/README.md)
  - Configuration options
  - Profile management
  - Scaling procedures
  - Troubleshooting

### For Project Overview

- **Delivery Summary**: [GCP_DEPLOYMENT_SUMMARY.md](GCP_DEPLOYMENT_SUMMARY.md)
  - What was delivered
  - Architecture overview
  - SBOM features
  - Verification checklist

- **Delivery Manifest**: [DELIVERY_MANIFEST.md](DELIVERY_MANIFEST.md)
  - Complete file inventory
  - Test coverage details
  - SBOM specifications
  - Known limitations

## Documents at a Glance

| Document | Size | Purpose | Audience |
|----------|------|---------|----------|
| [gcp-deploy.md](gcp-deploy.md) | 8,900 lines | Step-by-step deployment guide with doc-tests | DevOps/SRE |
| [GCP_DEPLOYMENT_SUMMARY.md](GCP_DEPLOYMENT_SUMMARY.md) | 800 lines | High-level overview and verification | Managers/Leads |
| [DELIVERY_MANIFEST.md](DELIVERY_MANIFEST.md) | 1,500 lines | Detailed artifact inventory | Architects |
| [helm/README.md](../helm/README.md) | 2,500 lines | Helm configuration reference | Operators |

## Deployment Profiles

Choose the profile that matches your environment:

### Development (`values-dev.yaml`)
- 1 replica (no autoscaling)
- 250m CPU, 256Mi memory
- NodePort service
- Debug logging
- Fast iteration

### Staging (`values-staging.yaml`)
- 2-5 replicas (autoscaling)
- 500m CPU, 512Mi memory
- LoadBalancer service
- Pod disruption budget
- Pre-production validation

### Production (`values.yaml`)
- 3-10 replicas (autoscaling)
- 1000m CPU, 1Gi memory
- LoadBalancer service
- Production-grade monitoring
- 99.99% uptime SLA

### Government (`values-gov.yaml`)
- 3-10 replicas (autoscaling)
- Network policies enabled
- FIPS mode
- Audit logging
- FedRAMP/CJIS compliance

## Quick Start (4 Steps)

```bash
# 1. Push image to Artifact Registry (10 min)
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1

# 2. Generate SBOM (3 min)
./tools/gcp/upload_sbom.sh 0.7.0 taiea-v1 us-central1

# 3. Deploy to GKE (10 min)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod

# 4. Verify health (2 min)
./tools/gcp/verify_deployment.sh taiea-v1 prod
```

**Total Time**: ~25 minutes to production deployment

## SBOM Features

### Automatic Generation
- SPDX JSON format (vulnerability scanning)
- CycloneDX JSON format (supply chain risk)
- Syft-based analysis

### Storage & Compliance
- Uploaded to GCS: `gs://{PROJECT_ID}-sbom-artifacts/`
- Retained for audit trail
- Accessible for compliance verification

### Supply Chain Visibility
- Track dependencies across versions
- Identify known vulnerabilities
- Support for FedRAMP/CJIS compliance
- Third-party audit evidence

## Monitoring & Observability

### Metrics (Prometheus)
- Endpoint: `:9090/metrics`
- Connection tracking
- Request latency histogram
- Memory/CPU usage

### Logs (Cloud Logging)
- Automatic pod log collection
- Namespace: `erlmcp`
- Query: `resource.type=k8s_container AND resource.labels.namespace_name=erlmcp`

### Dashboard (Cloud Monitoring)
- Pre-configured dashboard in GCP Console
- Real-time metrics visualization
- Automated alert configuration

## Testing

Run deployment validation tests:

```bash
rebar3 ct --suite=erlmcp_gcp_SUITE
```

Tests cover:
- Helm values validation (6 tests)
- Deployment manifest structure (4 tests)
- SBOM metadata (2 tests)
- GCP configuration (2 tests)

## Troubleshooting

Common issues and solutions:

**Pod in CrashLoopBackOff**
```bash
kubectl logs -n erlmcp -l app=erlmcp --tail=50
```

**Image pull fails**
```bash
# Re-create image pull secret
kubectl delete secret gcr-secret -n erlmcp
kubectl create secret docker-registry gcr-secret \
  --docker-server=us-central1-docker.pkg.dev \
  --docker-username=_json_key \
  --docker-password="$(gcloud auth print-access-token)" \
  -n erlmcp
```

**Service endpoint pending**
```bash
# Wait for LoadBalancer assignment (1-2 minutes)
kubectl get service erlmcp -n erlmcp -w
```

For more troubleshooting, see [gcp-deploy.md](gcp-deploy.md) "Troubleshooting" section.

## Infrastructure Setup

Prerequisites before deployment:

1. **GCP Project**: With billing enabled
2. **Terraform**: Infrastructure provisioning (see `gcp/README.md`)
3. **gcloud CLI**: Authentication and cluster management
4. **kubectl**: Kubernetes cluster access
5. **Docker**: Local image building
6. **Helm**: (optional) Template management

See `gcp/README.md` for Terraform infrastructure setup.

## Security & Compliance

### Image Security
- Non-root user (uid 1000)
- Minimal Alpine base
- Vulnerability scanning in Artifact Registry
- SBOM for supply chain tracking

### Deployment Security
- Network policies (government profile)
- Pod security context
- Resource limits enforced
- Health checks configured

### Compliance Support
- FedRAMP (government profile)
- CJIS (with audit logging)
- SOC 2 (monitoring/logging)
- Supply chain visibility (SBOM)

## Performance

### Resource Allocation by Profile

| Profile | CPU Request | CPU Limit | Mem Request | Mem Limit |
|---------|-------------|-----------|-------------|-----------|
| dev | 250m | 500m | 256Mi | 512Mi |
| staging | 500m | 1000m | 512Mi | 1Gi |
| prod | 1000m | 2000m | 1Gi | 2Gi |
| gov | 1000m | 2000m | 1Gi | 2Gi |

### Autoscaling (HPA)

Configured per profile:
- **Staging**: 2-5 replicas (75% CPU threshold)
- **Production**: 3-10 replicas (70% CPU, 80% memory threshold)
- **Government**: 3-10 replicas (60% CPU, 75% memory threshold)

## Support & Resources

### Documentation
- [GCP Deployment Guide](gcp-deploy.md) - Complete walkthrough
- [Helm Documentation](../helm/README.md) - Configuration reference
- [Architecture Docs](../architecture.md) - System design
- [API Reference](../api-reference.md) - erlmcp API

### External Resources
- [GCP Artifact Registry](https://cloud.google.com/artifact-registry/docs)
- [GKE Deployment Guide](https://cloud.google.com/kubernetes-engine/docs/deploy-app)
- [SPDX Standard](https://spdx.dev/)
- [CycloneDX Standard](https://cyclonedx.org/)

## File Structure

```
docs/marketplace/
├── README.md (this file)
├── gcp-deploy.md (main guide, 8,900 lines)
├── GCP_DEPLOYMENT_SUMMARY.md (overview)
├── DELIVERY_MANIFEST.md (inventory)
└── EVIDENCE_BUNDLE_v1.3.0.md (compliance evidence)

helm/
├── README.md (Helm reference)
├── erlmcp/
│   ├── values.yaml (prod profile)
│   ├── values-dev.yaml
│   ├── values-staging.yaml
│   └── values-gov.yaml

tools/gcp/
├── push_image.sh (build & push)
├── upload_sbom.sh (SBOM generation)
├── deploy_helm.sh (deployment)
└── verify_deployment.sh (health check)

test/
└── erlmcp_gcp_SUITE.erl (validation tests)
```

## Next Steps

1. **First deployment**: Follow [gcp-deploy.md](gcp-deploy.md) Step 1-4
2. **Setup monitoring**: Follow [gcp-deploy.md](gcp-deploy.md) Step 5
3. **Verify security**: Follow [gcp-deploy.md](gcp-deploy.md) Step 6
4. **Automated deployment**: Use helper scripts in `tools/gcp/`
5. **GitOps setup**: Configure Flux or ArgoCD for continuous deployment

## Version Information

- **erlmcp Version**: 0.7.0
- **Kubernetes**: 1.24+
- **Helm**: 3.10+
- **GKE**: v1.27+
- **Documentation Version**: 1.0
- **Date**: 2026-01-27

---

**Status**: ✅ Production-Ready

For support, see the troubleshooting sections in relevant documents.
