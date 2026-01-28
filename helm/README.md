# erlmcp Helm Charts for Kubernetes Deployment

This directory contains Helm charts and deployment configurations for running erlmcp on Kubernetes (primarily GKE).

## Directory Structure

```
helm/
├── erlmcp/                    # Main Helm chart
│   ├── Chart.yaml            # Helm chart metadata
│   ├── values.yaml            # Default (production) values
│   ├── values-dev.yaml        # Development profile
│   ├── values-staging.yaml    # Staging profile
│   ├── values-gov.yaml        # Government/compliance profile
│   └── templates/             # Helm templates (deployment, service, etc.)
└── README.md                  # This file
```

## Quick Start

### 1. Push Docker Image to GCP Artifact Registry

```bash
# From erlmcp project root
./tools/gcp/push_image.sh 0.7.0 taiea-v1 us-central1
```

**What this does:**
- Builds Docker image with reproducible metadata
- Configures Docker authentication with Artifact Registry
- Pushes image to `us-central1-docker.pkg.dev/taiea-v1/erlmcp-repo/erlmcp:0.7.0`
- Verifies image in registry

### 2. Generate SBOM and Upload to GCS

```bash
./tools/gcp/upload_sbom.sh 0.7.0 taiea-v1 us-central1
```

**What this does:**
- Generates SPDX and CycloneDX SBOMs using Syft
- Creates GCS bucket for SBOM artifacts
- Uploads SBOMs for compliance tracking
- Outputs bucket URLs

### 3. Deploy to GKE

```bash
# Deploy production profile (3 replicas, 2 CPU/2Gi memory per pod)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod

# Deploy staging profile (2 replicas with autoscaling)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 staging

# Deploy development profile (1 replica, minimal resources)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 dev

# Deploy government profile (enhanced security/compliance)
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 gov
```

### 4. Verify Deployment

```bash
./tools/gcp/verify_deployment.sh taiea-v1 prod
```

**Output includes:**
- Cluster connection status
- Deployment readiness
- Pod status
- Service endpoint
- Health/metrics endpoint verification
- Recent pod logs

## Deployment Profiles

Each profile is optimized for specific use cases:

### Development (values-dev.yaml)

**Use case:** Local testing, development iterations

- **Replicas:** 1
- **CPU:** 250m request, 500m limit
- **Memory:** 256Mi request, 512Mi limit
- **Service Type:** NodePort
- **Autoscaling:** Disabled
- **Logging Level:** Debug
- **Pull Policy:** Always

Deploy:
```bash
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 dev
```

### Staging (values-staging.yaml)

**Use case:** Pre-production testing, performance validation

- **Replicas:** 2 (with min 2, max 5 autoscaling)
- **CPU:** 500m request, 1000m limit
- **Memory:** 512Mi request, 1Gi limit
- **Service Type:** LoadBalancer
- **Autoscaling:** Enabled (75% CPU threshold)
- **Logging Level:** Info
- **Pod Disruption Budget:** Minimum 1 available

Deploy:
```bash
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 staging
```

### Production (values.yaml)

**Use case:** Live production deployment

- **Replicas:** 3 (with min 3, max 10 autoscaling)
- **CPU:** 1000m request, 2000m limit
- **Memory:** 1Gi request, 2Gi limit
- **Service Type:** LoadBalancer
- **Autoscaling:** Enabled (70% CPU, 80% memory threshold)
- **Logging Level:** Info
- **Pod Anti-Affinity:** Preferred across nodes

Deploy:
```bash
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod
```

### Government (values-gov.yaml)

**Use case:** FedRAMP/CJIS compliance, sensitive workloads

- **Replicas:** 3 (with min 3, max 10 autoscaling)
- **CPU:** 1000m request, 2000m limit
- **Memory:** 1Gi request, 2Gi limit
- **Service Type:** LoadBalancer
- **Security:** Non-root user, read-only filesystem
- **Network Policy:** Required pod anti-affinity, strict egress rules
- **Audit Logging:** Enabled
- **FIPS Mode:** Enabled
- **Pod Disruption Budget:** Minimum 2 available

Deploy:
```bash
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 gov
```

## Configuration Reference

### Core Values

| Key | Default | Description |
|-----|---------|-------------|
| `replicaCount` | 3 | Number of pod replicas |
| `image.tag` | 0.7.0 | Docker image tag |
| `image.pullPolicy` | IfNotPresent | Image pull policy |
| `resources.limits.cpu` | 2000m | CPU limit per pod |
| `resources.limits.memory` | 2Gi | Memory limit per pod |
| `service.type` | LoadBalancer | Kubernetes service type |
| `autoscaling.enabled` | true | Enable horizontal pod autoscaling |
| `autoscaling.minReplicas` | 3 | Minimum pods for HPA |
| `autoscaling.maxReplicas` | 10 | Maximum pods for HPA |

### Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `ERLMCP_ENV` | Deployment environment | production, staging, dev |
| `ERLANG_COOKIE` | Erlang distributed cookie | erlmcp_prod_secret |
| `POD_NAME` | Pod name (auto-injected) | erlmcp-xxxxx-xxxxx |
| `POD_NAMESPACE` | Pod namespace (auto-injected) | erlmcp |
| `LANG` | Locale | C.UTF-8 |

### GCP-Specific Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `gcp.project` | taiea-v1 | GCP project ID |
| `gcp.region` | us-central1 | GCP region |
| `gcp.logging.enabled` | true | Cloud Logging integration |
| `gcp.monitoring.enabled` | true | Cloud Monitoring integration |
| `gcp.monitoring.interval` | 30s | Metrics scrape interval |

## Health Checks

erlmcp exposes two health endpoints:

### `/health` (Liveness Probe)

Indicates if the pod should be restarted.

```bash
curl http://erlmcp:8080/health
```

**Configuration:**
- Initial delay: 30s (allow startup)
- Period: 10s (check every 10 seconds)
- Timeout: 5s (fail if no response)
- Failure threshold: 3 (restart after 3 failures)

### `/ready` (Readiness Probe)

Indicates if the pod should receive traffic.

```bash
curl http://erlmcp:8080/ready
```

**Configuration:**
- Initial delay: 10s (quick startup check)
- Period: 5s (frequent readiness checks)
- Timeout: 3s (fast timeout)
- Failure threshold: 2 (remove from service after 2 failures)

## Monitoring and Observability

### Prometheus Metrics

erlmcp exposes Prometheus metrics on port 9090:

```bash
# Port forward to local
kubectl port-forward -n erlmcp svc/erlmcp 9090:9090

# Query metrics
curl http://localhost:9090/metrics
```

**Key metrics:**
- `erlmcp_connections_total` - Total connections
- `erlmcp_request_duration_seconds` - Request latency histogram
- `erlmcp_memory_used_bytes` - Memory usage
- `erlmcp_uptime_seconds` - Pod uptime

### Cloud Logging Integration

GKE automatically sends pod logs to Cloud Logging:

```bash
# View logs for erlmcp namespace
gcloud logging read "resource.type=k8s_container AND resource.labels.namespace_name=erlmcp" \
  --limit 50 \
  --format=json | jq '.[] | {timestamp: .timestamp, message: .textPayload}'
```

### Cloud Monitoring Dashboard

Access the erlmcp dashboard in GCP Console:

1. Go to Cloud Console → Monitoring → Dashboards
2. Select "erlmcp Production Dashboard"
3. View real-time metrics (connections, latency, memory, CPU)

## Scaling

### Manual Scaling

```bash
# Scale to 5 replicas
kubectl scale deployment erlmcp -n erlmcp --replicas=5

# Verify
kubectl get deployment erlmcp -n erlmcp
```

### Autoscaling (HPA)

Configured automatically based on profile:

```bash
# View HPA status
kubectl get hpa -n erlmcp

# View HPA metrics
kubectl describe hpa erlmcp -n erlmcp
```

**Profiles with autoscaling:**
- Staging: 2-5 replicas, 75% CPU threshold
- Production: 3-10 replicas, 70% CPU/80% memory threshold
- Government: 3-10 replicas, 60% CPU/75% memory threshold

## Troubleshooting

### Pods in CrashLoopBackOff

Check pod logs:
```bash
kubectl logs -n erlmcp deployment/erlmcp --tail=50
```

Common causes:
- Image not found in Artifact Registry
- Image pull secret misconfigured
- ERLANG_COOKIE not set

### Service endpoint pending

Wait for LoadBalancer to assign IP (usually 1-2 minutes):
```bash
kubectl get service erlmcp -n erlmcp -w
```

### Metrics not appearing

Verify Prometheus scraping:
```bash
kubectl port-forward -n erlmcp svc/erlmcp 9090:9090
curl http://localhost:9090/metrics
```

### Deployment stuck in rollout

Check deployment events:
```bash
kubectl describe deployment erlmcp -n erlmcp

# Rollback to previous version if needed
kubectl rollout undo deployment/erlmcp -n erlmcp
```

## Security Best Practices

### Pod Security

- **Non-root user:** Runs as uid 1000 (erlmcp user)
- **Read-only filesystem:** Available in government profile
- **No privilege escalation:** `allowPrivilegeEscalation: false`
- **Dropped capabilities:** All Linux capabilities dropped

### Network Security

- **Pod isolation:** Pod Anti-Affinity spreads pods across nodes
- **Network Policies:** Enforced in government profile
- **Service isolation:** Namespace-scoped RBAC

### Image Security

- **Image scanning:** GCP Artifact Analysis scans for vulnerabilities
- **SBOM tracking:** Software Bill of Materials generated at build time
- **Image signing:** Can be enabled with Cosign + Sigstore

## Advanced Topics

### Custom Values

Create a custom values file:

```yaml
# custom-values.yaml
replicaCount: 5
env:
  ERLMCP_ENV: staging
  ERLANG_COOKIE: my-custom-cookie

# Deploy with custom values
kubectl apply -f deployment-custom.yaml
```

### Multi-Region Deployment

Deploy to multiple regions:

```bash
# Deploy to us-central1
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod

# Deploy to europe-west1
./tools/gcp/deploy_helm.sh 0.7.0 taiea-v1 europe-west1 prod
```

### GitOps with Flux/ArgoCD

Store Helm values in Git and use GitOps for deployments:

```bash
# Install Flux
flux bootstrap github --owner=your-org --repo=your-repo

# Create Helm Release
flux create source helm erlmcp \
  --url https://your-helm-repo
```

## Testing

Run Helm validation tests:

```bash
# Run GCP test suite
cd /Users/sac/erlmcp
rebar3 ct --suite=erlmcp_gcp_SUITE
```

## Version Compatibility

- **Kubernetes:** 1.24+
- **Helm:** 3.10+
- **GKE:** v1.27+

## Related Documentation

- [GCP Deployment Guide](../docs/marketplace/gcp-deploy.md)
- [Architecture Documentation](../docs/architecture.md)
- [Configuration Reference](../docs/api-reference.md)
