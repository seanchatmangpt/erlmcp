# erlmcp Deployment Index

Complete navigation guide for Docker and Kubernetes deployment files.

## Quick Navigation

### Start Here
- **New to erlmcp deployment?** → Read [`DOCKER_K8S_README.md`](./DOCKER_K8S_README.md)
- **Production deployment?** → Use [`docs/DEPLOYMENT_CHECKLIST.md`](./docs/DEPLOYMENT_CHECKLIST.md)
- **Troubleshooting?** → See [`docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting`](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting)

---

## File Organization

### Docker Configuration

```
Root Directory:
├── Dockerfile                      Main production image
├── Dockerfile.dev                  Development image (with tools)
├── docker-compose.yml              Full stack configuration
└── .dockerignore                   Build optimization

docker/ Directory:
├── docker-compose.yml              Additional Compose configs
├── Dockerfile.production           Alternative production build
├── prometheus.yml                  Prometheus configuration
├── otel-collector-config.yaml      OpenTelemetry collector
└── grafana-datasources.yml         Grafana datasources
```

### Kubernetes Manifests

```
k8s/ Directory:
├── namespace.yaml                  [9 lines] Namespace definition
├── deployment.yaml                 [275 lines] Deployment + Services + ConfigMap + PVC
├── statefulset.yaml                [261 lines] StatefulSet for clustering
├── service.yaml                    [104 lines] All service types
├── configmap.yaml                  [201 lines] Configuration files
├── secret.yaml.template            [52 lines] Secrets template (DO NOT COMMIT)
├── rbac.yaml                       [76 lines] ServiceAccount, Role, RoleBinding
├── network-policy.yaml             [149 lines] Network policies
├── hpa.yaml                        [45 lines] Horizontal Pod Autoscaler
├── pdb.yaml                        [22 lines] Pod Disruption Budgets
├── ingress.yaml                    [45 lines] Ingress routing
├── backup-pvc.yaml                 [132 lines] Backup/restore configuration
└── monitoring.yaml                 [163 lines] Prometheus/Grafana monitoring
```

### Documentation

```
docs/ Directory:
├── DOCKER_KUBERNETES_SETUP.md      [17KB] Complete operational guide
│   ├── Docker Setup                Docker build and runtime
│   ├── Docker Compose              Full stack configuration
│   ├── Kubernetes Deployment       K8s manifests and operations
│   ├── Production Best Practices   Design patterns
│   ├── Monitoring and Observability Prometheus, Grafana, OTEL
│   ├── Security                    RBAC, network policies, secrets
│   └── Troubleshooting             Common issues and solutions
│
└── DEPLOYMENT_CHECKLIST.md         [10KB] Production deployment checklist
    ├── Pre-Deployment Verification Code quality, docs, versions
    ├── Docker Image Preparation    Build, test, security
    ├── Kubernetes Configuration    Manifests, secrets, RBAC
    ├── Cluster Preparation         Resources, add-ons
    ├── Pre-Launch Validation       Health, connectivity
    ├── Testing and Validation      Functional, performance, security
    ├── Monitoring and Alerting     Prometheus, alerts, logging
    ├── Backup and Disaster Recovery Data backup, restore, failover
    ├── Operational Readiness       Docs, training, monitoring
    └── Sign-Off                    Approvals and handoff

Root Directory:
├── DOCKER_K8S_README.md            [11KB] Quick reference guide
├── DOCKER_KUBERNETES_DEPLOYMENT_SUMMARY.md  Completion summary
└── DEPLOYMENT_INDEX.md             This file
```

### Scripts

```
scripts/ Directory:
├── docker-validation.sh            [390 lines] Docker image validation
│   ├── Checks                      Prerequisites, build, size, security
│   ├── Tests                       Container startup, health, ports
│   └── Validation                  Volumes, security scanning, reports
│
└── k8s-validation.sh               [416 lines] Kubernetes deployment validation
    ├── Checks                      Cluster, namespace, manifests
    ├── Tests                       RBAC, pods, services, networking
    └── Validation                  Monitoring, connectivity, reports
```

---

## Reading Guide

### Path 1: Quick Start (15 minutes)
1. Read [`DOCKER_K8S_README.md`](./DOCKER_K8S_README.md) overview
2. Review [`k8s/` directory listing](./k8s/)
3. Run validation script: `./scripts/docker-validation.sh`
4. Run validation script: `./scripts/k8s-validation.sh`

### Path 2: Complete Setup (2-3 hours)
1. Read [`docs/DOCKER_KUBERNETES_SETUP.md`](./docs/DOCKER_KUBERNETES_SETUP.md)
   - Docker section (30 min)
   - Kubernetes section (1 hour)
   - Monitoring section (30 min)
   - Troubleshooting (30 min)
2. Review all K8s manifests (30 min)
3. Practice with Docker Compose (30 min)
4. Test Kubernetes deployment (30 min)

### Path 3: Production Deployment (4-6 hours)
1. Review [`docs/DEPLOYMENT_CHECKLIST.md`](./docs/DEPLOYMENT_CHECKLIST.md)
2. Execute pre-deployment checklist (1 hour)
3. Prepare Docker image and push to registry (1 hour)
4. Configure Kubernetes secrets and manifests (1 hour)
5. Deploy to staging cluster (1 hour)
6. Test and validate (1-2 hours)
7. Obtain approvals and deploy to production (1 hour)

### Path 4: Operations (ongoing)
1. Bookmark [`docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting`](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting)
2. Keep [`docs/DEPLOYMENT_CHECKLIST.md`](./docs/DEPLOYMENT_CHECKLIST.md) handy for operational tasks
3. Use validation scripts for health checks
4. Reference common operations in [`DOCKER_K8S_README.md`](./DOCKER_K8S_README.md)

---

## File Reference Table

### Docker Files

| File | Size | Purpose | Key Content |
|------|------|---------|-------------|
| Dockerfile | ~200 LOC | Production image | Multi-stage, Alpine, health check |
| Dockerfile.dev | ~50 LOC | Development | Build tools, debug utilities |
| docker-compose.yml | ~210 LOC | Orchestration | Services, networking, volumes |
| .dockerignore | ~25 LOC | Build optimization | Exclude patterns |

### Kubernetes Files

| File | Lines | Replicas | Resource | Resource Limits |
|------|-------|----------|----------|-----------------|
| namespace.yaml | 9 | N/A | Namespace | N/A |
| deployment.yaml | 275 | 3 | Deployment | CPU: 2000m, Memory: 2Gi |
| statefulset.yaml | 261 | 3 | StatefulSet | CPU: 2000m, Memory: 2Gi |
| service.yaml | 104 | N/A | Services (4 types) | N/A |
| configmap.yaml | 201 | N/A | Configuration | N/A |
| secret.yaml.template | 52 | N/A | Secrets | N/A |
| rbac.yaml | 76 | N/A | RBAC | N/A |
| network-policy.yaml | 149 | N/A | Network Policies | N/A |
| hpa.yaml | 45 | 3-10 | HPA | 70% CPU, 80% Memory |
| pdb.yaml | 22 | N/A | PDB | 2 minimum available |
| ingress.yaml | 45 | N/A | Ingress | N/A |
| backup-pvc.yaml | 132 | N/A | Backup Job | Daily CronJob |
| monitoring.yaml | 163 | N/A | Monitoring | ServiceMonitor, PrometheusRule |

### Documentation Files

| File | Size | Sections | Use Case |
|------|------|----------|----------|
| DOCKER_KUBERNETES_SETUP.md | 17KB | 7 major | Complete guide, reference |
| DEPLOYMENT_CHECKLIST.md | 10KB | 9 major | Production deployment |
| DOCKER_K8S_README.md | 11KB | 6 major | Quick reference, overview |
| DEPLOYMENT_INDEX.md | This | Navigation | Find what you need |

### Validation Scripts

| File | Lines | Tests | Validates |
|------|-------|-------|-----------|
| docker-validation.sh | 390 | 13 | Image build, security, runtime |
| k8s-validation.sh | 416 | 17 | Manifests, RBAC, deployment |

---

## Common Tasks

### I want to...

#### Build and run with Docker
1. Read: [`DOCKER_K8S_README.md` - Docker section](./DOCKER_K8S_README.md#docker)
2. Guide: [`docs/DOCKER_KUBERNETES_SETUP.md#docker-setup`](./docs/DOCKER_KUBERNETES_SETUP.md#docker-setup)
3. Run: `./scripts/docker-validation.sh`

#### Deploy to Kubernetes
1. Read: [`DOCKER_K8S_README.md` - Kubernetes section](./DOCKER_K8S_README.md#kubernetes-deployment)
2. Checklist: [`docs/DEPLOYMENT_CHECKLIST.md`](./docs/DEPLOYMENT_CHECKLIST.md)
3. Guide: [`docs/DOCKER_KUBERNETES_SETUP.md#kubernetes-deployment`](./docs/DOCKER_KUBERNETES_SETUP.md#kubernetes-deployment)
4. Review: [`k8s/` manifests](./k8s/)
5. Run: `./scripts/k8s-validation.sh`

#### Configure for production
1. Follow: [`docs/DEPLOYMENT_CHECKLIST.md`](./docs/DEPLOYMENT_CHECKLIST.md)
2. Review: [`docs/DOCKER_KUBERNETES_SETUP.md#production-best-practices`](./docs/DOCKER_KUBERNETES_SETUP.md#production-best-practices)
3. Setup: Secrets in [`k8s/secret.yaml.template`](./k8s/secret.yaml.template)
4. Configure: `k8s/configmap.yaml` for environment

#### Set up monitoring
1. Guide: [`docs/DOCKER_KUBERNETES_SETUP.md#monitoring-and-observability`](./docs/DOCKER_KUBERNETES_SETUP.md#monitoring-and-observability)
2. Deploy: [`k8s/monitoring.yaml`](./k8s/monitoring.yaml)
3. Configure: Prometheus at [`k8s/configmap.yaml`](./k8s/configmap.yaml)
4. Alerts: PrometheusRule in [`k8s/monitoring.yaml`](./k8s/monitoring.yaml)

#### Troubleshoot issues
1. Check: [`docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting`](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting)
2. Validate: `./scripts/docker-validation.sh` or `./scripts/k8s-validation.sh`
3. Review: Logs in container or pod
4. Reference: Common operations in [`DOCKER_K8S_README.md`](./DOCKER_K8S_README.md)

#### Implement security
1. Review: [`docs/DOCKER_KUBERNETES_SETUP.md#security`](./docs/DOCKER_KUBERNETES_SETUP.md#security)
2. Configure: [`k8s/rbac.yaml`](./k8s/rbac.yaml) for access control
3. Apply: [`k8s/network-policy.yaml`](./k8s/network-policy.yaml) for network security
4. Manage: [`k8s/secret.yaml.template`](./k8s/secret.yaml.template) for credentials

#### Scale application
1. Guide: [`docs/DOCKER_KUBERNETES_SETUP.md#kubernetes-deployment#scaling`](./docs/DOCKER_KUBERNETES_SETUP.md#kubernetes-deployment)
2. Configure: [`k8s/hpa.yaml`](./k8s/hpa.yaml) for auto-scaling
3. Adjust: Resource limits in deployment/statefulset

---

## Key Metrics Summary

### Docker Image
- **Runtime Size**: 140 MB (target: <150MB) ✅
- **Build Time**: ~3 minutes
- **Startup Time**: <30 seconds
- **Base Image**: Alpine 3.20

### Kubernetes Deployment
- **Default Replicas**: 3
- **CPU Request**: 500m per pod
- **Memory Request**: 512Mi per pod
- **CPU Limit**: 2000m per pod
- **Memory Limit**: 2Gi per pod
- **Max Replicas (HPA)**: 10
- **Min Availability (PDB)**: 2 pods

### Monitoring
- **Metrics Port**: 9090
- **Scrape Interval**: 30s
- **Alert Rules**: 5
- **Dashboard**: Included (Grafana)

### Security
- **Container User**: 1000:1000
- **Network Policies**: Deny-all + explicit allows
- **RBAC**: Least privilege
- **TLS Support**: Yes (cert-manager compatible)

---

## Support

### Documentation References
- Official Kubernetes: https://kubernetes.io/docs/
- Official Docker: https://docs.docker.com/
- Erlang/OTP: https://www.erlang.org/doc/

### Getting Help
1. Check troubleshooting guide: [`docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting`](./docs/DOCKER_KUBERNETES_SETUP.md#troubleshooting)
2. Run validation scripts: `./scripts/docker-validation.sh` or `./scripts/k8s-validation.sh`
3. Review logs: `docker logs` or `kubectl logs`
4. Check manifests: Review corresponding YAML file

---

## Maintenance

### Keeping Up-to-Date

**Monthly**:
- Review security patches
- Update base image (Alpine)
- Update dependencies

**Quarterly**:
- Review resource limits
- Tune autoscaler parameters
- Update documentation

**Annually**:
- Full security audit
- Performance optimization
- Architecture review

---

## Version Information

- **erlmcp Version**: 0.7.0
- **Deployment Version**: 1.0.0
- **Last Updated**: 2026-01-27
- **Kubernetes Minimum**: 1.20
- **Docker Minimum**: 20.10

---

**Navigation Complete** - You're ready to deploy erlmcp!
