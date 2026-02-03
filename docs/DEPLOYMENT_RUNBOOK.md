# erlmcp v3 Worldwide Deployment Runbook

**Version:** 3.0.0
**Last Updated:** 2026-02-02
**Status:** Production Ready

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture](#2-architecture)
3. [Multi-Region Deployment](#3-multi-region-deployment)
4. [Deployment Procedures](#4-deployment-procedures)
5. [Service Level Agreements](#5-service-level-agreements)
6. [Disaster Recovery](#6-disaster-recovery)
7. [Operations Runbooks](#7-operations-runbooks)
8. [Emergency Procedures](#8-emergency-procedures)
9. [Rollback Procedures](#9-rollback-procedures)
10. [Monitoring & Observability](#10-monitoring--observability)
11. [Troubleshooting](#11-troubleshooting)
12. [Maintenance Operations](#12-maintenance-operations)
13. [Appendices](#13-appendices)

---

## 1. Overview

### 1.1 Purpose

This runbook provides comprehensive, step-by-step instructions for deploying erlmcp v3 to multiple regions worldwide. It covers:

- Multi-region Kubernetes deployments
- Blue-green and canary release strategies
- Geographic traffic routing
- Disaster recovery and failover
- Service level objectives and monitoring
- Operational procedures for day-to-day management

### 1.2 Deployment Scope

erlmcp v3 is designed for worldwide deployment across multiple cloud regions:

| Region | Location | Purpose | Status |
|--------|----------|---------|--------|
| us-east-1 | US-East (N. Virginia) | Primary (Active) | Production |
| us-west-2 | US-West (Oregon) | DR (Standby) | Production |
| eu-west-1 | EU (Ireland) | Active EU | Production |
| ap-southeast-1 | Asia Pacific (Singapore) | Active APAC | Production |
| ap-northeast-1 | Asia Pacific (Tokyo) | Active Japan | Production |

### 1.3 Target Architecture

```
                     ┌─────────────────────────────────────────┐
                     │            Global Load Balancer           │
                     │              (Route 53 / Cloud DNS)        │
                     └───────────────┬─────────────────────────┘
                                     │
                ┌────────────────────┼────────────────────┐
                │                    │                    │
        ┌───────▼───────┐    ┌──────▼──────┐    ┌──────▼──────┐
        │   us-east-1   │    │  eu-west-1  │    │ ap-southeast│
        │   (Primary)   │    │ (Active EU) │    │ (Active APAC)│
        │               │    │             │    │             │
        │  ┌─────────┐  │    │  ┌─────────┐ │    │  ┌─────────┐│
        │  │ erlmcp  │  │    │  │ erlmcp  │ │    │  │ erlmcp  ││
        │  │ Pods: 3 │  │    │  │ Pods: 2 │ │    │  │ Pods: 2 ││
        │  └─────────┘  │    │  └─────────┘ │    │  └─────────┘│
        │  ┌─────────┐  │    │  ┌─────────┐ │    │  ┌─────────┐│
        │  │   PG    │  │    │  │   PG    │ │    │  │   PG    ││
        │  │ Primary │  │    │  │ Primary │ │    │  │ Primary ││
        │  └─────────┘  │    │  └─────────┘ │    │  └─────────┘│
        └───────┬───────┘    └──────┬──────┘    └──────┬──────┘
                │                    │                    │
                └────────────────────┼────────────────────┘
                                     │
                            Replication (async)
                                     │
        ┌────────────────────────────┼────────────────────────────┐
        │                            │                            │
┌───────▼────────┐          ┌────────▼────────┐         ┌────────▼────────┐
│   us-west-2    │          │   ap-northeast-1│         │  eu-central-1   │
│  (DR Standby)  │          │  (Active Japan)│         │   (DR Standby)  │
│                │          │                 │         │                 │
│  ┌─────────┐   │          │  ┌─────────┐   │         │  ┌─────────┐   │
│  │ erlmcp  │   │          │  │ erlmcp  │   │         │  │ erlmcp  │   │
│  │ Pods: 0 │   │          │  │ Pods: 2 │   │         │  │ Pods: 0 │   │
│  │(Hot SR) │   │          │  └─────────┘   │         │  │(Cold SR)│   │
│  └─────────┘   │          │                 │         │  └─────────┘   │
│  ┌─────────┐   │          │  ┌─────────┐   │         │  ┌─────────┐   │
│  │   PG    │   │          │  │   PG    │   │         │  │   PG    │   │
│  │ Standby │   │          │  │ Primary │   │         │  │ Standby │   │
│  └─────────┘   │          │  └─────────┘   │         │  └─────────┘   │
└────────────────┘          └─────────────────┘         └─────────────────┘

Key:
  Active: Fully operational, serving live traffic
  DR Standby: Hot standby (ready to failover) or Cold standby (provision on failover)
  PG: PostgreSQL with streaming replication
  SR: Standby Replica
```

### 1.4 Technology Stack

| Component | Technology | Version |
|-----------|------------|---------|
| Container Runtime | Docker | 24.0+ |
| Container Orchestrator | Kubernetes | 1.28+ |
| Service Mesh | Istio | 1.19+ |
| Ingress Controller | NGINX / Envoy | Latest |
| Database | PostgreSQL | 15+ |
| Cache | Redis | 7+ |
| Message Queue | RabbitMQ / Kafka | 3.12+ / 3.5+ |
| Monitoring | Prometheus + Grafana | Latest |
| Tracing | Jaeger / Tempo | Latest |
| Logging | Loki / ELK | Latest |

---

## 2. Architecture

### 2.1 Component Overview

erlmcp v3 consists of the following core components:

```
┌─────────────────────────────────────────────────────────────────┐
│                         erlmcp v3 Stack                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    API Gateway Layer                      │  │
│  │  (Authentication, Rate Limiting, Request Routing)        │  │
│  └─────────────────────┬─────────────────────────────────────┘  │
│                        │                                         │
│  ┌─────────────────────▼─────────────────────────────────────┐  │
│  │                   Service Mesh (Istio)                    │  │
│  │  (mTLS, Traffic Management, Observability)               │  │
│  └─────────────────────┬─────────────────────────────────────┘  │
│                        │                                         │
│  ┌─────────────────────▼─────────────────────────────────────┐  │
│  │                  erlmcp Core Services                     │  │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐        │  │
│  │  │  HTTP   │ │  TCP    │ │  WS     │ │  SSE    │        │  │
│  │  │Transport│ │Transport│ │Transport│ │Transport│        │  │
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘        │  │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐        │  │
│  │  │Registry │ │Session  │ │Workflow │ │Cluster  │        │  │
│  │  │  Shards │ │  HA     │ │Orchestr.│ │Coord.   │        │  │
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘        │  │
│  └─────────────────────┬─────────────────────────────────────┘  │
│                        │                                         │
│  ┌─────────────────────▼─────────────────────────────────────┐  │
│  │                   Data Layer                               │  │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐        │  │
│  │  │PostgreSQL│ │  Redis  │ │  Mnesia  │ │  S3     │        │  │
│  │  │(Primary) │ │ (Cache) │ │ (Distrib)│ │(Backup) │        │  │
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘        │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                  Observability Layer                      │  │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐        │  │
│  │  │Prometheus│ │ Jaeger  │ │   Loki  │ │ Grafana │        │  │
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘        │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 Communication Patterns

1. **North-South Traffic** (External → Internal)
   - API Gateway → Service Mesh → erlmcp Services
   - TLS termination at API Gateway
   - mTLS between services

2. **East-West Traffic** (Internal Services)
   - Service-to-service via Service Mesh
   - mTLS required for all inter-service communication
   - Circuit breakers and retries managed by Istio

3. **Database Traffic**
   - Connection pooling via PgBouncer
   - Read replicas for read-heavy operations
   - Streaming replication for HA

### 2.3 Scalability Model

| Dimension | Scale Strategy | Limits |
|-----------|---------------|--------|
| Horizontal Scaling | K8s HPA + Cluster Autoscaler | 100 pods per region |
| Vertical Scaling | Resource requests/limits | 8GB RAM, 4 CPU per pod |
| Data Sharding | Consistent hashing | 256 shards max |
| Database Scaling | Read replicas + partitioning | 10 replicas, 1TB partitions |

---

## 3. Multi-Region Deployment

### 3.1 Region Selection Strategy

#### Primary Region Selection Criteria

1. **Proximity to Users** - Latency < 100ms for 95% of users
2. **Cloud Provider Presence** - Full service availability
3. **Compliance** - Data residency requirements
4. **Cost** - Balance between performance and cost
5. **DR Pairing** - Paired regions for disaster recovery

#### Recommended Region Pairs

| Primary | DR Pair | Distance | RTO | RPO |
|---------|---------|----------|-----|-----|
| us-east-1 | us-west-2 | >1000 miles | 5 min | 1 min |
| eu-west-1 | eu-central-1 | >500 miles | 10 min | 5 min |
| ap-southeast-1 | ap-northeast-1 | >3000 miles | 15 min | 5 min |

### 3.2 Multi-Region Deployment Procedure

#### Step 1: Prepare Primary Region (us-east-1)

```bash
# Set context for primary region
export KUBECONFIG=$HOME/.kube/primary-config
export REGION=us-east-1
export ENV=production

# Create namespace
kubectl create namespace erlmcp-prod

# Create secrets
kubectl create secret generic erlmcp-secrets \
  --from-literal=db-password=$(aws secretsmanager get-secret-value --secret-id erlmcp/db --region $REGION --query SecretString --output text) \
  --from-literal=erlang-cookie=$(openssl rand -base64 32) \
  --from-literal=jwt-secret=$(openssl rand -base64 64) \
  -n erlmcp-prod

# Create TLS certificate
cert-manager certificate \
  --name erlmcp-tls \
  --namespace erlmcp-prod \
  --dns-names erlmcp.io,*.erlmcp.io,*.us-east-1.erlmcp.io \
  --issuer letsencrypt-prod

# Deploy base infrastructure
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/rbac.yaml
kubectl apply -f k8s/network-policy.yaml
kubectl apply -f k8s/priority-class.yaml
kubectl apply -f k8s/resource-quota.yaml
```

#### Step 2: Deploy to Primary Region

```bash
# Deploy using Helm
helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
  --namespace erlmcp-prod \
  --values k8s/deployments/helm/erlmcp/values.yaml \
  --values k8s/production/primary-region.yaml \
  --set global.region=$REGION \
  --set global.environment=production \
  --set global.clusterName=erlmcp-primary \
  --wait \
  --timeout 15m

# Verify deployment
kubectl rollout status statefulset/erlmcp-cluster -n erlmcp-prod
kubectl get pods -n erlmcp-prod -l app=erlmcp
```

#### Step 3: Configure Database Replication

```bash
# Deploy PostgreSQL with replication
helm upgrade --install erlmcp-pg bitnami/postgresql \
  --namespace erlmcp-prod \
  --set architecture=replication \
  --set primary.persistence.size=100Gi \
  --set primary.resources.requests.memory=4Gi \
  --set primary.resources.requests.cpu=2 \
  --set replica.count=2 \
  --set replica.persistence.size=100Gi \
  --values k8s/production/postgres-values.yaml

# Get primary connection string
export PG_PRIMARY=$(kubectl get svc erlmcp-pg-primary -n erlmcp-prod -o jsonpath='{.status.loadBalancer.ingress[0].ip}')

# Configure streaming replication
kubectl exec -it erlmcp-pg-primary-0 -n erlmcp-prod -- psql -U postgres -c "
  SELECT * FROM pg_stat_replication;
"
```

#### Step 4: Deploy to Secondary Regions

```bash
# For each active region (eu-west-1, ap-southeast-1, ap-northeast-1)
for REGION in eu-west-1 ap-southeast-1 ap-northeast-1; do
  echo "Deploying to $REGION..."

  export KUBECONFIG=$HOME/.kube/${REGION}-config

  # Create namespace and secrets
  kubectl create namespace erlmcp-prod

  kubectl create secret generic erlmcp-secrets \
    --from-literal=db-password=$(aws secretsmanager get-secret-value --secret-id erlmcp/db --region $REGION --query SecretString --output text) \
    --from-literal=erlang-cookie=$(aws ssm get-parameter --name /erlmcp/erlang-cookie --region $REGION --query Parameter.Value --output text) \
    --from-literal=jwt-secret=$(aws ssm get-parameter --name /erlmcp/jwt-secret --region $REGION --query Parameter.Value --output text) \
    -n erlmcp-prod

  # Deploy erlmcp
  helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
    --namespace erlmcp-prod \
    --values k8s/deployments/helm/erlmcp/values.yaml \
    --values k8s/production/secondary-region.yaml \
    --set global.region=$REGION \
    --set global.environment=production \
    --set global.clusterName=erlmcp-${REGION} \
    --set erlmcp.replicaCount=2 \
    --wait \
    --timeout 15m

  echo "$REGION deployment complete"
done
```

#### Step 5: Deploy DR Regions

```bash
# For DR regions (us-west-2, eu-central-1)
for REGION in us-west-2 eu-central-1; do
  echo "Deploying DR to $REGION..."

  export KUBECONFIG=$HOME/.kube/${REGION}-config

  # Create namespace
  kubectl create namespace erlmcp-prod

  # Import secrets from primary (same secrets across regions)
  aws secretsmanager get-secret-value --secret-id erlmcp/db --region us-east-1 --query SecretString --output text | \
    aws secretsmanager put-secret-value --secret-id erlmcp/db --region $REGION --secret-string file:///dev/stdin

  # Deploy with reduced scale (hot standby)
  helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
    --namespace erlmcp-prod \
    --values k8s/deployments/helm/erlmcp/values.yaml \
    --values k8s/production/dr-region.yaml \
    --set global.region=$REGION \
    --set global.environment=production \
    --set global.clusterName=erlmcp-${REGION} \
    --set erlmcp.replicaCount=0 \
    --set erlmcp.hpa.enabled=false \
    --wait

  echo "$REGION DR deployment complete"
done
```

#### Step 6: Configure Global Load Balancer

```bash
# Using AWS Route 53
aws route53 create-hosted-zone \
  --name erlmcp.io \
  --caller-reference "$(date +%s)-erlmcp-prod"

# Create health checks
aws route53 create-health-check \
  --caller-reference "$(date +%s)-us-east-1" \
  --health-check-config '{
    "IPAddress": "'$(kubectl get svc erlmcp-ingress -n erlmcp-prod -o jsonpath='{.status.loadBalancer.ingress[0].ip}')'",
    "Port": 443,
    "Type": "HTTPS",
    "ResourcePath": "/health",
    "FullyQualifiedDomainName": "us-east-1.erlmcp.io",
    "RequestInterval": 30,
    "FailureThreshold": 3
  }'

# Create latency-based records for each region
for REGION in us-east-1 us-west-2 eu-west-1 eu-central-1 ap-southeast-1 ap-northeast-1; do
  aws route53 change-resource-record-sets \
    --hosted-zone-id $(aws route53 list-hosted-zones --query "HostedZones[?Name=='erlmcp.io.']".Id --output text) \
    --change-batch '{
      "Changes": [{
        "Action": "CREATE",
        "ResourceRecordSet": {
          "Name": "erlmcp.io",
          "Type": "A",
          "SetIdentifier": "'$REGION'",
          "Region": "'$REGION'",
          "AliasTarget": {
            "HostedZoneId": "Z35SXDOTRKT7K6",
            "DNSName": "'$(kubectl get svc erlmcp-ingress -n erlmcp-prod --context $REGION -o jsonpath='{.status.loadBalancer.ingress[0].hostname')'",
            "EvaluateTargetHealth": true
          }
        }
      }]
    }'
done
```

### 3.3 Inter-Region Connectivity

#### Erlang Distribution Configuration

```erlang
# vm.args for distributed Erlang across regions
-setcookie 'ERLANG_COOKIE_HERE'
-name erlmcp@us-east-1.erlmcp.internal
-kernel inet_dist_use_interface {127,0,0,1}
-kernel inet_dist_listen_min 9100
-kernel inet_dist_listen_max 9105

# For cross-region clustering, use DNS A records:
# - erlmcp-us-east-1.erlmcp.svc.cluster.local
# - erlmcp-eu-west-1.erlmcp.svc.cluster.local
# etc.
```

#### Cluster Formation

```bash
# Connect primary region pods
for i in 0 1 2; do
  kubectl exec erlmcp-cluster-$i -n erlmcp-prod -- erl -sname connector \
    -setcookie erlmcp-cookie \
    -eval "net_adm:ping('erlmcp@erlmcp-cluster-0.erlmcp-headless.erlmcp-prod.svc.cluster.local'), init:stop()."
done

# Connect cross-region (from us-east-1 to eu-west-1)
kubectl exec erlmcp-cluster-0 -n erlmcp-prod -- erl -sname connector \
  -setcookie erlmcp-cookie \
  -eval "net_adm:ping('erlmcp@erlmcp-cluster-0.erlmcp-headless.erlmcp-prod.eu-west-1.svc.cluster.local'), init:stop()."
```

### 3.4 Data Replication Strategy

#### PostgreSQL Replication

```yaml
# postgres-ha.yaml for multi-region
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata:
  name: erlmcp-postgres
  namespace: erlmcp-prod
spec:
  instances: 3

  # Primary region configuration
  primaryUpdateStrategy: unsupervised

  # Replication slots for cross-region
  replicationSlots:
    highAvailability:
      enabled: true
    enabled: true
    synchronizeReplicas: false

  # Cross-region replication
  managed:
    roles:
      - name: replication_user
        ensure: present
        login: true
        inRoles:
          - pg_read_all_data

  # Backup configuration
  backup:
    barmanObjectStore:
      destinationPath: s3://erlmcp-backups/postgres
      s3Credentials:
        accessKeyId:
          name: aws-credentials
          key: accessKeyId
        secretAccessKey:
          name: aws-credentials
          key: secretAccessKey
      wal:
        compression: gzip
        maxParallel: 8
      retentionDays: 30
```

#### Data Synchronization

```bash
# Set up logical replication for cross-region
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- psql -U postgres -c "
  CREATE PUBLICATION erlmcp_pub FOR TABLE sessions, registry, workflows, receipts;
"

# On each region subscriber
for REGION in eu-west-1 ap-southeast-1; do
  kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context $REGION -- psql -U postgres -c "
    CREATE SUBSCRIPTION erlmcp_sub
    CONNECTION 'host=us-east-1.erlmcp.io dbname=erlmcp user=replication_user password=xxx'
    PUBLICATION erlmcp_pub
    WITH (create_slot = false);
  "
done
```

---

## 4. Deployment Procedures

### 4.1 Standard Deployment Flow

#### Pre-Deployment Checklist

```bash
# Run this script before any deployment
./scripts/deploy/pre-deploy-check.sh
```

Checklist items:
- [ ] All tests passing in latest commit
- [ ] Security scan completed (0 critical vulnerabilities)
- [ ] Performance benchmarks within SLA
- [ ] Staging soak completed (minimum 24 hours)
- [ ] Change board approval obtained
- [ ] On-call team notified
- [ ] Rollback plan documented
- [ ] Monitoring dashboards ready
- [ ] Runbooks reviewed

#### Deployment Phases

**Phase 1: Preparation (T-30 minutes)**

```bash
# 1. Set deployment context
export DEPLOY_VERSION=$(git describe --tags --abbrev=0)
export DEPLOY_ENV=production
export PRIMARY_REGION=us-east-1

# 2. Verify cluster connectivity
kubectl cluster-info --context=$PRIMARY_REGION
kubectl get nodes --context=$PRIMARY_REGION

# 3. Check current state
kubectl get statefulset erlmcp-cluster -n erlmcp-prod --context=$PRIMARY_REGION
kubectl get pods -n erlmcp-prod -l app=erlmcp --context=$PRIMARY_REGION

# 4. Capture pre-deployment state
kubectl get all -n erlmcp-prod -o yaml --context=$PRIMARY_REGION > /tmp/pre-deploy-${DEPLOY_ENV}.yaml

# 5. Create pre-deployment backup
./scripts/backup/snapshot-production.sh
```

**Phase 2: Build and Push (T-15 minutes)**

```bash
# 1. Build Docker image
docker build -f docker/Dockerfile.production \
  -t ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION} \
  --build-arg VERSION=${DEPLOY_VERSION} \
  --build-arg BUILD_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ) \
  --build-arg VCS_REF=$(git rev-parse HEAD) \
  .

# 2. Security scan
trivy image --severity CRITICAL,HIGH ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# 3. Push to registry
docker push ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# 4. Sign image
cosign sign \
  -a version=${DEPLOY_VERSION} \
  -a git.sha=$(git rev-parse HEAD) \
  ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}
```

**Phase 3: Update Manifests (T-5 minutes)**

```bash
# 1. Update image in Helm values
sed -i.bak "s/tag: .*/tag: \"${DEPLOY_VERSION}\"/" k8s/production/primary-region.yaml

# 2. Verify manifests
helm template erlmcp ./k8s/deployments/helm/erlmcp \
  --values k8s/deployments/helm/erlmcp/values.yaml \
  --values k8s/production/primary-region.yaml \
  --namespace erlmcp-prod \
  --debug > /tmp/manifest-${DEPLOY_VERSION}.yaml

# 3. Dry-run validation
kubectl apply --dry-run=server -f /tmp/manifest-${DEPLOY_VERSION}.yaml --context=$PRIMARY_REGION

# 4. Commit manifests
git add k8s/production/primary-region.yaml
git commit -m "chore: deploy erlmcp ${DEPLOY_VERSION} to production"
```

**Phase 4: Canary Deployment (T+0 minutes)**

```bash
# 1. Deploy canary (10% traffic)
kubectl apply -f k8s/production/canary-10-percent.yaml --context=$PRIMARY_REGION

# 2. Wait for canary ready
kubectl wait --for=condition=available deployment/erlmcp-canary -n erlmcp-prod --timeout=5m --context=$PRIMARY_REGION

# 3. Monitor canary metrics
./scripts/deploy/monitor-canary.sh 10m

# 4. Run canary tests
./scripts/test/canary-validation.sh
```

**Phase 5: Gradual Rollout (T+30 minutes)**

```bash
# If canary passes, increase to 50%
kubectl apply -f k8s/production/canary-50-percent.yaml --context=$PRIMARY_REGION
./scripts/deploy/monitor-canary.sh 15m

# If 50% passes, full rollout
kubectl apply -f k8s/production/full-rollout.yaml --context=$PRIMARY_REGION
kubectl rollout status statefulset/erlmcp-cluster -n erlmcp-prod --timeout=15m --context=$PRIMARY_REGION
```

**Phase 6: Verification (T+60 minutes)**

```bash
# 1. Health checks
./scripts/health/production-health-check.sh

# 2. Run smoke tests
./scripts/test/smoke-production.sh

# 3. Verify metrics
curl -sf https://erlmcp.io/metrics | grep erlmcp_

# 4. Check traces
curl -sf https://jaeger.erlmcp.io/api/traces?service=erlmcp | jq .
```

### 4.2 Blue-Green Deployment

**When to use:** Major version upgrades, breaking changes

```bash
# 1. Deploy to green environment
helm upgrade --install erlmcp-green ./k8s/deployments/helm/erlmcp \
  --namespace erlmcp-prod \
  --values k8s/deployments/helm/erlmcp/values.yaml \
  --values k8s/production/green-values.yaml \
  --set global.version=${DEPLOY_VERSION}

# 2. Wait for green ready
kubectl wait --for=condition=available statefulset/erlmcp-green -n erlmcp-prod --timeout=10m

# 3. Run tests against green
./scripts/test/target-environment.sh https://green.erlmcp.io

# 4. Switch traffic (instant cutover)
kubectl patch svc erlmcp-ingress -n erlmcp-prod -p '{"spec":{"selector":{"version":"green"}}}'

# 5. Monitor for 15 minutes
./scripts/monitor/watch.sh green 900

# 6. If issues, rollback
# kubectl patch svc erlmcp-ingress -n erlmcp-prod -p '{"spec":{"selector":{"version":"blue"}}}'
```

### 4.3 Rolling Update Deployment

**When to use:** Patch releases, non-breaking updates

```bash
# Rolling update is default for StatefulSets
kubectl set image statefulset/erlmcp-cluster \
  erlmcp=ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION} \
  -n erlmcp-prod

# Monitor rollout
kubectl rollout status statefulset/erlmcp-cluster -n erlmcp-prod --timeout=15m

# If issues, pause and rollback
kubectl rollout pause statefulset/erlmcp-cluster -n erlmcp-prod
kubectl rollout undo statefulset/erlmcp-cluster -n erlmcp-prod
```

### 4.4 Multi-Region Deployment Strategy

#### Strategy 1: Rolling Wave (Recommended)

Deploy to regions sequentially, allowing each region to stabilize before moving to the next.

```bash
# Order based on traffic and risk
REGIONS="eu-west-1 ap-southeast-1 ap-northeast-1 us-east-1"

for REGION in $REGIONS; do
  echo "Deploying to $REGION..."

  export KUBECONFIG=$HOME/.kube/${REGION}-config
  export DEPLOY_VERSION=$(git describe --tags --abbrev=0)

  # Deploy to this region
  helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
    --namespace erlmcp-prod \
    --values k8s/deployments/helm/erlmcp/values.yaml \
    --values k8s/production/${REGION}.yaml \
    --set global.region=$REGION \
    --set erlmcp.image.tag=${DEPLOY_VERSION} \
    --wait --timeout 15m

  # Wait for soak period
  echo "Soaking $REGION for 15 minutes..."
  sleep 900

  # Verify health
  ./scripts/health/region-check.sh $REGION

  echo "$REGION deployment complete"
done
```

#### Strategy 2: Parallel Deployment

Deploy to all regions simultaneously. Higher risk but faster deployment.

```bash
# Deploy to all active regions in parallel
for REGION in eu-west-1 ap-southeast-1 ap-northeast-1 us-east-1; do
  (
    export KUBECONFIG=$HOME/.kube/${REGION}-config
    helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
      --namespace erlmcp-prod \
      --values k8s/deployments/helm/erlmcp/values.yaml \
      --values k8s/production/${REGION}.yaml \
      --set global.region=$REGION \
      --set erlmcp.image.tag=${DEPLOY_VERSION} \
      --wait --timeout 15m
  ) &
done

# Wait for all deployments
wait

# Verify all regions
for REGION in eu-west-1 ap-southeast-1 ap-northeast-1 us-east-1; do
  ./scripts/health/region-check.sh $REGION
done
```

---

## 5. Service Level Agreements

### 5.1 Service Level Objectives (SLOs)

| Metric | SLO | Measurement Period | Error Budget |
|--------|-----|-------------------|--------------|
| Availability | 99.95% | Rolling 30 days | 21.6 minutes/month |
| Uptime | 99.9% | Monthly | 43.2 minutes/month |
| API Response Time (P95) | < 200ms | Daily | N/A |
| API Response Time (P99) | < 500ms | Daily | N/A |
| Error Rate | < 0.05% | Daily | N/A |
| Data Durability | 99.9999999% | Annual | N/A |
| Recovery Time Objective (RTO) | < 5 minutes | Per incident | N/A |
| Recovery Point Objective (RPO) | < 1 minute | Per incident | N/A |

### 5.2 SLO Calculation

#### Availability SLO

```
Target: 99.95% monthly uptime
Allowed downtime: 30 days * 24 hours * 60 minutes * (1 - 0.9995) = 21.6 minutes

Calculation:
  Total minutes in month: 43,200
  Allowed downtime: 43,200 * 0.0005 = 21.6 minutes
  Error budget per day: 21.6 / 30 = 0.72 minutes = 43 seconds
```

#### Latency SLO

```
P95 Target: < 200ms
P99 Target: < 500ms

Measurement:
  - Sample every request
  - Calculate percentiles over 5-minute windows
  - Alert if 2 consecutive windows exceed threshold
```

#### Error Rate SLO

```
Target: < 0.05% (1 error per 2000 requests)

Measurement:
  - Count HTTP 5xx responses
  - Divide by total requests
  - Alert if error budget exceeded for rolling hour
```

### 5.3 SLI Measurements

#### Collection Methods

```yaml
# prometheus-rules.yaml
groups:
  - name: erlmcp_sli
    interval: 30s
    rules:
      # Availability SLI
      - record: job:erlmcp_availability:ratio_rate5m
        expr: |
          sum(rate(http_requests_total{status=~"5.."}[5m]))
          /
          sum(rate(http_requests_total[5m]))

      # Latency SLI
      - record: job:erlmcp_latency:p95
        expr: |
          histogram_quantile(0.95,
            sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
          )

      - record: job:erlmcp_latency:p99
        expr: |
          histogram_quantile(0.99,
            sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
          )

      # Error Rate SLI
      - record: job:erlmcp_error_rate:ratio_rate5m
        expr: |
          sum(rate(http_requests_total{status=~"5.."}[5m]))
          /
          sum(rate(http_requests_total[5m]))

      # Throughput SLI
      - record: job:erlmcp_throughput:rate5m
        expr: |
          sum(rate(http_requests_total[5m]))
```

### 5.4 Error Budget Policy

#### Budget Calculation

```
Monthly Error Budget: 21.6 minutes at 99.95% availability

Burn Rates:
  - 1x normal: 43 seconds/day
  - 2x accelerated: 86 seconds/day
  - 10x accelerated: 430 seconds/day (7 minutes)
  - 100x accelerated: 72 minutes/day

If error budget depleted:
  - Stop all non-essential releases
  - Focus on reliability improvements
  - Require approval for any changes
```

#### Alert Thresholds

```yaml
# sli-alerts.yaml
groups:
  - name: sli_alerts
    rules:
      # Availability alert
      - alert: HighErrorRate
        expr: job:erlmcp_error_rate:ratio_rate5m > 0.001
        for: 10m
        labels:
          severity: critical
          sli: availability
        annotations:
          summary: "Error rate above 0.1% (burning error budget at 100x)"

      # Latency alert
      - alert: HighLatency
        expr: job:erlmcp_latency:p95 > 0.5
        for: 10m
        labels:
          severity: warning
          sli: latency
        annotations:
          summary: "P95 latency above 500ms (2.5x SLO target)"

      # Error budget alert
      - alert: ErrorBudgetBurn
        expr: |
          (
            (1 - job:erlmcp_availability:ratio_rate5m) * (30 * 24 * 3600)
          ) > 720
        for: 5m
        labels:
          severity: critical
          sli: error_budget
        annotations:
          summary: "Error budget burning faster than 1x daily rate"
```

### 5.5 SLA Reporting

#### Monthly SLA Report Template

```markdown
# erlmcp SLA Report - Month Year

## Executive Summary
- Overall Availability: XX.XXX%
- SLO Met: Yes/No
- Incidents: N
- Total Downtime: X minutes

## SLO Performance

### Availability
| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Monthly Uptime | 99.95% | XX.XXX% | Met/Missed |
| Error Rate | < 0.05% | X.XXXX% | Met/Missed |

### Latency
| Metric | Target | P50 | P95 | P99 | Status |
|--------|--------|-----|-----|-----|--------|
| API Response | < 200ms | XXms | XXms | XXms | Met/Missed |

### Incidents
| Date | Severity | Duration | Impact | Root Cause |
|-------|----------|----------|--------|------------|
| YYYY-MM-DD | P0/P1/P2 | X min | Description | Summary |

## Error Budget Analysis
- Starting Error Budget: 21.6 minutes
- Consumed: X minutes (XX%)
- Remaining: X minutes (XX%)
- Burn Rate: Xx normal

## Trends
- Availability trend (3 months): [up/down/stable]
- Latency trend (3 months): [improving/degrading/stable]
- Incident frequency: [increasing/decreasing/stable]

## Actions
- Completed improvements: N items
- In-flight improvements: N items
- Planned improvements: N items
```

---

## 6. Disaster Recovery

### 6.1 DR Architecture

```
Active-Passive Multi-Region DR:

┌─────────────────────────────────────────────────────────┐
│                   Active Region (us-east-1)             │
│  ┌─────────────────────────────────────────────────┐   │
│  │  erlmcp Pods (Running)                          │   │
│  │  PostgreSQL Primary (Running)                   │   │
│  │  Redis Primary (Running)                        │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────┬───────────────────────────────────┘
                      │
                      │ Replication (async)
                      │
┌─────────────────────▼───────────────────────────────────┐
│                 DR Region (us-west-2)                   │
│  ┌─────────────────────────────────────────────────┐   │
│  │  erlmcp Pods (Stopped / Hot Standby)            │   │
│  │  PostgreSQL Standby (Replica)                   │   │
│  │  Redis Standby (Replica)                        │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

### 6.2 Backup Strategy

#### Backup Schedule

| Backup Type | Frequency | Retention | Location |
|-------------|-----------|-----------|----------|
| Database Full | Daily 2AM | 30 days | S3 + Glacier |
| Database Incremental | Every 4 hours | 7 days | S3 |
| ETS Snapshots | Hourly | 7 days | S3 |
| Configuration | Per change | 90 days | Git + S3 |
| Logs | Continuous | 30 days | S3 |

#### Automated Backup Script

```bash
#!/bin/bash
# scripts/backup/daily-backup.sh

set -euo pipefail

BACKUP_DATE=$(date +%Y%m%d-%H%M%S)
BACKUP_PREFIX="erlmcp-backup-${BACKUP_DATE}"
S3_BUCKET="s3://erlmcp-backups/prod"

echo "Starting backup: ${BACKUP_PREFIX}"

# 1. Database backup
echo "Backing up PostgreSQL..."
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  pg_dumpall -U postgres | \
  gzip | \
  aws s3 cp - ${S3_BUCKET}/postgres/${BACKUP_PREFIX}.sql.gz

# 2. ETS backup (from running nodes)
echo "Backing up ETS tables..."
for POD in $(kubectl get pods -n erlmcp-prod -l app=erlmcp -o name); do
  kubectl exec ${POD} -n erlmcp-prod -- \
    erlmcpctl backup /tmp/ets.backup

  kubectl cp ${POD}:/tmp/ets.backup \
    /tmp/ets-$(basename ${POD}).backup -n erlmcp-prod
done

tar -czf /tmp/ets-backups.tar.gz /tmp/ets-*.backup
aws s3 cp /tmp/ets-backups.tar.gz ${S3_BUCKET}/ets/${BACKUP_PREFIX}.tar.gz

# 3. ConfigMap backup
echo "Backing up configuration..."
kubectl get configmap -n erlmcp-prod -o yaml > /tmp/configmaps.yaml
aws s3 cp /tmp/configmaps.yaml ${S3_BUCKET}/config/${BACKUP_PREFIX}.yaml

# 4. Secrets backup (encrypted)
echo "Backing up secrets..."
kubectl get secret -n erlmcp-prod -o yaml | \
  openssl enc -aes-256-cbc -salt -pass env:BACKUP_KEY | \
  aws s3 cp - ${S3_BUCKET}/secrets/${BACKUP_PREFIX}.enc

# 5. Kubernetes resources
echo "Backing up Kubernetes resources..."
kubectl get all -n erlmcp-prod -o yaml > /tmp/k8s-resources.yaml
aws s3 cp /tmp/k8s-resources.yaml ${S3_BUCKET}/k8s/${BACKUP_PREFIX}.yaml

# 6. Clean old backups
echo "Cleaning old backups..."
aws s3 ls ${S3_BUCKET}/postgres/ | \
  grep "prev" | \
  awk '{print $2}' | \
  head -n -30 | \
  xargs -I {} aws s3 rm ${S3_BUCKET}/postgres/{}

echo "Backup complete: ${BACKUP_PREFIX}"
```

### 6.3 Failover Procedures

#### Automated Failover

```bash
#!/bin/bash
# scripts/dr/failover-to-dr.sh

set -euo pipefail

PRIMARY_REGION=${1:-us-east-1}
DR_REGION=${2:-us-west-2}
FAILOVER_TYPE=${3:-planned}  # planned or emergency

echo "Starting ${FAILOVER_TYPE} failover: ${PRIMARY_REGION} -> ${DR_REGION}"

if [ "$FAILOVER_TYPE" = "emergency" ]; then
  echo "EMERGENCY FAILOVER - skipping safety checks"
else
  # Planned failover: verify DR readiness
  echo "Verifying DR region readiness..."
  kubectl get nodes --context=$DR_REGION

  # Verify DR pods can start
  kubectl scale statefulset/erlmcp-cluster --replicas=1 -n erlmcp-prod --context=$DR_REGION

  # Wait for pod ready
  kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=5m --context=$DR_REGION

  echo "DR region verified, proceeding with failover"
fi

# 1. Promote PostgreSQL standby
echo "Promoting PostgreSQL in ${DR_REGION}..."
kubectl exec erlmcp-pg-standby-0 -n erlmcp-prod --context=$DR_REGION -- \
  psql -U postgres -c "SELECT pg_promote();"

# 2. Scale up erlmcp in DR
echo "Scaling up erlmcp in ${DR_REGION}..."
kubectl scale statefulset/erlmcp-cluster --replicas=3 -n erlmcp-prod --context=$DR_REGION

# 3. Wait for readiness
echo "Waiting for pods to be ready..."
kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=10m --context=$DR_REGION

# 4. Update DNS
echo "Updating DNS to point to ${DR_REGION}..."
aws route53 change-resource-record-sets \
  --hosted-zone-id Z35SXDOTRKT7K6 \
  --change-batch file://scripts/dr/failover-dns-change.json

# 5. Verify traffic flowing
echo "Verifying traffic..."
for i in {1..10}; do
  if curl -sf https://erlmcp.io/health > /dev/null; then
    echo "Traffic verification successful"
    break
  fi
  echo "Waiting for traffic... ($i/10)"
  sleep 10
done

# 6. Update monitoring
echo "Updating monitoring targets..."
./scripts/monitoring/update-targets.sh ${DR_REGION}

echo "Failover to ${DR_REGION} complete"
```

#### Manual Failover Steps

```bash
# 1. Verify DR region health
kubectl get nodes --context=us-west-2
kubectl get pods -n erlmcp-prod --context=us-west-2

# 2. Promote database
kubectl exec erlmcp-pg-standby-0 -n erlmcp-prod --context=us-west-2 -- \
  psql -U postgres -c "SELECT pg_promote();"

# 3. Start application
kubectl scale statefulset/erlmcp-cluster --replicas=3 -n erlmcp-prod --context=us-west-2

# 4. Update Route 53
aws route53 change-resource-record-sets --hosted-zone-id Z35SXDOTRKT7K6 \
  --change-batch '{
    "Changes": [{
      "Action": "UPSERT",
      "ResourceRecordSet": {
        "Name": "erlmcp.io",
        "Type": "A",
        "SetIdentifier": "us-west-2-dr",
        "Region": "us-west-2",
        "AliasTarget": {
          "HostedZoneId": "Z35SXDOTRKT7K6",
          "DNSName": "elb.us-west-2.amazonaws.com",
          "EvaluateTargetHealth": true
        }
      }
    }]
  }'

# 5. Scale down primary (if safe)
# Only after DR is fully operational
kubectl scale statefulset/erlmcp-cluster --replicas=0 -n erlmcp-prod --context=us-east-1
```

### 6.4 Failback Procedure

```bash
#!/bin/bash
# scripts/dr/failback-to-primary.sh

PRIMARY_REGION=${1:-us-east-1}
DR_REGION=${2:-us-west-2}

echo "Starting failback: ${DR_REGION} -> ${PRIMARY_REGION}"

# 1. Verify primary region is recovered
echo "Verifying primary region recovery..."
kubectl get nodes --context=$PRIMARY_REGION
kubectl get nodes --context=$PRIMARY_REGION | grep Ready

# 2. Rebuild primary from backups if needed
./scripts/backup/restore-from-backup.sh ${PRIMARY_REGION}

# 3. Set up replication from DR to primary
echo "Setting up reverse replication..."
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context=$PRIMARY_REGION -- \
  psql -U postgres -c "
    CREATE SUBSCRIPTION dr_replication
    CONNECTION 'host=${DR_REGION}.erlmcp.io dbname=erlmcp user=replicator'
    PUBLICATION erlmcp_pub
    WITH (create_slot = false);
  "

# 4. Wait for sync
echo "Waiting for data sync..."
sleep 300  # 5 minutes minimum sync

# 5. Switch application to primary
echo "Switching traffic to primary..."
kubectl scale statefulset/erlmcp-cluster --replicas=3 -n erlmcp-prod --context=$PRIMARY_REGION
kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=10m --context=$PRIMARY_REGION

# 6. Update DNS
aws route53 change-resource-record-sets --hosted-zone-id Z35SXDOTRKT7K6 \
  --change-batch file://scripts/dr/failback-dns-change.json

# 7. Verify and demote DR
echo "Verifying primary is serving traffic..."
sleep 60
curl -sf https://erlmcp.io/health

# 8. Demote DR to standby
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context=$DR_REGION -- \
  psql -U postgres -c "SELECT pg_demote('standby');"

# 9. Scale down DR pods
kubectl scale statefulset/erlmcp-cluster --replicas=0 -n erlmcp-prod --context=$DR_REGION

echo "Failback complete"
```

---

## 7. Operations Runbooks

### 7.1 Daily Operations

#### Morning Checklist

```bash
#!/bin/bash
# scripts/ops/daily-checklist.sh

echo "=== erlmcp Daily Operations Checklist ==="
echo "Date: $(date)"
echo ""

# 1. Health check across all regions
echo "1. Regional Health Check"
for REGION in us-east-1 us-west-2 eu-west-1 ap-southeast-1; do
  STATUS=$(curl -sf https://${REGION}.erlmcp.io/health | jq -r '.status')
  echo "  ${REGION}: ${STATUS}"
done

# 2. Database replication status
echo ""
echo "2. Database Replication"
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  psql -U postgres -c "SELECT * FROM pg_stat_replication;" | \
  column -t -s'|'

# 3. Pod status
echo ""
echo "3. Pod Status"
kubectl get pods -n erlmcp-prod -l app=erlmcp --all-regions

# 4. Resource usage
echo ""
echo "4. Resource Usage"
kubectl top pods -n erlmcp-prod -l app=erlmcp | \
  awk '{if($1!="NAME") printf "%-40s %8s %8s\n", $1, $2, $3}'

# 5. Recent errors
echo ""
echo "5. Recent Errors (last hour)"
kubectl logs -n erlmcp-prod -l app=erlmcp --since=1h | grep -i error | wc -l
echo "  Total error count in logs"

# 6. Certificate expiry
echo ""
echo "6. Certificate Status"
kubectl get secret erlmcp-tls -n erlmcp-prod -o jsonpath='{.data.tls\.crt}' | \
  base64 -d | openssl x509 -noout -dates

# 7. Backup verification
echo ""
echo "7. Latest Backup"
aws s3 ls s3://erlmcp-backups/prod/postgres/ | tail -1

echo ""
echo "=== Daily Checklist Complete ==="
```

#### Weekly Tasks

```bash
#!/bin/bash
# scripts/ops/weekly-tasks.sh

echo "=== Weekly Maintenance Tasks ==="

# 1. Review and clean old logs
echo "1. Log Cleanup"
./scripts/ops/cleanup-logs.sh --older-than=30d

# 2. Review disk usage
echo "2. Disk Usage Review"
./scripts/ops/check-disk-usage.sh --threshold=80

# 3. Database vacuum
echo "3. Database Vacuum"
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  psql -U postgres -c "VACUUM ANALYZE;"

# 4. Update certificates if needed
echo "4. Certificate Check"
./scripts/ops/check-certs.sh --within=30d

# 5. Generate weekly report
echo "5. Weekly Report"
./scripts/ops/generate-weekly-report.sh

echo "=== Weekly Tasks Complete ==="
```

### 7.2 Scaling Operations

#### Horizontal Scaling

```bash
#!/bin/bash
# scripts/ops/scale-horizontal.sh

REPLICAS=${1:-5}
REGION=${2:-us-east-1}

echo "Scaling erlmcp to ${REPLICAS} replicas in ${REGION}"

# 1. Update StatefulSet
kubectl scale statefulset/erlmcp-cluster \
  --replicas=${REPLICAS} \
  -n erlmcp-prod \
  --context=${REGION}

# 2. Wait for pods ready
kubectl wait --for=condition=ready pod \
  -l app=erlmcp \
  -n erlmcp-prod \
  --context=${REGION} \
  --timeout=10m

# 3. Update HPA if enabled
kubectl patch hpa erlmcp-hpa \
  -n erlmcp-prod \
  --context=${REGION} \
  -p '{"spec":{"maxReplicas":'$((${REPLICAS} * 2))'}}'

echo "Scaling complete. Current replicas:"
kubectl get pods -n erlmcp-prod -l app=erlmcp --context=${REGION}
```

#### Vertical Scaling

```bash
#!/bin/bash
# scripts/ops/scale-vertical.sh

CPU_REQUEST=${1:-1000m}
CPU_LIMIT=${2:-4000m}
MEM_REQUEST=${3:-2Gi}
MEM_LIMIT=${4:-4Gi}

echo "Vertical scaling: CPU ${CPU_REQUEST}/${CPU_LIMIT}, Memory ${MEM_REQUEST}/${MEM_LIMIT}"

# Update resource requirements
kubectl set resources statefulset/erlmcp-cluster \
  -n erlmcp-prod \
  --requests=cpu=${CPU_REQUEST},memory=${MEM_REQUEST} \
  --limits=cpu=${CPU_LIMIT},memory=${MEM_LIMIT}

# Rolling restart to apply
kubectl rollout restart statefulset/erlmcp-cluster -n erlmcp-prod

echo "Vertical scaling complete"
```

### 7.3 Database Operations

#### Connection Pool Management

```bash
#!/bin/bash
# scripts/ops/manage-connection-pool.sh

ACTION=${1:-status}  # status, scale-up, scale-down
POOL_SIZE=${2:-50}

case $ACTION in
  status)
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "
        SELECT
          count(*) as connections,
          count(*) FILTER (WHERE state = 'active') as active,
          count(*) FILTER (WHERE state = 'idle') as idle
        FROM pg_stat_activity;
      "
    ;;

  scale-up)
    kubectl patch deployment pgbouncer -n erlmcp-prod \
      -p '{"spec":{"replicas":'${POOL_SIZE}'}}'
    ;;

  scale-down)
    kubectl patch deployment pgbouncer -n erlmcp-prod \
      -p '{"spec":{"replicas":'${POOL_SIZE}'}}'
    ;;
esac
```

#### Database Maintenance

```bash
#!/bin/bash
# scripts/ops/db-maintenance.sh

TASK=${1:-analyze}

case $TASK in
  analyze)
    echo "Running ANALYZE..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "ANALYZE VERBOSE;"
    ;;

  vacuum)
    echo "Running VACUUM..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "VACUUM VERBOSE;"
    ;;

  reindex)
    echo "Running REINDEX..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "REINDEX DATABASE erlmcp;"
    ;;

  full)
    echo "Running full maintenance..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "VACUUM FULL ANALYZE;"
    ;;
esac
```

---

## 8. Emergency Procedures

### 8.1 Incident Response

#### Severity Levels

| Severity | Name | Response Time | Resolution Time | Examples |
|----------|------|---------------|-----------------|----------|
| P0 | Critical | 5 minutes | 1 hour | Complete outage |
| P1 | High | 15 minutes | 4 hours | Major degradation |
| P2 | Medium | 1 hour | 1 day | Partial degradation |
| P3 | Low | 1 day | 1 week | Minor issues |

#### P0 - Complete Outage Procedure

```bash
#!/bin/bash
# scripts/emergency/p0-outage-response.sh

echo "=== P0 OUTAGE RESPONSE ==="
echo "Time: $(date)"
echo ""

# 1. Declare incident
echo "1. Declaring incident..."
pdectl incident create \
  --severity critical \
  --title "Complete Service Outage" \
  --description "All regions not responding" \
  --trigger-auto-escalation

# 2. Page on-call
echo "2. Paging on-call team..."
pagerduty on-call notify --severity critical

# 3. Update status page
echo "3. Updating status page..."
status-page update --status major_outage \
  --message "Investigating complete service outage"

# 4. Assemble war room
echo "4. Assembling war room..."
# Check primary region health
for REGION in us-east-1 us-west-2 eu-west-1 ap-southeast-1; do
  echo "Checking ${REGION}..."
  kubectl get pods -n erlmcp-prod --context=${REGION} 2>/dev/null || echo "  ${REGION}: UNREACHABLE"
done

# 5. Immediate mitigation
echo "5. Attempting immediate mitigation..."

# Check if DNS is working
if ! host erlmcp.io > /dev/null; then
  echo "  DNS issue detected"
  ./scripts/emergency/fix-dns.sh
fi

# Check load balancer
if ! curl -sf https://erlmcp.io/health > /dev/null; then
  echo "  Load balancer issue detected"
  ./scripts/emergency/fix-lb.sh
fi

# 6. Initiate DR failover if needed
read -p "Initiate DR failover? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
  ./scripts/dr/failover-to-dr.sh us-east-1 us-west-2 emergency
fi

echo "=== P0 RESPONSE COMPLETE ==="
```

### 8.2 Security Incidents

#### Data Breach Response

```bash
#!/bin/bash
# scripts/emergency/security-incident.sh

echo "=== SECURITY INCIDENT RESPONSE ==="
echo "Time: $(date)"
echo ""

# 1. Isolate affected systems
echo "1. Isolating affected systems..."
kubectl networkpolicy create incident-isolation -n erlmcp-prod \
  --pod-selector=app=erlmcp \
  --deny-all

# 2. Enable audit logging
echo "2. Enabling audit logging..."
kubectl apply -f scripts/emergency/audit-logging-policy.yaml

# 3. Preserve evidence
echo "3. Preserving evidence..."
./scripts/security/capture-forensics.sh

# 4. Rotate secrets
echo "4. Rotating all secrets..."
./scripts/security/rotate-all-secrets.sh

# 5. Notify security team
echo "5. Notifying security team..."
security-incident notify --severity high \
  --description "Potential security breach detected"

# 6. Initiate investigation
echo "6. Starting investigation..."
./scripts/security/investigate-incident.sh

echo "=== SECURITY INCIDENT COMPLETE ==="
```

### 8.3 Data Corruption Response

```bash
#!/bin/bash
# scripts/emergency/data-corruption.sh

echo "=== DATA CORRUPTION RESPONSE ==="

# 1. Stop all writes
echo "1. Stopping writes..."
kubectl apply -f scripts/emergency/read-only-mode.yaml

# 2. Verify corruption extent
echo "2. Verifying corruption..."
./scripts/backup/verify-backup-integrity.sh

# 3. Determine restore point
echo "3. Finding last good backup..."
LAST_GOOD=$(aws s3 ls s3://erlmcp-backups/prod/postgres/ | grep "passed" | tail -1 | awk '{print $2}')

# 4. Execute restore
echo "4. Restoring from backup..."
./scripts/backup/restore.sh ${LAST_GOOD}

# 5. Verify restored data
echo "5. Verifying restored data..."
./scripts/ops/run-data-validation.sh

# 6. Resume operations
echo "6. Resuming operations..."
kubectl delete -f scripts/emergency/read-only-mode.yaml

echo "=== DATA CORRUPTION RESPONSE COMPLETE ==="
```

---

## 9. Rollback Procedures

### 9.1 Automatic Rollback

Rollback is automatically triggered when:

- Health check fails for 5 consecutive checks
- Error rate exceeds 5% for 5 minutes
- P99 latency exceeds 10 seconds for 5 minutes
- Crash loop detected
- Canary analysis metrics exceed thresholds

```yaml
# k8s/production/rollback-policy.yaml
apiVersion: argoproj.io/v1alpha1
kind: RolloutMan
metadata:
  name: erlmcp-rollback
  namespace: erlmcp-prod
spec:
  replicas: 3

  strategy:
    canary:
      steps:
      - setWeight: 10
      - pause: {duration: 10m}
      - setWeight: 50
      - pause: {duration: 10m}
      - setWeight: 100

      analysis:
        templates:
        - templateName: success-rate
        - templateName: latency

        args:
        - name: service
          value: erlmcp
        - name: threshold
          value: "99"

      rollbackPrometheus:
        # Auto-rollback on metric thresholds
        - metricName: http_requests_total
          threshold: 0.05
          provider: prometheus

      maxUnavailable: 0

  rollback:
    # Automatic rollback on analysis failure
    autoRollback: true
```

### 9.2 Manual Rollback

#### Rollback to Previous Version

```bash
#!/bin/bash
# scripts/rollback/rollback-previous.sh

ENVIRONMENT=${1:-production}
REGION=${2:-us-east-1}

echo "Rolling back ${ENVIRONMENT} in ${REGION}..."

# 1. Get previous version
PREV_VERSION=$(kubectl get statefulset erlmcp-cluster \
  -n erlmcp-prod \
  --context=${REGION} \
  -o jsonpath='{.status.currentRevision}' | \
  grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' | \
  head -1)

echo "Rolling back to ${PREV_VERSION}"

# 2. Update image tag
kubectl set image statefulset/erlmcp-cluster \
  erlmcp=ghcr.io/seanchatmangpt/erlmcp:${PREV_VERSION} \
  -n erlmcp-prod \
  --context=${REGION}

# 3. Wait for rollback
kubectl rollout status statefulset/erlmcp-cluster \
  -n erlmcp-prod \
  --context=${REGION} \
  --timeout=10m

# 4. Verify
./scripts/health/region-check.sh ${REGION}

echo "Rollback complete"
```

#### Rollback to Specific Version

```bash
#!/bin/bash
# scripts/rollback/rollback-to-version.sh

VERSION=${1}
ENVIRONMENT=${2:-production}
REGION=${3:-us-east-1}

if [ -z "$VERSION" ]; then
  echo "Usage: $0 <version> [environment] [region]"
  exit 1
fi

echo "Rolling back to version ${VERSION}..."

# 1. Verify version exists
if ! docker pull ghcr.io/seanchatmangpt/erlmcp:${VERSION} 2>/dev/null; then
  echo "Version ${VERSION} not found"
  exit 1
fi

# 2. Update StatefulSet
kubectl set image statefulset/erlmcp-cluster \
  erlmcp=ghcr.io/seanchatmangpt/erlmcp:${VERSION} \
  -n erlmcp-prod \
  --context=${REGION}

# 3. Verify signature
cosign verify ghcr.io/seanchatmangpt/erlmcp:${VERSION}

# 4. Wait for rollout
kubectl rollout status statefulset/erlmcp-cluster \
  -n erlmcp-prod \
  --context=${REGION} \
  --timeout=10m

echo "Rollback to ${VERSION} complete"
```

### 9.3 Emergency Rollback

```bash
#!/bin/bash
# scripts/rollback/emergency-rollback.sh

REGION=${1:-us-east-1}

echo "EMERGENCY ROLLBACK in ${REGION}"

# 1. Scale to zero immediately
kubectl scale statefulset/erlmcp-cluster \
  --replicas=0 \
  -n erlmcp-prod \
  --context=${REGION}

# 2. Force delete all pods
kubectl delete pods -n erlmcp-prod -l app=erlmcp \
  --context=${REGION} \
  --force --grace-period=0

# 3. Get last known good backup
LAST_GOOD=$(aws s3 ls s3://erlmcp-backups/prod/postgres/ | \
  grep -E "backup-[0-9]+" | \
  tail -1 | \
  awk '{print $2}')

# 4. Restore if needed
read -p "Restore from backup ${LAST_GOOD}? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
  ./scripts/backup/restore.sh ${LAST_GOOD}
fi

# 5. Scale back up
kubectl scale statefulset/erlmcp-cluster \
  --replicas=3 \
  -n erlmcp-prod \
  --context=${REGION}

# 6. Wait for recovery
kubectl wait --for=condition=ready pod \
  -l app=erlmcp \
  -n erlmcp-prod \
  --context=${REGION} \
  --timeout=15m

echo "Emergency rollback complete"
```

---

## 10. Monitoring & Observability

### 10.1 Metrics Collection

#### Key Metrics to Monitor

| Category | Metric | Type | Alert Threshold |
|----------|--------|------|----------------|
| Application | http_requests_total | Counter | N/A |
| Application | http_request_duration_seconds | Histogram | P95 > 200ms |
| Application | http_errors_total | Counter | Rate > 0.001 |
| Application | active_connections | Gauge | > 10000 |
| Application | queue_depth | Gauge | > 1000 |
| Infrastructure | cpu_usage | Gauge | > 80% |
| Infrastructure | memory_usage | Gauge | > 85% |
| Infrastructure | disk_usage | Gauge | > 80% |
| Infrastructure | network_io | Counter | N/A |
| Database | pg_stat_activity | Gauge | > 100 |
| Database | pg_replication_lag | Gauge | > 1s |
| Database | pg_cache_hit_ratio | Gauge | < 0.95 |

#### Prometheus Configuration

```yaml
# prometheus-rules.yaml
groups:
  - name: erlmcp_alerts
    interval: 30s
    rules:
      # Application alerts
      - alert: HighErrorRate
        expr: |
          (
            sum(rate(http_requests_total{status=~"5.."}[5m]))
            /
            sum(rate(http_requests_total[5m]))
          ) > 0.001
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "High error rate detected"
          description: "Error rate is {{ $value | humanizePercentage }}"

      - alert: HighLatency
        expr: |
          histogram_quantile(0.95,
            sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
          ) > 0.5
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "High latency detected"
          description: "P95 latency is {{ $value }}s"

      # Infrastructure alerts
      - alert: HighCPUUsage
        expr: |
          sum(rate(container_cpu_usage_seconds_total{namespace="erlmcp-prod"}[5m]))
          by (pod)
          > 0.8
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "High CPU usage on {{ $labels.pod }}"

      - alert: HighMemoryUsage
        expr: |
          sum(container_memory_working_set_bytes{namespace="erlmcp-prod"})
          by (pod)
          /
          sum(container_spec_memory_limit_bytes{namespace="erlmcp-prod"})
          by (pod)
          > 0.85
        for: 10m
        labels:
          severity: critical
        annotations:
          summary: "High memory usage on {{ $labels.pod }}"

      # Database alerts
      - alert: DatabaseReplicationLag
        expr: |
          pg_stat_replication_lag_seconds > 5
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Database replication lag: {{ $value }}s"

      - alert: DatabaseConnectionPoolExhausted
        expr: |
          sum(pg_stat_activity_count) / sum(pg_settings_max_connections) > 0.9
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "Database connection pool nearly exhausted"
```

### 10.2 Logging

#### Log Aggregation

```yaml
# loki-promtail-config.yaml
server:
  http_listen_port: 9080

positions:
  filename: /tmp/positions.yaml

clients:
  - url: http://loki.erlmcp.io:3100/loki/api/v1/push

scrape_configs:
  # Kubernetes logs
  - job_name: kubernetes-pods
    kubernetes_sd_configs:
      - role: pod
    relabel_configs:
      - source_labels: [__meta_kubernetes_pod_label_app]
        action: keep
        regex: erlmcp
      - source_labels: [__meta_kubernetes_pod_name]
        target_label: pod
      - source_labels: [__meta_kubernetes_namespace]
        target_label: namespace
      - source_labels: [__meta_kubernetes_pod_node_name]
        target_label: node
      - source_labels: [__meta_kubernetes_pod_label_version]
        target_label: version

  # Erlang/OTP logs
  - job_name: erlang-logs
    static_configs:
      - targets:
          - localhost
        labels:
          job: erlang
          __path__: /var/log/erlmcp/*.log
```

### 10.3 Tracing

#### Jaeger Integration

```erlang
%% otel_configuration.erl
-module(otel_configuration).
-export([init/0]).

init() ->
    %% Configure OpenTelemetry
    application:set_env(opentelemetry, processors,
        [{otel_batch_processor, #{exporter => otlp_exporter}}]),

    %% Configure OTLP exporter
    application:set_env(opentelemetry_exporter, otlp_protocol,
        #{endpoint => "http://jaeger-collector:4317"}),

    %% Start tracing
    opentelemetry:start(),
    opentelemetry:start_tracer_provider(),

    %% Instrument key modules
    opentelemetry_api:register_tracer_provider(otel_tracer_provider),

    ok.
```

---

## 11. Troubleshooting

### 11.1 Common Issues

#### Issue: Pods Not Starting

**Symptoms:**
- Pods stuck in Pending state
- CreateContainerConfigError

**Diagnosis:**
```bash
kubectl describe pod <pod-name> -n erlmcp-prod
kubectl get events -n erlmcp-prod --sort-by='.lastTimestamp'
```

**Common Causes:**
1. Image pull failure
   ```bash
   # Check image pull secret
   kubectl get secret ghcr-pull-secret -n erlmcp-prod -o yaml

   # Test image pull
   docker pull ghcr.io/seanchatmangpt/erlmcp:latest
   ```

2. Resource limits
   ```bash
   # Check node resources
   kubectl describe nodes

   # Check if requests can be met
   kubectl top nodes
   ```

3. Missing PVC
   ```bash
   # Check PVC status
   kubectl get pvc -n erlmcp-prod

   # Check storage class
   kubectl get storageclass
   ```

**Resolution:**
```bash
# For image issues, update secret
kubectl delete secret ghcr-pull-secret -n erlmcp-prod
kubectl create secret docker-registry ghcr-pull-secret \
  --docker-server=ghcr.io \
  --docker-username=<username> \
  --docker-password=<token> \
  -n erlmcp-prod

# For resource issues, scale up nodes
kubectl scale nodepool <nodepool> --replicas=<n>

# For PVC issues, check storage
kubectl get pv -n erlmcp-prod
```

#### Issue: High Memory Usage

**Symptoms:**
- OOMKilled events
- Pods restarting

**Diagnosis:**
```bash
# Check pod memory usage
kubectl top pods -n erlmcp-prod -l app=erlmcp

# Check for OOMKilled
kubectl get pods -n erlmcp-prod -o json | jq '.items[] | select(.status.containerStatuses[].state.terminated.reason == "OOMKilled")'

# Check Erlang memory
kubectl exec -it <pod-name> -n erlmcp-prod -- \
  erl -n erlmcp@127.0.0.1 -setcookie test \
  -eval 'erlang:display(erlang:memory(total)), init:stop().'
```

**Resolution:**
```bash
# 1. Increase memory limit
kubectl set resources statefulset erlmcp-cluster \
  -n erlmcp-prod \
  --limits=memory=4Gi

# 2. Enable more aggressive GC
# In vm.args or config:
# +env ERL_FULLSWEEP_AFTER 0
# +env ERL_MAX_PORTS 65536

# 3. Restart with new limits
kubectl rollout restart statefulset erlmcp-cluster -n erlmcp-prod
```

#### Issue: Database Connection Failures

**Symptoms:**
- "connection refused" errors
- Connection pool exhaustion

**Diagnosis:**
```bash
# Test database connectivity
kubectl run -it --rm debug --image=postgres:15 --restart=Never \
  -- psql -h erlmcp-pg-primary -U postgres -c "SELECT 1"

# Check connection count
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  psql -U postgres -c "SELECT count(*) FROM pg_stat_activity;"

# Check for blocked queries
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  psql -U postgres -c "
    SELECT pid, query, state, wait_event
    FROM pg_stat_activity
    WHERE state = 'active'
    ORDER BY query_start;
  "
```

**Resolution:**
```bash
# 1. Increase pool size
kubectl set env deployment/erlmcp \
  -n erlmcp-prod \
  --prefix=DB_POOL \
  DB_POOL_SIZE=50

# 2. Kill long-running queries
kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
  psql -U postgres -c "
    SELECT pg_terminate_backend(pid)
    FROM pg_stat_activity
    WHERE query_start < now() - interval '5 minutes';
  "

# 3. Add PgBouncer if not present
helm upgrade --install pgbouncer bitnami/pgbouncer \
  --set poolMode=transaction \
  --set maxClientConn=1000 \
  --namespace erlmcp-prod
```

### 11.2 Debug Commands

```bash
# Connect to running Erlang node
kubectl exec -it <pod-name> -n erlmcp-prod -- \
  erl -sname debug -setcookie <cookie> -remsh erlmcp@<pod-name>

# Check registered processes
kubectl exec <pod-name> -n erlmcp-prod -- \
  /opt/erlmcp/bin/erlmcp eval "erlang:processes()."

# Check ETS tables
kubectl exec <pod-name> -n erlmcp-prod -- \
  /opt/erlmcp/bin/erlmcp eval "ets:all()."

# Check application logs
kubectl logs <pod-name> -n erlmcp-prod --tail=1000 | less

# Port forward for local debugging
kubectl port-forward <pod-name> 8080:8080 -n erlmcp-prod
```

---

## 12. Maintenance Operations

### 12.1 Certificate Rotation

```bash
#!/bin/bash
# scripts/maintenance/rotate-certs.sh

DOMAIN=${1:-erlmcp.io}
NAMESPACE=${2:-erlmcp-prod}

echo "Rotating certificates for ${DOMAIN}..."

# 1. Generate new certificate
cert-manager request \
  --name erlmcp-tls-new \
  --namespace ${NAMESPACE} \
  --dns-names ${DOMAIN},*.${DOMAIN} \
  --issuer letsencrypt-prod \
  --wait

# 2. Verify new certificate
kubectl get secret erlmcp-tls-new -n ${NAMESPACE} -o json | \
  jq -r '.data."tls.crt"' | base64 -d | \
  openssl x509 -noout -dates

# 3. Update ingress
kubectl patch ingress erlmcp-ingress -n ${NAMESPACE} \
  --type=json \
  -p='[{"op": "replace", "path": "/spec/tls/0/secretName", "value": "erlmcp-tls-new"}]'

# 4. Update services
for svc in $(kubectl get svc -n ${NAMESPACE} -o name); do
  kubectl patch ${svc} -n ${NAMESPACE} \
    -p '{"metadata":{"annotations":{"cert-manager.io/alt-name":"erlmcp-tls-new"}}}'
done

# 5. Verify
curl -v https://${DOMAIN}/health 2>&1 | grep "issuer="

# 6. Clean up old certificate (after 24 hours)
# kubectl delete secret erlmcp-tls-old -n ${NAMESPACE}

echo "Certificate rotation complete"
```

### 12.2 Node Maintenance

```bash
#!/bin/bash
# scripts/maintenance/drain-node.sh

NODE=${1}

if [ -z "$NODE" ]; then
  echo "Usage: $0 <node-name>"
  exit 1
fi

echo "Draining node ${NODE}..."

# 1. Cordon the node (mark unschedulable)
kubectl cordon ${NODE}

# 2. Evict all pods (graceful shutdown)
kubectl drain ${NODE} \
  --ignore-daemonsets \
  --delete-local-data \
  --grace-period=120 \
  --timeout=5m

# 3. Verify pods moved
kubectl get pods -n erlmcp-prod -o wide

# 4. Perform maintenance
echo "Node drained. Perform maintenance now."
echo "Press enter when done..."
read

# 5. Uncordon the node
kubectl uncordon ${NODE}

echo "Node ${NODE} is back in service"
```

### 12.3 Database Maintenance

```bash
#!/bin/bash
# scripts/maintenance/db-maintenance.sh

TASK=${1:-vacuum}

case $TASK in
  vacuum)
    echo "Running VACUUM ANALYZE on all tables..."
    TABLES=$(kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -t -c "SELECT tablename FROM pg_tables WHERE schemaname = 'public';")

    for TABLE in $TABLES; do
      echo "Vacuuming ${TABLE}..."
      kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
        psql -U postgres -c "VACUUM ANALYZE ${TABLE};"
    done
    ;;

  reindex)
    echo "Reindexing all tables..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "REINDEX DATABASE erlmcp;"
    ;;

  analyze)
    echo "Analyzing all tables..."
    kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
      psql -U postgres -c "ANALYZE VERBOSE;"
    ;;

  vacuum-full)
    echo "Running VACUUM FULL (WARNING: locks tables)..."
    read -p "This will lock tables. Continue? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
        psql -U postgres -c "VACUUM FULL;"
    fi
    ;;
esac

echo "Database maintenance complete"
```

---

## 13. Appendices

### 13.1 Quick Reference

#### Common Commands

```bash
# Health check
curl -sf https://erlmcp.io/health | jq .

# Get pod status
kubectl get pods -n erlmcp-prod -l app=erlmcp

# View logs
kubectl logs -f deployment/erlmcp -n erlmcp-prod

# Port forward
kubectl port-forward svc/erlmcp 8080:8080 -n erlmcp-prod

# Exec into pod
kubectl exec -it <pod-name> -n erlmcp-prod -- bash

# Scale up
kubectl scale deployment erlmcp --replicas=5 -n erlmcp-prod

# Rollout restart
kubectl rollout restart deployment erlmcp -n erlmcp-prod

# Rollback
kubectl rollout undo deployment erlmcp -n erlmcp-prod
```

#### Environment Variables

```bash
# Required
export ERLMCP_ENV=production
export ERLMCP_REGION=us-east-1
export ERLMCP_DB_HOST=postgres
export ERLMCP_DB_NAME=erlmcp
export ERLMCP_DB_USER=erlmcp
export ERLMCP_DB_PASSWORD=<from-secret>
export ERLANG_COOKIE=<from-secret>
export JWT_SECRET=<from-secret>

# Optional
export OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
export LOG_LEVEL=info
export TRACE_ENABLED=true
```

### 13.2 Contact Information

| Role | Contact | Hours |
|------|---------|-------|
| On-Call Engineer | PagerDuty: erlmcp-oncall | 24/7 |
| Engineering Lead | eng-lead@erlmcp.io | Business hours |
| DevOps Team | devops@erlmcp.io | Business hours |
| Security Team | security@erlmcp.io | 24/7 for critical |
| Executive Team | exec@erlmcp.io | Business hours |

### 13.3 Useful Links

| Resource | URL |
|----------|-----|
| CI/CD Pipeline | https://github.com/seanchatmangpt/erlmcp/actions |
| Grafana Dashboard | https://grafana.erlmcp.io/d/erlmcp |
| Logs | https://logs.erlmcp.io |
| Traces | https://jaeger.erlmcp.io |
| Status Page | https://status.erlmcp.io |
| Documentation | https://docs.erlmcp.io |
| Runbooks | https://github.com/seanchatmangpt/erlmcp-runbooks |

### 13.4 Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 3.0.0 | Initial worldwide deployment runbook |

---

**Document Status**: Production Ready
**Next Review**: 2026-03-02
**Maintained By**: erlmcp DevOps Team
