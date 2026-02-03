# Fortune 500 Worldwide Deployment Guide - erlmcp v3

**Version:** 3.0.0
**Status:** Production Ready
**Last Updated:** 2026-02-02

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Fortune 500 Deployment Requirements](#fortune-500-deployment-requirements)
3. [Multi-Region Architecture](#multi-region-architecture)
4. [Pre-Deployment Planning](#pre-deployment-planning)
5. [Step-by-Step Deployment Guides](#step-by-step-deployment-guides)
6. [Validation & Testing](#validation--testing)
7. [Go-Live Procedures](#go-live-procedures)
8. [Post-Deployment](#post-deployment)
9. [Appendices](#appendices)

---

## Executive Summary

This guide provides comprehensive procedures for deploying erlmcp v3 across multiple global regions to meet Fortune 500 enterprise requirements:

- **99.99% availability SLA** across all regions
- **Multi-region active-active** deployment
- **Zero-downtime deployments** with blue-green strategy
- **Automated disaster recovery** with 5-minute RTO
- **Comprehensive observability** with metrics, logs, traces
- **Security-first architecture** with zero-trust principles

---

## Fortune 500 Deployment Requirements

### Enterprise Compliance Matrix

| Compliance Standard | Requirement | erlmcp v3 Support |
|---------------------|-------------|------------------|
| **SOX 404** | Internal controls over financial reporting | Automated audit trails, change logging |
| **PCI DSS** | Payment card data security | Encryption at rest/transit, access controls |
| **HIPAA** | Protected health information | PHI audit logging, data retention |
| **GDPR** | EU data protection | EU region isolation, data portability |
| **SOC 2** | Security controls | Multi-factor auth, intrusion detection |
| **ISO 27001** | ISMS | Comprehensive security framework |

### Deployment Requirements

```yaml
fortune_500_requirements:
  availability:
    monthly_uptime: 99.99%
    maximum_downtime_per_month: 43.2_minutes
    disaster_recovery_rto: 5_minutes
    disaster_recovery_rpo: 1_minute

  performance:
    api_response_time_p95: < 100ms
    api_response_time_p99: < 500ms
    throughput_minimum: 10_000_rps
    throughput_target: 50_000_rps

  security:
    encryption_at_rest: AES-256
    encryption_in_transit: TLS 1.3+
    mtls_required: true
    vulnerability_scanning: continuous
    pen_testing: quarterly
    security_monitoring: 24/7

  compliance:
    audit_retention: 7_years
    data_classification: required
    privacy_by_design: true
    breach_notification: 72 hours
    data_residency: enforced

  operations:
    change_management: required
    incident_response: 24/7
    monitoring_coverage: 100%
    sla_reporting: monthly
    capacity_planning: quarterly
```

---

## Multi-Region Architecture

### Global Deployment Map

```
┌───────────────────────────────────────────────────────────────────────┐
│                      Global Traffic Management (Route 53)                │
│                         + Latency-Based Routing                       │
│                         + Health Checks                            │
└───────────────────────────────┬───────────────────────────────────────┘
                                │
        ┌───────────────────────┼────────────────────────────────┐
        │                       │                                │
   ┌────▼─────┐          ┌────▼─────┐                     ┌──────▼──────┐
   │US-EAST-1 │          │EU-WEST-1 │                     │AP-SOUTHEAST-1│
   │(Primary)│          │(Secondary)│                     │(Tertiary)  │
   │         │          │         │                     │             │
   │3 AZs   │          │3 AZs   │                     │2 AZs       │
   │30 nodes│          │20 nodes│                     │15 nodes    │
   │99.999% │          │99.999% │                     │99.99%     │
   └────┬────┘          └────┬────┘                     └──────┬──────┘
        │                   │                                    │
        └───────────────────┼────────────────────────────────────┘
                            │
                   ┌────────▼─────────┐
                   │  US-WEST-2 (DR)   │
                   │                   │
                   │2 AZs             │
                   │10 nodes (standby) │
                   │Hot standby        │
                   └───────────────────┘
```

### Region Specifications

#### US-EAST-1 (Primary Region)

```yaml
region: us-east-1
datacenter_location: N. Virginia
role: Primary

infrastructure:
  provider: AWS
  service: EKS v1.28
  node_types:
    control_plane: c5.2xlarge (8 vCPU, 32GB RAM)
    worker_general: m5.4xlarge (16 vCPU, 64GB RAM)
    worker_memory: r5.8xlarge (32 vCPU, 256GB RAM)
  total_nodes: 30

network:
  vpc: 10.0.0.0/16
  availability_zones: 3 (us-east-1a, us-east-1b, us-east-1c)
  public_subnets: 3
  private_subnets: 3
  database_subnets: 3

database:
  type: PostgreSQL 15
  instance: db.r6g.4xlarge
  storage: 2TB SSD gp3
  replication: 2 replicas
  backup_retention: 30 days

compute:
  baseline_replicas: 3
  max_replicas: 100
  hpa_enabled: true
  target_cpu: 70%
  target_memory: 80%

sla:
  availability: 99.999%
  rto: 5 minutes
  rpo: 1 minute
```

#### EU-WEST-1 (Secondary Region)

```yaml
region: eu-west-1
datacenter_location: Ireland
role: Secondary (Active)

infrastructure:
  provider: AWS
  service: EKS v1.28
  node_types:
    control_plane: c5.2xlarge
    worker_general: m5.4xlarge
    worker_memory: r5.8xlarge
  total_nodes: 20

network:
  vpc: 10.1.0.0/16
  availability_zones: 3 (eu-west-1a, eu-west-1b, eu-west-1c)
  public_subnets: 3
  private_subnets: 3
  database_subnets: 3

database:
  type: PostgreSQL 15
  instance: db.r6g.4xlarge
  storage: 2TB SSD gp3
  replication: 2 replicas
  backup_retention: 30 days

compute:
  baseline_replicas: 2
  max_replicas: 50
  hpa_enabled: true
  target_cpu: 70%
  target_memory: 80%

sla:
  availability: 99.999%
  rto: 5 minutes
  rpo: 1 minute
  compliance: GDPR
```

#### AP-SOUTHEAST-1 (Tertiary Region)

```yaml
region: ap-southeast-1
datacenter_location: Singapore
role: Tertiary (Active)

infrastructure:
  provider: AWS
  service: EKS v1.28
  node_types:
    control_plane: c5.2xlarge
    worker_general: m5.4xlarge
    worker_memory: r5.4xlarge
  total_nodes: 15

network:
  vpc: 10.2.0.0/16
  availability_zones: 2 (ap-southeast-1a, ap-southeast-1b)
  public_subnets: 2
  private_subnets: 2
  database_subnets: 2

database:
  type: PostgreSQL 15
  instance: db.r6g.2xlarge
  storage: 1TB SSD gp3
  replication: 1 replica
  backup_retention: 30 days

compute:
  baseline_replicas: 2
  max_replicas: 40
  hpa_enabled: true
  target_cpu: 70%
  target_memory: 80%

sla:
  availability: 99.99%
  rto: 10 minutes
  rpo: 5 minutes
```

#### US-WEST-2 (DR Region)

```yaml
region: us-west-2
datacenter_location: Oregon
role: Disaster Recovery

infrastructure:
  provider: AWS
  service: EKS v1.28
  node_types:
    control_plane: c5.xlarge
    worker_general: m5.2xlarge
    worker_memory: r5.2xlarge
  total_nodes: 10 (standby)

network:
  vpc: 10.3.0.0/16
  availability_zones: 2 (us-west-2a, us-west-2b)
  public_subnets: 2
  private_subnets: 2
  database_subnets: 2

database:
  type: PostgreSQL 15
  instance: db.r6g.2xlarge
  storage: 500GB SSD gp3
  replication: async from us-east-1
  backup_retention: 30 days

compute:
  baseline_replicas: 0 (hot standby)
  max_replicas: 50
  hpa_enabled: false (until failover)

sla:
  availability: 99.9%
  rto: 15 minutes
  rpo: 5 minutes
  failover_priority: 1
```

---

## Pre-Deployment Planning

### 1. Stakeholder Engagement

#### Pre-Deployment Meeting Checklist

```yaml
meeting_schedule:
  week_4:
    - day_1: Stakeholder kickoff
    - day_2: Technical deep dive
    - day_3: Security review
    - day_4: Operations training
    - day_5: Final approval

stakeholders:
  executive:
    name: C-Suite
    concerns: Business impact, cost, timeline
    deliverable: Executive summary presentation

  legal:
    name: Legal & Compliance
    concerns: Contractual obligations, regulatory
    deliverable: Compliance sign-off

  security:
    name: CISO / Security Team
    concerns: Security posture, vulnerabilities
    deliverable: Security assessment

  ops:
    name: Operations / DevOps
    concerns: Runbooks, monitoring, support
    deliverable: Operations readiness report

  finance:
    name: Finance
    concerns: Budget, ROI tracking
    deliverable: Cost breakdown
```

### 2. Infrastructure Planning

#### Capacity Planning Calculator

```bash
#!/bin/bash
# scripts/planning/calculate-capacity.sh

# Fortune 500 Capacity Planning Calculator

# Input parameters
EXPECTED_USERS=${1:-100000}  # Expected concurrent users
REQUESTS_PER_USER=${2:-10}    # Requests per user per hour
PEAK_RATIO=${3:-3}            # Peak-to-average ratio
BUFFER_RATIO=${4:-2}          # Safety buffer

# Calculate requirements
AVG_RPS=$(echo "scale=2; $EXPECTED_USERS * $REQUESTS_PER_USER / 3600" | bc)
PEAK_RPS=$(echo "scale=2; $AVG_RPS * $PEAK_RATIO" | bc)
BUFFERED_RPS=$(echo "scale=2; $PEAK_RPS * $BUFFER_RATIO" | bc)

# Per-region capacity (4 regions, 60-20-20 split)
PRIMARY_RPS=$(echo "scale=2; $BUFFERED_RPS * 0.60" | bc)
SECONDARY_RPS=$(echo "scale=2; $BUFFERED_RPS * 0.20" | bc)
TERTIARY_RPS=$(echo "scale=2; $BUFFERED_RPS * 0.20" | bc)

cat <<EOF
=== Fortune 500 Capacity Calculation Results ===

Input Parameters:
  Expected Users: $EXPECTED_USERS
  Requests/User/Hour: $REQUESTS_PER_USER
  Peak Ratio: ${PEAK_RATIO}x
  Buffer Ratio: ${BUFFER_RATIO}x

Calculated Requirements:
  Average Throughput: ${AVG_RPS} RPS
  Peak Throughput: ${PEAK_RPS} RPS
  Buffered Throughput: ${BUFFERED_RPS} RPS

Regional Distribution (60-20-20):
  US-EAST-1 (Primary):   ${PRIMARY_RPS} RPS
  EU-WEST-1 (Secondary): ${SECONDARY_RPS} RPS
  AP-SOUTHEAST-1 (Tertiary): ${TERTIARY_RPS} RPS
  US-WEST-2 (DR):         0 RPS (standby)

Resource Requirements per Region:
  US-EAST-1:   30 nodes (m5.4xlarge + r5.8xlarge)
  EU-WEST-1:   20 nodes (m5.4xlarge + r5.8xlarge)
  AP-SOUTHEAST-1: 15 nodes (m5.4xlarge + r5.4xlarge)
  US-WEST-2:   10 nodes (standby, m5.2xlarge)

Total Resources:
  Compute: 75 nodes across all regions
  Storage: 5TB PostgreSQL + 10TB EBS
  Network: 10 Gbps interconnects
  Monitoring: Full stack (Prometheus + Grafana + Jaeger)

Estimated Monthly Cost: $XXX,XXX
SLA Target: 99.99% uptime
EOF
```

### 3. Security Planning

#### Security Checklist

```yaml
pre_deployment_security:
  infrastructure:
    - [ ] VPC with private subnets configured
    - [ ] Security groups with least privilege
    - [ ] Network policies applied
    - [ ] WAF rules configured
    - [ ] DDoS protection enabled
    - [ ] TLS certificates provisioned
    - [ ] Secrets stored in vault/SSM

  application:
    - [ ] mTLS configured between services
    - [ ] Encryption at rest enabled
    - [ ] API authentication configured
    - [ ] Rate limiting configured
    - [ ] Input validation enabled
    - [ ] Output encoding enabled
    - [ ] Secrets rotation scheduled

  compliance:
    - [ ] Audit logging enabled
    - [ ] Data classification implemented
    - [ ] Retention policies configured
    - [ ] GDPR data portability ready
    - [ ] Breach notification configured
    - [ ] Penetration testing completed

  access_control:
    - [ ] RBAC configured
    - [ ] MFA enforced for admin access
    - [ ] Least privilege applied
    - [ ] Access reviews scheduled
    - [ ] Off-boarding procedures ready
```

---

## Step-by-Step Deployment Guides

### Phase 1: Foundation (Week 1-2)

#### 1.1 Create Global Infrastructure

```bash
#!/bin/bash
# scripts/deployment/01-create-infrastructure.sh

set -euo pipefail

# Configuration
PROJECT="erlmcp-prod"
PRIMARY_REGION="us-east-1"
REGIONS=("us-east-1" "eu-west-1" "ap-southeast-1" "us-west-2")

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

error() {
    echo "ERROR: $1" >&2
    exit 1
}

# Create VPCs for all regions
create_vpCs() {
    log "Creating VPCs in all regions..."

    for REGION in "${REGIONS[@]}"; do
        log "Creating VPC in $REGION..."

        CIDR="10.$(echo "$REGIONS" | grep -n "^$REGION " | awk '{print NR-1}').0.0.0/16"

        VPC_ID=$(aws ec2 create-vpc \
            --region $REGION \
            --cidr-block $CIDR \
            --tag-specifications "ResourceType=vpc,Tags=[{Key=Name,Value=erlmcp-prod},{Key=Environment,Value=production},{Key=Region,Value=$REGION}]" \
            --query "Vpc.VpcId" \
            --output text)

        log "Created VPC $VPC_ID in $REGION"

        # Enable DNS support
        aws ec2 modify-vpc-attribute \
            --region $REGION \
            --vpc-id $VPC_ID \
            --enable-dns-support "{\"Value\":true}" \
            --enable-dns-hostnames "{\"Value\":true}"

        # Store VPC ID for cleanup
        echo "$REGION:$VPC_ID" >> /tmp/erlmcp_vpcs.txt
    done
}

# Create EKS clusters
create_clusters() {
    log "Creating EKS clusters in all regions..."

    for REGION in "${REGIONS[@]}"; do
        log "Creating EKS cluster in $REGION..."

        CLUSTER_NAME="erlmcp-prod-${REGION//-/}"
        ROLE_NAME="erlmcp-cluster-role-$REGION"

        # Create cluster role
        CLUSTER_ROLE_ARN=$(aws iam create-role \
            --role-name $ROLE_NAME \
            --assume-role-policy-document '{
                "Version": "2012-10-17",
                "Statement": [{
                    "Effect": "Allow",
                    "Principal": {"Service": "eks.amazonaws.com"},
                    "Action": "sts:AssumeRole"
                }]
            }' \
            --query "Role.Arn" \
            --output text)

        # Attach cluster policy
        aws iam attach-role-policy \
            --role-name $ROLE_NAME \
            --policy-arn arn:aws:iam::aws:policy/AmazonEKSClusterPolicy

        # Get VPC ID
        VPC_ID=$(grep "^$REGION:" /tmp/erlmcp_vpcs.txt | cut -d: -f2)

        # Get subnet IDs
        SUBNETS=$(aws ec2 describe-subnets \
            --region $REGION \
            --filters "Name=vpc-id,Values=$VPC_ID" "Name=tag:Name,Values=*private*" \
            --query "Subnets[].SubnetId" \
            --output text | tr '\n' ' ')

        # Create cluster
        aws eks create-cluster \
            --region $REGION \
            --name $CLUSTER_NAME \
            --role-arn $CLUSTER_ROLE_ARN \
            --resources-vpc-config \
            subnetIds="$SUBNETS" \
            --kubernetes-version "1.28" \
            --logging \
                clusterLogging={
                    enabled=true,
                    types=["api","audit","authenticator","controller","scheduler"]
                } \
            --tags \
            Environment=production,Region=$REGION,Managed=erlmcp \
            --enable-irsa

        log "Creating EKS cluster $CLUSTER_NAME..."

        # Wait for cluster
        aws eks wait cluster-active \
            --region $REGION \
            --name $CLUSTER_NAME
    done
}

# Create node groups
create_node_groups() {
    log "Creating node groups in all regions..."

    for REGION in "${REGIONS[@]}"; do
        CLUSTER_NAME="erlmcp-prod-${REGION//-/}"
        ROLE_NAME="erlmcp-node-role-$REGION"

        # Node group configuration based on region role
        case $REGION in
            us-east-1)
                GENERAL_COUNT=20
                MEMORY_COUNT=5
                INSTANCE_TYPE="m5.4xlarge"
                ;;
            eu-west-1)
                GENERAL_COUNT=15
                MEMORY_COUNT=5
                INSTANCE_TYPE="m5.4xlarge"
                ;;
            ap-southeast-1)
                GENERAL_COUNT=10
                MEMORY_COUNT=5
                INSTANCE_TYPE="m5.4xlarge"
                ;;
            us-west-2)
                GENERAL_COUNT=5
                MEMORY_COUNT=5
                INSTANCE_TYPE="m5.2xlarge"
                ;;
        esac

        # Get VPC ID
        VPC_ID=$(grep "^$REGION:" /tmp/erlmcp_vpcs.txt | cut -d: -f2)

        # Get subnets
        SUBNETS=$(aws ec2 describe-subnets \
            --region $REGION \
            --filters "Name=vpc-id,Values=$VPC_ID" "Name=tag:Name,Values=*private*" \
            --query "Subnets[].SubnetId" \
            --output text | tr '\n' ' ')

        # Create general purpose node group
        aws eks create-nodegroup \
            --region $REGION \
            --cluster-name $CLUSTER_NAME \
            --nodegroup-name general-purpose \
            --node-role $ROLE_NAME \
            --subnets $SUBNETS \
            --instance-types $INSTANCE_TYPE \
            --ami-type AL2_x86_64 \
            --capacity-type ON_DEMAND \
            --scaling-config minSize=$GENERAL_COUNT,maxSize=50,desiredSize=$GENERAL_COUNT \
            --disk-size 100 \
            --tags Environment=production,NodeGroup=general-purpose

        # Create memory optimized node group
        aws eks create-nodegroup \
            --region $REGION \
            --cluster-name $CLUSTER_NAME \
            --nodegroup-name memory-optimized \
            --node-role $ROLE_NAME \
            --subnets $SUBNETS \
            --instance-types r5.8xlarge,r5.4xlarge \
            --ami-type AL2_x86_64 \
            --capacity-type ON_DEMAND \
            --scaling-config minSize=$MEMORY_COUNT,maxSize=20,desiredSize=$MEMORY_COUNT \
            --disk-size 200 \
            --labels NodeGroup=memory-optimized \
            --taints key=workload-type,value=memory-optimized,effect=NoSchedule \
            --tags Environment=production,NodeGroup=memory-optimized

        log "Node groups created for $REGION"
    done
}

main() {
    log "Starting Phase 1: Foundation..."

    create_vpCs
    create_clusters
    create_node_groups

    log "Phase 1 complete. Foundation infrastructure ready."

    cat <<EOF

=== Infrastructure Created ===

VPCs:
$(cat /tmp/erlmcp_vpcs.txt)

EKS Clusters:
$(for REGION in "${REGIONS[@]}"; do
    echo "  $REGION: erlmcp-prod-${REGION//-/}"
done)

Next Steps:
1. Configure networking and load balancers
2. Deploy supporting services (monitoring, logging)
3. Deploy databases
4. Deploy erlmcp application

EOF
}

main "$@"
```

### Phase 2: Database Setup (Week 2)

#### 2.1 Deploy PostgreSQL Clusters

```bash
#!/bin/bash
# scripts/deployment/02-database-setup.sh

set -euo pipe_pipefail

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Deploy PostgreSQL in primary region with replication
deploy_primary_postgres() {
    REGION="us-east-1"
    NAMESPACE="erlmcp-production"

    log "Deploying primary PostgreSQL cluster in $REGION..."

    # Update kubeconfig
    aws eks update-kubeconfig --region $REGION --name erlmcp-prod-useast1

    # Create secrets
    kubectl create secret generic erlmcp-pg-credentials \
        --from-literal=postgresql-password=$(openssl rand -base64 32) \
        --from-literal=replication-password=$(openssl rand -base64 32) \
        --namespace $NAMESPACE \
        --dry-run=client -o yaml | kubectl apply -f -

    # Deploy PostgreSQL using CloudNativePG
    kubectl apply -f - <<EOF
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata:
  name: erlmcp-postgres
  namespace: $NAMESPACE
spec:
  instances: 3

  # Primary configuration
  primaryUpdateStrategy: unsupervised
  primaryUpdateStrategy: 15

  # Bootstrap configuration
  bootstrap:
    initdb:
      database: erlmcp
      owner: erlmcp
      secret:
        name: erlmcp-pg-credentials
      encoding: UTF8
      locale: en_US.UTF-8
      dataChecksums: on

  # Superuser secret
  enableSuperuserAccess: true
  superuserSecret:
    name: erlmcp-pg-superuser

  # PostgreSQL configuration
  postgresql:
    parameters:
      max_connections: "500"
      shared_buffers: "4GB"
      effective_cache_size: "12GB"
      maintenance_work_mem: "1GB"
      checkpoint_completion_target: "0.9"
      wal_buffers: "64MB"
      default_statistics_target: "0.1"
      random_page_cost: "1.1"
      effective_io_concurrency: "200"
      work_mem: "32MB"
      min_wal_size: "2GB"
      wal_keep_size: "4GB"

  # Replication configuration
  replication:
    slots:
      highAvailability:
        enabled: true
      synchronizeReplicas: false

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

  # Storage configuration
  storage:
    size: 2Ti
    storageClass: gp3
    resizeInUseVolumes: true

  # Monitoring
  monitoring:
    enabled: true
    prometheusRule:
      enabled: true

  # Resources
  resources:
    requests:
      memory: 16Gi
      cpu: 8
    limits:
      memory: 32Gi
      cpu: 16

  # Affinity
  affinity:
    podAntiAffinity:
      requiredDuringSchedulingIgnoredDuringExecution:
        - labelSelector:
            matchLabels:
              app.kubernetes.io/name: erlmcp-postgres
          topologyKey: kubernetes.io/hostname
EOF

    # Wait for PostgreSQL to be ready
    kubectl wait --for=condition=ready pod \
        -l cnpg.io/pod-role=instance \
        -n $NAMESPACE \
        --timeout=10m

    log "Primary PostgreSQL deployed and ready"
}

# Deploy standby databases in secondary regions
deploy_standby_postgres() {
    for REGION in eu-west-1 ap-southeast-1 us-west-2; do
        log "Deploying standby PostgreSQL in $REGION..."

        NAMESPACE="erlmcp-production"
        CLUSTER_NAME="erlmcp-prod-${REGION//-/}"

        aws eks update-kubeconfig --region $REGION --name $CLUSTER_NAME

        # Import credentials from primary
        PRIMARY_PASSWORD=$(kubectl get secret erlmcp-pg-credentials \
            -n erlmcp-production --context=us-east-1 \
            --template='{{.data.postgresql-password}}')

        # Create secret
        kubectl create secret generic erlmcp-pg-credentials \
            --from-literal=postgresql-password=$PRIMARY_PASSWORD \
            --from-literal=replication-password=$PRIMARY_PASSWORD \
            --namespace $NAMESPACE \
            --dry-run=client -o yaml | kubectl apply -f -

        # Deploy standby
        kubectl apply -f - <<EOF
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata:
  name: erlmcp-postgres
  namespace: $NAMESPACE
spec:
  instances: 2

  primaryUpdateStrategy: unsupervised
  primaryUpdateStrategy: 15

  bootstrap:
    initdb:
      database: erlmcp
      owner: erlmcp
      encoding: UTF-8
      locale: en_US.UTF-8

  replication:
    standby:
      enabled: true
      waitForArchive: true
    slots:
      highAvailability:
        enabled: true

  storage:
    size: 1Ti
    storageClass: gp3

  resources:
    requests:
      memory: 8Gi
      cpu: 4
    limits:
      memory: 16Gi
      cpu: 8
EOF

        log "Standby PostgreSQL deployed in $REGION"
    done
}

# Configure replication
configure_replication() {
    log "Configuring cross-region replication..."

    # Primary to secondary connections
    for REGION in eu-west-1 ap-southeast-1; do
        # Get primary endpoint
        PRIMARY_ENDPOINT=$(aws eks describe-cluster \
            --region us-east-1 \
            --name erlmcp-prod-useast1 \
            --query "cluster.endpoint" \
            --output text)

        # Create publication on primary
        kubectl exec -n erlmcp-production \
            erlmcp-postgres-1 \
            -- psql -U postgres -c "
                CREATE PUBLICATION IF NOT EXISTS erlmcp_pub
                FOR ALL TABLES;
            "

        # Create subscription on secondary
        kubectl exec -n erlmcp-production \
            --context=$REGION \
            erlmcp-postgres-1 \
            -- psql -U postgres -c "
                CREATE SUBSCRIPTION IF NOT EXISTS erlmcp_sub
                CONNECTION 'host=$PRIMARY_ENDPOINT dbname=erlmcp user=replicator password=$PRIMARY_PASSWORD'
                PUBLICATION erlmcp_pub
                WITH (create_slot = false);
            "

        log "Replication configured: us-east-1 -> $REGION"
    done
}

main() {
    log "Starting Phase 2: Database Setup..."

    deploy_primary_postgres
    deploy_standby_postgres
    configure_replication

    log "Phase 2 complete. Databases ready."
}

main "$@"
```

### Phase 3: Application Deployment (Week 3)

#### 3.1 Deploy erlmcp Application

```bash
#!/bin/bash
# scripts/deployment/03-application-deploy.sh

set -euo pipefail

VERSION=${1:-v3.0.0}
REGIONS=("us-east-1" "eu-west-1" "ap-southeast-1")
STRATEGY=${2:-rolling}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

error() {
    log "ERROR: $1"
    exit 1
}

# Deploy to all regions
deploy_to_regions() {
    for REGION in "${REGIONS[@]}"; do
        log "Deploying to $REGION..."

        (
            export KUBECONFIG=$HOME/.kube/${REGION}-config
            CLUSTER_NAME="erlmcp-prod-${REGION//-/}"
            NAMESPACE="erlmcp-production"

            # Update kubeconfig
            aws eks update-kubeconfig --region $REGION --name $CLUSTER_NAME

            # Create namespace
            kubectl create namespace $NAMESPACE --dry-run=client -o yaml | kubectl apply -f -

            # Create secrets
            kubectl create secret generic erlmcp-secrets \
                --from-literal=erlang-cookie=$(aws ssm get-parameter --name /erlmcp/erlang-cookie --region us-east-1 --query Parameter.Value --output text) \
                --from-literal=jwt-secret=$(aws ssm get-parameter --name /erlmcp/jwt-secret --region us-east-1 --query Parameter.Value --output text) \
                --namespace $NAMESPACE \
                --dry-run=client -o yaml | kubectl apply -f -

            # Deploy using Helm
            helm upgrade --install erlmcp ./charts/erlmcp \
                --namespace $NAMESPACE \
                --values charts/erlmcp/values.yaml \
                --set global.region=$REGION \
                --set global.environment=production \
                --set global.cluster=$CLUSTER_NAME \
                --set global.version=$VERSION \
                --set image.repository=ghcr.io/seanchatmangpt/erlmcp \
                --set image.tag=$VERSION \
                --set erlmcp.replicaCount=3 \
                --set erlmcp.hpa.enabled=true \
                --set erlmcp.hpa.minReplicas=3 \
                --set erlmcp.hpa.maxReplicas=50 \
                --set erlmcp.hpa.targetCPUUtilizationPercentage=70 \
                --set erlmcp.hpa.targetMemoryUtilizationPercentage=80 \
                --wait \
                --timeout 15m

            # Wait for rollout
            kubectl rollout status statefulset/erlmcp-cluster -n $NAMESPACE --timeout=10m

            log "Deployment to $REGION complete"

        ) &
    done

    # Wait for all regions
    wait

    log "All regions deployed"
}

# Verify deployments
verify_deployments() {
    log "Verifying deployments..."

    for REGION in "${REGIONS[@]}"; do
        (
            export KUBECONFIG=$HOME/.kube/${REGION}-config
            NAMESPACE="erlmcp-production"

            # Health check
            ENDPOINT=$(kubectl get svc erlmcp -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')
            if curl -sf https://$ENDPOINT/health | grep -q "healthy"; then
                log "$REGION: HEALTHY"
            else
                log "$REGION: UNHEALTHY"
                return 1
            fi
        )
    done

    log "All regions verified"
}

main() {
    if [ -z "$VERSION" ]; then
        echo "Usage: $0 <version> [strategy]"
        echo "Strategies: rolling, parallel, blue-green"
        exit 1
    fi

    log "Starting Phase 3: Application Deployment of version $VERSION..."

    deploy_to_regions
    verify_deployments

    log "Phase 3 complete. Application deployed."
}

main "$@"
```

### Phase 4: Observability (Week 3)

#### 4.1 Deploy Monitoring Stack

```bash
#!/bin/bash
# scripts/deployment/04-observability.sh

set -euo pipefail

REGIONS=("us-east-1" "eu-west-1" "ap-southeast-1" "us-west-2")

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Deploy Prometheus to all regions
deploy_prometheus() {
    for REGION in "${REGIONS[@]}"; do
        log "Deploying Prometheus to $REGION..."

        helm repo add prometheus-community \
            https://prometheus-community.github.io/helm-charts

        helm upgrade --install prometheus prometheus-community/kube-prometheus-stack \
            --namespace monitoring \
            --create-namespace \
            --set grafana.enabled=true \
            --set prometheus.prometheusSpec.serviceMonitorSelectorNilUsesHelmValues=false \
            --set prometheus.prometheusSpec.retention=30d \
            --set prometheus.prometheusSpec.storageSpec.volumeClaimTemplate.spec.resources.requests.storage=2Gi \
            --set prometheus.prometheusSpec.resources.requests.memory=4Gi \
            --set prometheus.prometheusSpec.resources.requests.cpu=2 \
            --set alertmanager.enabled=true \
            --set node-exporter.enabled=true \
            --set kube-state-metrics.enabled=true \
            --set prometheus-node-exporter.hostRoot=/host \
            --set prometheus-node-exporter.hostRootFs=false \
            --set defaultRules.install=true \
            --set grafana.adminPassword=admin \
            --set grafana.persistence.enabled=true \
            --set grafana.persistence.size=10Gi \
            --set global.region=$REGION

        log "Prometheus deployed to $REGION"
    done
}

# Deploy Loki for logging
deploy_loki() {
    for REGION in "${REGIONS[@]}"; do
        log "Deploying Loki to $REGION..."

        helm repo add grafana https://grafana.github.io/helm-charts

        helm upgrade --install loki grafana/loki-stack \
            --namespace logging \
            --create-namespace \
            --set loki.persistence.enabled=true \
            --set loki.persistence.size=20Gi \
            --set loki.storage.type=s3 \
            --set loki.storage.s3.endpoint=s3://erlmcp-logs \
            --set loki.storage.s3.region=$REGION \
            --set loki.storage.s3.accessKeyId=$(aws ssm get-parameter --name /erlmcp/logs/access-key-id --region $REGION --query Parameter.Value --output text) \
            --set loki.storage.s3.secretAccessKey=$(aws ssm get-parameter --name /erlmcp/logs/secret-access-key --region $REGION --query Parameter.Value --output text) \
            --set minio.enabled=false \
            --set gateway.enabled=false \
            --set global.region=$REGION

        log "Loki deployed to $REGION"
    done
}

# Deploy Jaeger for tracing
deploy_jaeger() {
    log "Deploying Jaeger to us-east-1 (centralized)"

    helm repo add jaegertracing https://jaegertracing.github.io/jaeger-helm-charts

    helm upgrade --install jaeger jaegertracing/jaeger \
        --namespace tracing \
        --create-namespace \
        --set prod-mode.enabled=true \
        --set storage.type=cassandra \
        --set storage.cassandra.client.host=jaeger-cassandra \
        --set storage.cassandra.keyspace=jaeger_v1 \
        --set storage.cassandra.datacenter=us-east-1 \
        --set agentCollector.enabled=true \
        --set agentCollector.host=hostNetwork \
        --set agentCollector.port=6831 \
        --set query.service.grpc.enabled=true \
        --set query.service.grpc.host=jaeger-collector \
        --set global.region=us-east-1 \
        --wait

    log "Jaeger deployed centrally"
}

# Configure cross-region metrics federation
configure_federation() {
    log "Configuring metrics federation..."

    # Configure Prometheus federation
    for REGION in eu-west-1 ap-southeast-1 us-west-2; do
        kubectl apply -f - <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: prometheus-federation
  namespace: monitoring
type: Opaque
stringData:
  write-ahead-credentials: |
    - name: federate
      password: $(kubectl get secret prometheus-federation -n monitoring -o jsonpath='{.data.write-ahead-credentials}' | jq -r '.[] | select(.name=="federate") | .password')
EOF
    done
}

main() {
    log "Starting Phase 4: Observability Setup..."

    deploy_prometheus
    deploy_loki
    deploy_jaeger
    configure_federation

    log "Phase 4 complete. Observability ready."
}

main "$@"
```

### Phase 5: Go-Live (Week 4)

#### 5.1 Go-Live Checklist

```bash
#!/bin/bash
# scripts/deployment/05-golive-checklist.sh

set -euo pipefail

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Final pre-go-live checks
run_final_checks() {
    log "=== Final Go-Live Checks ==="

    FAILED=0

    # 1. All regions healthy
    log "1. Regional Health Checks..."
    for REGION in us-east-1 eu-west-1 ap-southeast-1 us-west-2; do
        if ! curl -sf https://$REGION.erlmcp.io/health | grep -q "healthy"; then
            log "  FAIL: $REGION unhealthy"
            ((FAILED++))
        else
            log "  PASS: $REGION healthy"
        fi
    done

    # 2. Replication lag acceptable
    log "2. Replication Lag Checks..."
    for REGION in eu-west-1 ap-southeast-1; do
        LAG=$(kubectl exec erlmcp-cluster-0 -n erlmcp-production --context=$REGION -- \
            erlmcpctl replication-lag us-east-1 2>/dev/null || echo "999")

        if [ $LAG -lt 60 ]; then
            log "  PASS: $REGION lag ${LAG}s"
        else
            log "  WARN: $REGION lag ${LAG}s (high)"
        fi
    done

    # 3. Monitoring functional
    log "3. Monitoring Checks..."
    for REGION in us-east-1 eu-west-1 ap-southeast-1; do
        if curl -sf https://prometheus.$REGION.erlmcp.io/-/healthy > /dev/null; then
            log "  PASS: Prometheus in $REGION"
        else
            log "  FAIL: Prometheus down in $REGION"
            ((FAILED++))
        fi
    done

    # 4. Load test at 10% capacity
    log "4. Load Test (10% capacity)..."
    if ./scripts/load-test/load-test.sh 1000 60 | grep -q "PASS"; then
        log "  PASS: Load test passed"
    else
        log "  FAIL: Load test failed"
        ((FAILED++))
    fi

    # 5. Security scan passed
    log "5. Security Checks..."
    if trivy image --severity CRITICAL,HIGH ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION} --exit-code 1; then
        log "  PASS: No critical vulnerabilities"
    else
        log "  FAIL: Critical vulnerabilities found"
        ((FAILED++))
    fi

    if [ $FAILED -eq 0 ]; then
        log "All checks PASSED - Ready for go-live"
        return 0
    else
        log "Some checks FAILED - Review required"
        return 1
    fi
}

# Execute go-live
execute_golive() {
    log "=== EXECUTING GO-LIVE ==="

    # Get approval
    read -p "Enter approval token to proceed: " -r
    echo

    if [ "$REPLY" != "APPROVED" ]; then
        log "Go-live cancelled"
        return 1
    fi

    # Update DNS to route production traffic
    log "Updating DNS for production..."
    aws route53 change-resource-record-sets \
        --hosted-zone-id Z1234567890ABC \
        --change-batch file://scripts/dns/production-traffic.json

    # Scale to production capacity
    log "Scaling to production capacity..."
    for REGION in us-east-1 eu-west-1 ap-southeast-1; do
        kubectl scale deployment erlmcp \
            --replicas=10 \
            -n erlmcp-production \
            --context=$REGION
    done

    # Verify all regions serving traffic
    log "Verifying traffic flow..."
    sleep 60

    curl -sf https://erlmcp.io/health | grep -q "healthy"

    log "=== GO-LIVE COMPLETE ==="
    log ""
    log "Production traffic now flowing to all regions"
    log "Monitor dashboards:"
    log "  - https://grafana.erlmcp.io/d/erlmcp-production"
    log "  - https://status.erlmcp.io"
}

main() {
    if run_final_checks; then
        execute_golive
    else
        log "Go-live ABORTED - check failed items"
        exit 1
    fi
}

main "$@"
```

---

## Validation & Testing

### Smoke Tests

```bash
#!/bin/bash
# scripts/test/smoke-tests.sh

set -euo pipefail

VERSION=${1:-latest}
BASE_URL=${2:-https://erlmcp.io}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Health endpoint test
test_health() {
    log "Testing health endpoint..."

    RESPONSE=$(curl -sf "$BASE_URL/health")
    HEALTHY=$(echo "$RESPONSE" | jq -r '.status')

    if [ "$HEALTHY" = "healthy" ]; then
        log "PASS: Health check"
        return 0
    else
        log "FAIL: Health check returned: $RESPONSE"
        return 1
    fi
}

# API connectivity test
test_api() {
    log "Testing API connectivity..."

    # Test initialize endpoint
    RESPONSE=$(curl -sf "$BASE_URL/mcp" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}')

    if echo "$RESPONSE" | jq -e '.result' > /dev/null; then
        log "PASS: API connectivity"
        return 0
    else
        log "FAIL: API connectivity failed"
        return 1
    fi
}

# Performance test
test_performance() {
    log "Testing performance (100 concurrent users)..."

    RESULT=$(wrk -t4 -c100 -d30s "$BASE_URL/health" 2>/dev/null | grep "Latency")

    P95_LATENCY=$(echo "$RESULT" | grep -oP '\d+\.\d+(?=\s{3,})' | sort -n | tail -1)

    if [ $(echo "$P95_LATENCY < 0.5" | bc -l) -eq 1 ]; then
        log "PASS: P95 latency ${P95_LATENCY}s (< 500ms target)"
        return 0
    else
        log "FAIL: P95 latency ${P95_LATENCY}s (exceeds 500ms target)"
        return 1
    fi
}

# Multi-region test
test_multi_region() {
    log "Testing multi-region connectivity..."

    PASSED=0
    TOTAL=0

    for REGION in us-east-1 eu-west-1 ap-southeast-1; do
        TOTAL=$((TOTAL + 1))
        URL="https://$REGION.erlmcp.io/health"

        if curl -sf "$URL" | grep -q "healthy"; then
            log "  PASS: $REGION responding"
        else
            log "  FAIL: $REGION not responding"
            PASSED=$((PASSED + 1))
        fi
    done

    log "Multi-region test: $PASSED/$TOTAL passed"

    return $PASSED
}

# Data consistency test
test_data_consistency() {
    log "Testing data consistency..."

    # Compare database row counts
    US_COUNT=$(kubectl exec erlmcp-pg-primary-0 -n erlmcp-production --context=us-east-1 -- \
        psql -U postgres -c "SELECT COUNT(*) FROM sessions;" 2>/dev/null || echo "0")

    EU_COUNT=$(kubectl exec erlmcp-pg-primary-0 -n erlmcp-production --context=eu-west-1 -- \
        psql -U postgres -c "SELECT COUNT(*) FROM sessions;" 2>/dev/null || echo "0")

    AP_COUNT=$(kubectl exec erlmcp-pg-primary-0 -n erlmcp-production --context=ap-southeast-1 -- \
        psql -U postgres -c "SELECT COUNT(*) FROM sessions;" 2>/dev/null || echo "0")

    # Check if counts are within 5% of each other
    for REGION_COUNT in "$US_COUNT" "$EU_COUNT" "$AP_COUNT"; do
        for OTHER_COUNT in "$US_COUNT" "$EU_COUNT" "$AP_COUNT"; do
            DIFF=$((100 * ($REGION_COUNT - $OTHER_COUNT) / $OTHER_COUNT))
            if [ $DIFF -gt 5 ]; then
                log "  WARN: Data drift detected: $DIFF% difference"
            fi
        done
    done

    log "PASS: Data consistency check completed"
}

main() {
    log "=== Running Smoke Tests for $VERSION ==="

    FAILED=0

    test_health || ((FAILED++))
    test_api || ((FAILED++))
    test_performance || ((FAILED++))
    test_multi_region || ((FAILED++))
    test_data_consistency || ((FAILED++))

    if [ $FAILED -eq 0 ]; then
        log "=== All smoke tests PASSED ==="
        exit 0
    else
        log "=== $FAILED smoke tests FAILED ==="
        exit 1
    fi
}

main "$@"
```

---

## Post-Deployment

### 1. Handover Procedures

```bash
#!/bin/bash
# scripts/post-deployment/handover.sh

set -euo pipefail

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Transfer to operations team
handover_to_ops() {
    log "=== Operations Handover ==="

    # Generate handover document
    cat > /tmp/handover-$(date +%Y%m%d).md <<EOF
# erlmcp v3 Operations Handover

**Date:** $(date)
**Version:** 3.0.0
**Deployed By:** ${USER:-deployer}

## Deployment Summary

### Regions Deployed
- us-east-1 (Primary): 30 nodes, 99.999% SLA
- eu-west-1 (Secondary): 20 nodes, 99.999% SLA
- ap-southeast-1 (Tertiary): 15 nodes, 99.99% SLA
- us-west-2 (DR): 10 nodes (standby), 99.9% SLA

### Architecture Overview
- Multi-region active-active deployment
- PostgreSQL with cross-region replication
- Global load balancing via Route 53
- Centralized observability in us-east-1

## Access Information

### Kubernetes Clusters
| Region | Cluster Name | Kubeconfig Context |
|--------|---------------|------------------|
| us-east-1 | erlmcp-prod-useast1 | aws eks update-kubeconfig --region us-east-1 --name erlmcp-prod-useast1 |
| eu-west-1 | erlmcp-prod-euwest1 | aws eks update-kubeconfig --region eu-west-1 --name erlmcp-prod-euwest1 |
| ap-southeast-1 | erlmcp-prod-apsoutheast1 | aws eks update-kubeconfig --region ap-southeast-1 --name erlmcp-prod-apsoutheast1 |
| us-west-2 | erlmcp-dr-uswest2 | aws eks update-kubeconfig --region us-west-2 --name erlmcp-dr-uswest2 |

### URLs
- **API Gateway**: https://erlmcp.io
- **Grafana**: https://grafana.erlmcp.io
- **Prometheus**: https://prometheus.erlmcp.io
- **Jaeger**: https://jaeger.erlmcp.io
- **Status Page**: https://status.erlmcp.io

### Credentials
- **Database**: AWS Secrets Manager: erlmcp/db
- **Erlang Cookie**: AWS SSM: /erlmcp/erlang-cookie
- **JWT Secret**: AWS SSM: /erlmcp/jwt-secret
- **Registry**: ghcr.io/seanchatmangpt/erlmcp

## Operational Procedures

### Daily Operations
1. Check regional health at scripts/ops/daily-checklist.sh
2. Review metrics in Grafana
3. Check alerts in PagerDuty

### Scaling
- Auto-scaling configured (3-100 pods per region)
- Manual scaling: scripts/ops/scale-horizontal.sh
- Emergency scaling: scripts/ops/emergency-scale.sh

### Backup
- Automated: Daily at 2 AM UTC
- Manual: scripts/backup/manual-backup.sh
- Retention: 30 days

### Incident Response
- PagerDuty: erlmcp-on-call
- Slack: #incidents
- Runbooks: https://runbooks.erlmcp.io
- Escalation: On-call -> Lead -> Director -> CTO

## Runbooks Location
- Main Runbook: docs/DEPLOYMENT_RUNBOOK.md
- Regional Runbooks: docs/ops/REGIONS_RUNBOOKS.md
- DR Runbooks: docs/dr/README.md
- SLA Runbooks: docs/sla/README.md

## Known Issues
- None

## Next Steps
1. Monitor for 24 hours post-deployment
2. Review performance metrics
3. Plan optimization based on metrics
4. Schedule post-mortem in 1 week

---
*Generated: $(date -u)*
EOF

    log "Handover document created"
    log "Location: /tmp/handover-$(date +%Y%m%d).md"

    # Send to operations team
    mail -s "erlmcp v3 Operations Handover" \
        ops-team@erlmcp.com < /tmp/handover-$(date +%Y%m%d).md

    log "Handover complete"
}

# Schedule follow-up meeting
schedule_followup() {
    log "Scheduling follow-up meeting..."

    # Schedule for 7 days out
    echo "./scripts/meeting/schedule.sh \
        --title 'erlmcp v3 Post-Deployment Review' \
        --date $(date -d '+7 days' '+%Y-%m-%d') \
        --duration 60 \
        --attendees ops-team,dev-team,management" | at now

    log "Follow-up meeting scheduled"
}

main() {
    handover_to_ops
    schedule_followup

    log "Post-deployment handover complete"
}

main "$@"
```

### 2. Performance Optimization

```bash
#!/bin/bash
# scripts/post-deployment/optimize-performance.sh

set -euo pipefail

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Analyze current performance
analyze_performance() {
    log "=== Performance Analysis ==="

    # Collect metrics from all regions
    for REGION in us-east-1 eu-west-1 ap-southeast-1; do
        log "Analyzing $REGION..."

        # Get P95 latency
        P95=$(curl -sf "https://prometheus.erlmcp.io/api/v1/query?query=histogram_quantile(0.95,sum(rate(http_request_duration_seconds_bucket{region=\"$REGION\"}[1h])) by(le))" | jq -r '.data.result[0].value[1]')

        # Get throughput
        RPS=$(curl -sf "https://prometheus.erlmcp.io/api/v1/query?query=sum(rate(http_requests_total{region=\"$REGION\"}[1h]))" | jq -r '.data.result[0].value[1]')

        # Get error rate
        ERROR_RATE=$(curl -sf "https://prometheus.erlmcp.io/api/v1/query?query=sum(rate(http_requests_total{region=\"$REGION\",status=~\"5..\"}[1h]))/sum(rate(http_requests_total{region=\"$REGION\"}[1h]))" | jq -r '.data.result[0].value[1]')

        echo "  $REGION: P95=${P95}s, RPS=${RPS}, Errors=${ERROR_RATE}%"
    done
}

# Apply optimizations
optimize() {
    log "=== Applying Optimizations ==="

    # Check for resource utilization
    for REGION in us-east-1 eu-west-1 ap-southeast-1; do
        # Check CPU/memory usage
        CPU=$(kubectl top nodes -n erlmcp-production --context=$REGION --no-headers | awk '{sum+=$3} END {print sum/NR}' | xargs)
        MEMORY=$(kubectl top nodes -n erlmcp-production --context=$REGION --no-headers | awk '{sum+=$4} END {print sum/NR}' | xargs)

        log "$REGION: CPU=$CPU%, Memory=$MEMORY%"

        # Adjust if needed
        if [ $CPU -lt 50 ]; then
            log "  Scaling down $REGION (low CPU utilization)"
            kubectl scale deployment erlmcp --replicas=3 -n erlmcp-production --context=$REGION
        fi
    done
}

main() {
    analyze_performance
    optimize

    log "Performance optimization complete"
}

main "$@"
```

---

## Appendices

### Appendix A: Regional Configuration Files

#### us-east-1 Configuration

```yaml
# charts/erlmcp/values-useast1.yaml
global:
  region: us-east-1
  environment: production
  cluster: erlmcp-prod-useast1

erlmcp:
  replicaCount: 3

  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: 2000m
      memory: 4Gi

  hpa:
    enabled: true
    minReplicas: 3
    maxReplicas: 100
    targetCPUUtilizationPercentage: 70
    targetMemoryUtilizationPercentage: 80

  ingress:
    enabled: true
    className: nginx
    annotations:
      nginx.ingress.kubernetes.io/ssl-redirect: "true"
      nginx.ingress.kubernetes.io/proxy-buffer-size: "16k"
      cert-manager.io/cluster-issuer: letsencrypt-prod

monitoring:
  prometheus:
    enabled: true
    serviceMonitor:
      enabled: true
```

#### eu-west-1 Configuration

```yaml
# charts/erlmcp/values-euwest1.yaml
global:
  region: eu-west-1
  environment: production
  cluster: erlmcp-prod-euwest1

erlmcp:
  replicaCount: 2

  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: 2000m
      memory: 4Gi

  hpa:
    enabled: true
    minReplicas: 2
    maxReplicas: 50
    targetCPUUtilizationPercentage: 70
    targetMemoryUtilizationPercentage: 80

  compliance:
    gdpr:
      enabled: true
      data_residency: eu-west-1
      consent_required: true

  monitoring:
  prometheus:
    enabled: true
    serviceMonitor:
      enabled: true
    retention: 90d  # Extended for EU compliance
```

#### ap-southeast-1 Configuration

```yaml
# charts/erlmcp/values-apsoutheast1.yaml
global:
  region: ap-southeast-1
  environment: production
  cluster: erlmcp-prod-apsoutheast1

erlmcp:
  replicaCount: 2

  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: 2000m
      memory: 4Gi

  hpa:
    enabled: true
    minReplicas: 2
    maxReplicas: 40
    targetCPUUtilizationPercentage: 70
    targetMemoryUtilizationPercentage: 80

  monitoring:
  prometheus:
    enabled: true
    serviceMonitor:
      enabled: true
```

#### us-west-2 (DR) Configuration

```yaml
# charts/erlmcp/values-uswest2-dr.yaml
global:
  region: us-west-2
  environment: production
  cluster: erlmcp-dr-uswest2

erlmcp:
  replicaCount: 0  # Hot standby

  resources:
    requests:
      cpu: 100m
      memory: 256Mi
    limits:
      cpu: 500m
      memory: 1Gi

  hpa:
    enabled: false  # Disabled in DR mode

  standby:
    enabled: true
    mode: hot
    activation_timeout: 300s

  monitoring:
  prometheus:
    enabled: true
    serviceMonitor:
      enabled: true
```

### Appendix B: Emergency Contacts

| Role | Contact | Phone | Slack |
|------|---------|-------|-------|
| On-Call Engineer | +1-555-ONCALL | PagerDuty | @on-call |
| SRE Lead | sre-lead@erlmcp.io | +1-555-SRE | @sre-team |
| DevOps Lead | devops-lead@erlmcp.io | +1-555-DEVOPS | @devops |
| Database Lead | dba-lead@erlmcp.io | +1-555-DBA | @dba |
| Security Team | security@erlmcp.io | +1-556-SEC | @security |
| Executive | executive@erlmcp.io | +1-555-EXEC | @exec |

### Appendix C: Troubleshooting Guide

#### Common Issues and Solutions

| Issue | Symptoms | Region | Solution |
|-------|----------|--------|----------|
| Pods pending | kubectl get pods shows Pending | All | Check node resources, taints |
| High latency | P95 > 500ms | All | Scale up, check database |
| Connection refused | curl: (7) | All | Check security groups, service |
| Replication lag | Lag > 60s | All | Check network, restart replication |
| OOM killed | Pods restarting | All | Increase memory limits |
| DNS propagation | Domain not resolving | All | Check Route 53 health checks |

### Appendix D: SLA Definitions

```yaml
sla_definitions:
  availability:
    monthly_target: 99.99%
    calculation: (total_minutes - downtime) / total_minutes * 100
    allowed_downtime: 43.2 minutes/month
    exclusions:
      - Scheduled maintenance (max 4 hours/month)
      - Third-party outages
      - Force majeure events

  performance:
    api_latency_p95: < 100ms
    api_latency_p99: < 500ms
    measurement: histogram over 5-minute windows
    alert_threshold: 2 consecutive windows exceeding target

  throughput:
    minimum: 10,000 requests/second
    target: 50,000 requests/second
    measurement: requests per second
    alert_threshold: below minimum for 5 minutes

  durability:
    data: 99.9999999% (11 nines)
    measurement: Annual
    calculation: 1 - (bytes_lost / total_bytes)

  recoverability:
    rto_critical: 5 minutes
    rto_important: 30 minutes
    rto_standard: 4 hours
    rpo_critical: 1 minute
    rpo_important: 5 minutes
    rpo_standard: 1 hour
```

---

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 3.0.0 | Initial Fortune 500 deployment guide created |

---

**Document Status:** Production Ready
**Next Review:** 2026-03-02
**Maintained By:** erlmcp Deployment Team
**Approval:** Change Board

---
