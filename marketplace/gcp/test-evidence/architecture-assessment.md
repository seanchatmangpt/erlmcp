# GCP Marketplace Deployment Architecture Assessment
## erlmcp v3.0.0 - System Architecture Review

**Assessment Date:** 2026-02-02
**Assessor:** System Architecture Designer
**Version:** 3.0.0
**Status:** APPROVED with Recommendations

---

## Executive Summary

The erlmcp GCP Marketplace deployment architecture demonstrates a well-structured, enterprise-grade design with support for three deployment models (GKE, Cloud Run, Compute Engine). The architecture follows Google Cloud best practices for security, observability, and high availability. This assessment identifies the strengths of the current implementation and provides recommendations for improvement.

### Overall Rating: 4.2/5.0

| Category | Rating | Status |
|----------|--------|--------|
| Architecture Design | 4.5/5 | Excellent |
| Security & Compliance | 4.5/5 | Excellent |
| High Availability | 4.0/5 | Good |
| Scalability | 4.5/5 | Excellent |
| Observability | 4.5/5 | Excellent |
| Cost Optimization | 3.5/5 | Good |
| Documentation | 4.0/5 | Good |

---

## 1. Architecture Review

### 1.1 Deployment Path Analysis

#### 1.1.1 Google Kubernetes Engine (GKE)

**Strengths:**
- Regional cluster configuration for 99.95% SLA
- Private cluster with control plane isolation
- Workload Identity integration for secretless authentication
- Shielded GKE nodes with secure boot and integrity monitoring
- Multiple node pools with separate spot/preemptible pool
- Network Policy enforcement (Calico)
- Horizontal Pod Autoscaler (HPA) with CPU and memory metrics
- Pod Disruption Budget for graceful maintenance

**Architecture Diagram (GKE):**

```
┌─────────────────────────────────────────────────────────────────────┐
│                         GCP Region (us-central1)                    │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Regional GKE Cluster                       │  │
│  │  ┌────────────────────────────────────────────────────────┐  │  │
│  │  │              Private Control Plane                      │  │  │
│  │  │         (172.16.0.0/28 - No Public Endpoint)            │  │  │
│  │  └────────────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐          │  │
│  │  │   Zone A    │  │   Zone B    │  │   Zone C    │          │  │
│  │  │  (primary)  │  │  (primary)  │  │  (primary)  │          │  │
│  │  │  ┌───────┐  │  │  ┌───────┐  │  │  ┌───────┐  │          │  │
│  │  │  │ Node  │  │  │  │ Node  │  │  │  │ Node  │  │          │  │
│  │  │  │ Pool  │  │  │  │ Pool  │  │  │  │ Pool  │  │          │  │
│  │  │  │ 3x    │  │  │  │ 3x    │  │  │  │ 3x    │  │          │  │
│  │  │  │ e2-   │  │  │  │ e2-   │  │  │  │ e2-   │  │          │  │
│  │  │  │ std-4 │  │  │  │ std-4 │  │  │  │ std-4 │  │          │  │
│  │  │  └───────┘  │  │  └───────┘  │  │  └───────┘  │          │  │
│  │  │  ┌───────┐  │  │  ┌───────┐  │  │  ┌───────┐  │          │  │
│  │  │  │ Spot  │  │  │  │ Spot  │  │  │  │ Spot  │  │          │  │
│  │  │  │ Pool  │  │  │  │ Pool  │  │  │  │ Pool  │  │          │  │
│  │  │  │ 0-5x  │  │  │  │ 0-5x  │  │  │  │ 0-5x  │  │          │  │
│  │  │  └───────┘  │  │  └───────┘  │  │  └───────┘  │          │  │
│  │  └─────────────┘  └─────────────┘  └─────────────┘          │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    VPC Network (erlmcp-vpc)                   │  │
│  │  ┌────────────────────┐  ┌────────────────────┐              │  │
│  │  │  Subnet (10.0.0.0/24)│  │  Subnet (10.10.0.0/24)│             │  │
│  │  │    Pods (10.1.0.0/16)│  │                     │             │  │
│  │  │  Services (10.2.0.0/16)│  │                     │            │  │
│  │  └────────────────────┘  └────────────────────┘             │  │
│  │  ┌──────────────────────────────────────────────────────┐   │  │
│  │  │              Cloud NAT (Private Egress)               │   │  │
│  │  └──────────────────────────────────────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Google Cloud Operations                   │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │  │
│  │  │   Logging   │  │  Monitoring │  │    Trace            │  │  │
│  │  │  (Cloud     │  │ (Prometheus │  │  (Cloud Trace)      │  │  │
│  │  │   Logging)  │  │    + GCM)   │  │                     │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────────┐    │  │
│  │  │              Secret Manager (11 secrets)            │    │  │
│  │  └─────────────────────────────────────────────────────┘    │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

**Configuration:**
- **Release Channel:** REGULAR (balanced stability/features)
- **Datapath:** ADVANCED_DATAPATH (Dataplane V2)
- **Network Policy:** Calico for zero-trust enforcement
- **Autoscaling:** BALANCED profile with 3-10 node range

**Recommendations:**
1. Consider adding GPU node pool option for AI/ML workloads
2. Implement GKE Backup and Restore for disaster recovery
3. Add Network Endpoint Groups (NEG) for direct pod access

#### 1.1.2 Cloud Run

**Strengths:**
- Serverless auto-scaling (0 to 100 instances)
- Built-in load balancing and health checks
- Traffic splitting support for blue-green deployments
- Concurrency control (80 concurrent requests per instance)
- Ingress settings for internal-only deployment

**Architecture Diagram (Cloud Run):**

```
┌─────────────────────────────────────────────────────────────────────┐
│                         GCP Region (us-central1)                    │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Cloud Run Service                          │  │
│  │  ┌────────────────────────────────────────────────────────┐   │  │
│  │  │              Revision Controller                       │   │  │
│  │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐               │   │  │
│  │  │  │ 100%    │  │ Traffic │  │ Canary  │               │   │  │
│  │  │  │ Latest  │  │ Split   │  │ (5%)    │               │   │  │
│  │  │  │ Revision│  │         │  │         │               │   │  │
│  │  │  └─────────┘  └─────────┘  └─────────┘               │   │  │
│  │  └────────────────────────────────────────────────────────┘   │  │
│  │  ┌────────────────────────────────────────────────────────┐   │  │
│  │  │              Auto-scaling (0-100 instances)            │   │  │
│  │  │  Target: 80 concurrent requests / instance             │   │  │
│  │  └────────────────────────────────────────────────────────┘   │  │
│  │  ┌────────────────────────────────────────────────────────┐   │  │
│  │  │              Container Instance                         │   │  │
│  │  │  - CPU: 1 vCPU                                         │   │  │
│  │  │  - Memory: 512 MiB                                     │   │  │
│  │  │  - Timeout: 300s                                       │   │  │
│  │  │  - Concurrency: 80                                     │   │  │
│  │  └────────────────────────────────────────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                      Load Balancer                           │  │
│  │  - Cloud Run ingress                                        │  │
│  │  - Global anycast IP                                        │  │
│  │  - Cloud CDN integration                                    │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

**Configuration:**
- **Min Instances:** 0 (scale to zero)
- **Max Instances:** 100
- **Concurrency:** 80 requests/instance
- **Timeout:** 300 seconds
- **Ingress:** ALL (can be INTERNAL)

**Recommendations:**
1. Set min_instances to 1 for production to avoid cold starts
2. Implement max_instances per deployment tier
3. Add request-based CPU allocation for variable workloads

#### 1.1.3 Compute Engine

**Strengths:**
- Managed Instance Group (MIG) with auto-healing
- Health check-based auto-scaling
- Shielded VM with secure boot
- Optional confidential computing
- Custom VM images via Packer

**Architecture Diagram (Compute Engine):**

```
┌─────────────────────────────────────────────────────────────────────┐
│                         GCP Zone (us-central1-a)                    │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │              Managed Instance Group (MIG)                     │  │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐        │  │
│  │  │ erlmcp  │  │ erlmcp  │  │ erlmcp  │  │ erlmcp  │  ...   │  │
│  │  │  -vm-1  │  │  -vm-2  │  │  -vm-3  │  │  -vm-4  │        │  │
│  │  │ e2-std-4│  │ e2-std-4│  │ e2-std-4│  │ e2-std-4│        │  │
│  │  │Shielded │  │Shielded │  │Shielded │  │Shielded │        │  │
│  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘        │  │
│  │  ┌────────────────────────────────────────────────────────┐   │  │
│  │  │              Auto-scaling (2-10 instances)             │   │  │
│  │  │  Target CPU: 70%                                      │   │  │
│  │  └────────────────────────────────────────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                      Regional Load Balancer                  │  │
│  │  - Health check: /health (port 8080)                         │  │
│  │  - Backend service: MIG                                      │  │
│  │  - Session affinity: None                                    │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                   Instance Template                          │  │
│  │  - Custom Image: erlmcp-marketplace-v1                       │  │
│  │  - Service Account: erlmcp-vm-sa                             │  │
│  │  - Scopes: cloud-platform, logging.write, monitoring.write  │  │
│  │  - Metadata: SSH keys, startup script                        │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

**Configuration:**
- **Instance Type:** e2-standard-4
- **Min Instances:** 2
- **Max Instances:** 10
- **Target CPU:** 70%
- **Auto-healing:** Enabled with health check

**Recommendations:**
1. Add preemptible instance template option
2. Implement regional MIG for zone redundancy
3. Add custom machine type for cost optimization

---

### 1.2 VPC and Network Design

#### 1.2.1 Network Architecture

**VPC Module Analysis:**

| Component | Configuration | Assessment |
|-----------|---------------|------------|
| Routing Mode | REGIONAL | Good - Prevents cross-region routing issues |
| MTU | 1460 | Good - Compatible with Cloud VPN/Interconnect |
| Subnet Creation | Manual | Good - Explicit control over IP ranges |
| Default Routes | Preserved | Good - Required for Cloud NAT |
| Flow Logs | 5s interval, 50% sampling | Good balance between cost and visibility |

**IP Addressing:**
```
Primary Subnet (us-central1):  10.0.0.0/24
  - Pods:      10.1.0.0/16 (/16 secondary range)
  - Services:  10.2.0.0/16 (/16 secondary range)

Secondary Subnet (us-east1):   10.10.0.0/24
  - No secondary ranges (single-region GKE)

Master Access CIDR:           172.16.0.0/28
```

**Firewall Rule Analysis:**

| Rule | Priority | Source | Port | Assessment |
|------|----------|--------|------|------------|
| Internal Traffic | 1000 | 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 | 0-65535 | Good - Allows private communication |
| SSH Access | 1001 | 0.0.0.0/0 | 22 | Risk - Should be restricted |
| Health Checks | 1002 | Google IPs | 8080, 9090 | Good - Explicit ranges |
| HTTPS Public | 1003 | 0.0.0.0/0 | 443 | Conditional - Depends on use case |
| Master Access | 1004 | 10.0.0.0/8 | 10250 | Good - Restricted |
| Deny All | 65534 | 0.0.0.0/0 | All | Optional - Default deny |

**Security Recommendations:**
1. Restrict SSH access to specific CIDR ranges or use IAP
2. Implement VPC Service Controls for data egress prevention
3. Add Private Service Connect for private API access
4. Consider GKE Ephemeral Container IPs for pod IP management

#### 1.2.2 Cloud NAT Configuration

**Analysis:**
- NAT allocation: AUTO_ONLY (cost-optimized)
- Logging: Enabled with ALL filter
- Port ranges: 64-65536 min/max per VM
- Timeouts: Industry-standard defaults

**Recommendations:**
1. Consider Manual NAT for predictable IPs
2. Add NAT logging exclusion for health checks
3. Implement Cloud Router for dynamic VPN/Interconnect

---

### 1.3 Observability Architecture

#### 1.3.1 Monitoring Stack

**Components:**
```
Google Cloud Operations Suite
├── Cloud Monitoring (GCM)
│   ├── Custom Metrics (5 defined)
│   ├── Alert Policies (6 configured)
│   ├── Dashboards (4 templates)
│   ├── Uptime Checks (1 configured)
│   └── SLOs (2 defined: availability, latency)
├── Cloud Logging
│   ├── Log Sinks (BigQuery, Cloud Storage)
│   ├── Log Exclusions (health checks, successful GETs)
│   └── Aggregation: 5s interval
├── Cloud Trace
│   └── Sample rate: 10%
└── Notification Channels
    ├── Email
    ├── PagerDuty
    ├── Slack
    └── Webhook
```

**Custom Metrics:**
1. `custom.googleapis.com/erlmcp/http/latency` (GAUGE, DOUBLE, seconds)
2. `custom.googleapis.com/erlmcp/http/requests` (CUMULATIVE, INT64)
3. `custom.googleapis.com/erlmcp/connections/active` (GAUGE, INT64)
4. `custom.googleapis.com/erlmcp/erlang/processes` (GAUGE, INT64)
5. `custom.googleapis.com/erlmcp/memory/usage` (GAUGE, DOUBLE, bytes)

**Alert Policies:**
1. High Error Rate - > 1% for 5 minutes
2. High Latency - P99 > 1s for 5 minutes
3. High Memory - > 80% for 5 minutes
4. High CPU - > 80% for 10 minutes
5. Health Check Failure - Any failure
6. Low Process Count - < 100 for 5 minutes

**Observability Recommendations:**
1. Add RED method metrics (Rate, Errors, Duration)
2. Implement synthetic monitoring for multi-region
3. Add dashboard for Erlang VM specifics (reductions, GC)
4. Create log-based metrics for business KPIs
5. Implement alert noise reduction with grouping

#### 1.3.2 Dashboard Templates

**Main Dashboard Widgets:**
1. Request Rate (timeseries)
2. Error Rate (scorecard)
3. CPU Utilization (timeseries)
4. Memory Usage (timeseries)
5. Active Connections (timeseries)
6. Response Time (timeseries)

**Additional Dashboards:**
- Performance (optional): Latency heatmaps, throughput
- Erlang VM (optional): Process count, ETS tables, garbage collection
- Security (optional): Auth failures, rate limit violations

---

### 1.4 Secret Management Architecture

#### 1.4.1 Secret Manager Integration

**Secrets Defined (11 total):**

| Secret | Purpose | Rotation | Access |
|--------|---------|----------|--------|
| erlmcp-erlang-cookie | Cluster security | 90 days | SecretAccessor |
| erlmcp-db-password | Database auth | 90 days | SecretAccessor |
| erlmcp-redis-password | Redis auth | 90 days | SecretAccessor |
| erlmcp-tls-cert | TLS certificate | Automatic | SecretAccessor |
| erlmcp-tls-key | TLS private key | Automatic | SecretAccessor |
| erlmcp-ca-bundle | CA certificates | Manual | Viewer |
| erlmcp-jwt-private-key | JWT signing | 90 days | SecretAccessor |
| erlmcp-jwt-public-key | JWT verification | N/A | Viewer |
| erlmcp-grafana-password | Grafana auth | 90 days | SecretAccessor |
| erlmcp-backup-key | Backup encryption | 90 days | SecretAccessor |
| erlmcp-otel-ca-cert | OTEL mTLS | Manual | SecretAccessor |

**Secret Access Patterns:**
1. **GKE:** Workload Identity (KSA to GSA mapping)
2. **Cloud Run:** Secret Manager as environment variables
3. **Compute Engine:** Service account with SecretAccessor role

**Security Assessment:**
- Automatic secret generation with 64-character entropy
- IAM bindings per secret (least privilege)
- Secret access logging enabled
- Replication: Automatic (multi-region)

**Secret Management Recommendations:**
1. Implement secret versioning and rotation schedule
2. Add secret access audit alerts
3. Use Secret Manager CSI driver for Kubernetes (optional)
4. Implement secret expiration notifications
5. Add secret injection validation at startup

---

## 2. High Availability Assessment

### 2.1 HA Configuration by Deployment Type

#### 2.1.1 GKE HA Configuration

| Feature | Configuration | HA Level |
|---------|---------------|----------|
| Cluster Type | Regional | 99.95% SLA (21.6 min/month downtime) |
| Zones | 3 (us-central1-a, b, c) | Zone redundancy |
| Control Plane | Private endpoint + multi-zone | Master HA |
| Node Pools | Multi-zone with spread | Node HA |
| Pod Disruption Budget | minAvailable: 1 | Rolling update HA |
| Auto-repair | Enabled | Self-healing |
| Auto-upgrade | Max surge: 1, unavailable: 0 | Upgrade HA |

**Failure Domain Analysis:**
- **Zone Failure:** Survives (2/3 zones operational)
- **Node Failure:** Survives (MIG auto-replacement)
- **Control Plane Failure:** Survives (regional control plane)
- **Network Partition:** Survives (private cluster with NAT)

**GKE HA Recommendations:**
1. Consider multi-region cluster for 99.99% SLA
2. Implement GKE backup for etcd recovery
3. Add cluster autoscaler for node pool resilience
4. Implement pod topology spread constraints

#### 2.1.2 Cloud Run HA Configuration

| Feature | Configuration | HA Level |
|---------|---------------|----------|
| Instances | 0-100, min: 0 | 99.9% SLA (43.2 min/month) |
| Revisioning | Automatic | Blue-green support |
| Traffic Splitting | Supported | Gradual rollout |
| Concurrency | 80/instance | Load distribution |
| Health Checks | Liveness + Readiness | Self-healing |

**Failure Domain Analysis:**
- **Instance Failure:** Survives (automatic replacement)
- **Zone Failure:** Survives (regional deployment)
- **Cold Start:** Risk if min_instances=0
- **Deployment:** Zero-downtime with traffic splitting

**Cloud Run HA Recommendations:**
1. Set min_instances=1 for production
2. Implement max_instances limits per tier
3. Add circuit breaker pattern for downstream services
4. Use revision tags for rollback capability

#### 2.1.3 Compute Engine HA Configuration

| Feature | Configuration | HA Level |
|---------|---------------|----------|
| Instance Group | Managed (MIG) | Auto-healing |
| Health Checks | HTTP /health | Failure detection |
| Auto-scaling | 2-10 instances | Dynamic capacity |
| Zones | Single (us-central1-a) | Zone risk |
| Load Balancer | Regional | Zone failover |

**Failure Domain Analysis:**
- **Instance Failure:** Survives (MIG recreation)
- **Zone Failure:** NO (single zone deployment)
- **Network Failure:** Partial (health check based)
- **Disk Failure:** Survives (persistent disk replicated)

**Compute Engine HA Recommendations:**
1. Implement regional MIG for zone redundancy
2. Add health check to secondary port
3. Implement graceful shutdown handling
4. Add instance template versioning

### 2.2 Regional and Multi-Region Considerations

#### 2.2.1 Current Multi-Region Support

| Deployment Type | Multi-Region Support | Current State |
|-----------------|---------------------|---------------|
| GKE | Supported via Anthos | Not configured |
| Cloud Run | Supported via traffic steering | Not configured |
| Compute Engine | Supported via regional MIGs | Single zone |

**Multi-Region Requirements:**
1. **GKE:** Anthos Service Mesh or GKE Multi-cluster
2. **Cloud Run:** Regional deployment with traffic steering
3. **Data:** Cloud Spanner or Multi-region Cloud SQL
4. **Networking:** Cloud Load Balancer with multi-region backend
5. **Observability:** Regional monitoring aggregation

#### 2.2.2 Disaster Recovery Design

**RPO/RTO Targets:**
- **RPO (Recovery Point Objective):** 1 minute (data loss tolerance)
- **RTO (Recovery Time Objective):** 15 minutes (service restoration)

**DR Strategies by Tier:**

| Tier | Strategy | RPO | RTO |
|------|----------|-----|-----|
| Critical | Hot standby in separate region | <1 min | <5 min |
| Important | Warm standby, manual failover | 5 min | 15 min |
| Standard | Daily backups, restore from backup | 24 hr | 4 hr |

**Current Implementation:**
- Backup retention: 30 days (configurable)
- Backup encryption: Customer-managed keys
- Backup location: Regional (should be multi-region)

**DR Recommendations:**
1. Implement cross-region backup replication
2. Add disaster recovery runbook automation
3. Implement regular DR testing (quarterly)
4. Add regional failover documentation

---

## 3. Scalability Analysis

### 3.1 Autoscaling Configurations

#### 3.1.1 GKE Cluster Autoscaler

**Configuration:**
```
Profile: BALANCED
Min Nodes: 3
Max Nodes: 10 (primary), 5 (spot)
Scale-up: 3 nodes/60s
Scale-down: 1 node/300s (stabilization)
```

**Scale Triggers:**
- CPU: 70% target utilization
- Memory: 80% target utilization
- Custom metrics: Not configured (recommendation)

**Autoscaler Assessment:**
- Scale-up is fast (3 nodes in 60 seconds)
- Scale-down has 5-minute stabilization (prevents thrashing)
- Spot node pool can be evicted (preemptible)

**Recommendations:**
1. Add custom metric autoscaling (active connections, queue depth)
2. Implement priority classes for critical pods
3. Add overprovisioning pod for immediate scale-up
4. Consider cluster autoscaler for GPU node pool

#### 3.1.2 Cloud Run Autoscaler

**Configuration:**
```
Min Instances: 0
Max Instances: 100
Target Concurrency: 80 requests/instance
Scale-up: Immediate
Scale-down: 300s stabilization
```

**Scaling Algorithm:**
- Requests / Target Concurrency = Instance Count
- Example: 1000 requests / 80 = 12.5 = 13 instances

**Autoscaler Assessment:**
- Fast scale-up (sub-second to container start)
- Scale-down delay prevents thrashing
- Max 100 instances may limit peak traffic

**Recommendations:**
1. Set min_instances=1 for production (avoid cold starts)
2. Implement max_instances per environment tier
3. Add container startup time optimization
4. Consider CPU-based scaling for variable workloads

#### 3.1.3 Compute Engine Autoscaler

**Configuration:**
```
Min Instances: 2
Max Instances: 10
Target CPU: 70%
Cooldown: 60 seconds
```

**Autoscaler Assessment:**
- Traditional VM scaling (slower than containers)
- MIG provides health-based scaling
- Single zone limits capacity

**Recommendations:**
1. Implement regional MIG for zone-based scaling
2. Add utilization-based scaling targets
3. Implement preemptible instance template
4. Add custom health check for application readiness

### 3.2 Resource Limits and Quotas

#### 3.2.1 GCP Quota Limits

| Resource | Default | Requested | Assessment |
|----------|---------|-----------|------------|
| Compute Engine CPUs | Varies | 24+ | Adequate for initial |
| GKE clusters | 5 (regional) | 1 | Adequate |
| GKE nodes per cluster | 5000 | 100 | Adequate |
| Cloud Run instances | 100 | 100 | May need increase |
| Load balancers | 15 | 3 | Adequate |
| Cloud Routers | 5 | 1 | Adequate |

**Quota Recommendations:**
1. Document quota requirements in README
2. Add quota validation in pre-deployment checks
3. Implement quota monitoring and alerts
4. Add quota increase request process documentation

#### 3.2.2 Container Resource Limits

**GKE Pod Resources:**
```yaml
Requests:
  cpu: "500m"
  memory: "1Gi"
Limits:
  cpu: "2000m"
  memory: "2Gi"
```

**Cloud Run Resources:**
```yaml
CPU: 1 vCPU (fixed)
Memory: 512 MiB (fixed)
Max concurrent: 80 requests
```

**Resource Assessment:**
- GKE has 4x CPU limit for burst handling
- Memory limit is 2x request (reasonable)
- Cloud Run has fixed allocation (not optimal)

**Recommendations:**
1. Implement resource quota per namespace
2. Add limit ranges for container constraints
3. Implement priority classes for QoS
4. Add resource usage monitoring and alerts

### 3.3 Cost Optimization Opportunities

#### 3.3.1 Cost Analysis by Deployment

| Deployment | Base Cost/Month | Scale Cost | Optimization |
|------------|-----------------|------------|--------------|
| GKE (3 nodes) | ~$150 | +$50/node | Spot nodes, autoscaling |
| Cloud Run (0 min) | $0 | $0.40/MiB | Min instances, memory |
| Compute Engine (2 VMs) | ~$100 | +$50/VM | Preemptible, rightsizing |

**Cost Optimization Strategies:**

1. **Spot Instances (GKE/GCE):**
   - 60-80% cost savings
   - Risk: Preemption
   - Mitigation: Spot node pool with taints

2. **Committed Use Discounts:**
   - 1-3 year commitments
   - Up to 70% savings
   - Recommended for stable workloads

3. **Right Sizing:**
   - Analyze actual resource usage
   - Adjust machine types accordingly
   - Use custom machine types

4. **Network Optimization:**
   - Cloud NAT logging to identify patterns
   - Private Google Access for API calls
   - VPC peering vs VPN

**Cost Optimization Recommendations:**
1. Implement Cost Management alerts
2. Add SLO-based budget controls
3. Implement resource cleanup automation
4. Add cost attribution by deployment

---

## 4. Integration Patterns

### 4.1 GCP Service Integrations

#### 4.1.1 Cloud Operations Suite

**Integration Points:**

1. **Cloud Logging:**
   - Agent: Cloud Logging for GKE, Ops Agent for GCE
   - Log format: JSON structured logs
   - Exclusions: Health checks, successful GET requests
   - Exports: BigQuery, Cloud Storage

2. **Cloud Monitoring:**
   - Metrics: OpenTelemetry + custom metrics
   - Dashboards: 4 pre-configured templates
   - Alert policies: 6 critical alerts
   - SLOs: 2 service-level objectives

3. **Cloud Trace:**
   - Sampling: 10% (configurable)
   - Integration: OpenTelemetry traces
   - Storage: 30 days (configurable)

**Integration Assessment:**
- Comprehensive observability coverage
- Well-structured alert policies
- Missing: Incident correlation, AI-powered anomaly detection

**Recommendations:**
1. Implement log-based metrics for business KPIs
2. Add metric groups for correlated alerts
3. Implement canary analysis for deployments
4. Add observability for multi-region deployments

#### 4.1.2 Secret Manager Integration

**Workload Identity Flow:**
```
Pod (KSA) --[iam.gke.io/gcp-service-account]--> GSA --[IAM binding]--> Secret
```

**Secret Access Pattern:**
1. Application accesses secret via environment variable
2. Secret Manager CSI Driver mounts secret (optional)
3. Workload Identity provides authentication
4. IAM policy grants SecretAccessor role

**Security Assessment:**
- Zero long-lived credentials
- Automatic secret rotation support
- Audit logging for secret access

**Recommendations:**
1. Implement secret versioning strategy
2. Add secret rotation automation
3. Implement secret access patterns documentation
4. Add secret validation at application startup

#### 4.1.3 Cloud Operations Integration Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         erlmcp Application                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │   erlmcp    │  │   erlmcp    │  │   erlmcp    │                 │
│  │  Pod/VM 1   │  │  Pod/VM 2   │  │  Pod/VM 3   │                 │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘                 │
│         │                │                │                          │
│         └────────────────┴────────────────┘                          │
│                          │                                           │
│         ┌────────────────┴────────────────┐                          │
│         │     OpenTelemetry Collector      │                          │
│         │  - Metrics (Prometheus format)   │                          │
│         │  - Logs (JSON)                   │                          │
│         │  - Traces (OTLP)                 │                          │
│         └────────────────┬────────────────┘                          │
│                          │                                           │
└──────────────────────────┼───────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Google Cloud Operations                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │   Cloud     │  │   Cloud     │  │   Cloud     │                 │
│  │  Logging    │  │ Monitoring  │  │   Trace     │                 │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘                 │
│         │                │                │                          │
│         └────────────────┴────────────────┘                          │
│                          │                                           │
│  ┌────────────────────────┴────────────────────────┐                │
│  │              Alert Policies (6)                │                │
│  │  - High error rate    - High latency           │                │
│  │  - High memory        - Health check failure   │                │
│  │  - High CPU           - Low process count      │                │
│  └────────────────────────┬────────────────────────┘                │
│                          │                                           │
│  ┌────────────────────────┴────────────────────────┐                │
│  │           Notification Channels (4)             │                │
│  │  - Email  - PagerDuty  - Slack  - Webhook      │                │
│  └─────────────────────────────────────────────────┘                │
└─────────────────────────────────────────────────────────────────────┘
```

### 4.2 Workload Identity Implementation

#### 4.2.1 IAM Configuration

**Service Account Hierarchy:**
```
Project IAM
├── erlmcp-ksa (Kubernetes Service Account)
│   └── Workload Identity binding
│       └── erlmcp-sa@project.iam.gserviceaccount.com
│           ├── roles/secretmanager.secretAccessor
│           ├── roles/secretmanager.viewer
│           ├── roles/logging.logWriter
│           └── roles/monitoring.metricWriter
```

**Workload Identity Binding:**
```hcl
resource "google_service_account_iam_member" "workload_identity" {
  service_account_id = google_service_account.erlmcp[0].name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.helm_namespace}/${var.helm_release_name}]"
}
```

**Assessment:**
- Correct implementation of Workload Identity
- Minimal IAM roles (least privilege)
- No implicit credential usage

**Recommendations:**
1. Add workload identity federation for external systems
2. Implement IAM conditions for time-based access
3. Add IAM policy validation in CI/CD
4. Document IAM role requirements

### 4.3 Network Policy Enforcement

#### 4.3.1 GKE Network Policies

**Calico Policies Defined:**

1. **Ingress Policy:**
```yaml
# Allow traffic only from ingress controller
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-ingress
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  policyTypes:
  - Ingress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: ingress-nginx
    ports:
    - protocol: TCP
      port: 8080
```

2. **Egress Policy:**
```yaml
# Allow egress to kube-system (DNS) and database
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-egress
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  policyTypes:
  - Egress
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
    ports:
    - protocol: TCP
      port: 53
  - to:
    - namespaceSelector: {}
      podSelector:
        matchLabels:
          app: postgres
    ports:
    - protocol: TCP
      port: 5432
```

**Assessment:**
- Default-deny approach recommended but not enforced
- Specific policies for ingress/egress
- Missing: Inter-pod communication policies

**Recommendations:**
1. Implement default-deny ingress policy
2. Add pod-to-pod communication policies
3. Implement network policy logging
4. Add policy validation in admission controller

---

## 5. Key Component Diagram Descriptions

### 5.1 System Context Diagram

```
                    ┌─────────────────────────────────────────┐
                    │           Google Cloud Platform         │
                    │  ┌───────────────────────────────────┐  │
                    │  │   erlmcp Marketplace Deployment   │  │
                    │  │                                   │  │
                    │  │  ┌─────┐  ┌─────┐  ┌───────┐    │  │
                    │  │  │ GKE │  │ CR  │  │  GCE   │    │  │
                    │  │  │     │  │     │  │        │    │  │
                    │  │  └──┬──┘  └──┬──┘  └───┬───┘    │  │
                    │  └─────┼────────┼───────────┼───────┘  │
                    └──────────┼────────┼───────────┼──────────┘
                               │        │           │
                ┌──────────────┼────────┼───────────┼──────────────┐
                │              │        │           │              │
                ▼              ▼        ▼           ▼              ▼
        ┌─────────────┐ ┌───────────┐ ┌──────────┐ ┌─────────────┐
        │   Clients   │ │  GCP APIs │ │ Secret   │ │   Cloud     │
        │  (MCP       │ │ (Logging, │ │ Manager  │ │ Operations  │
        │  Clients)   │ │ Monitor)  │ │          │ │             │
        └─────────────┘ └───────────┘ └──────────┘ └─────────────┘
```

### 5.2 Container Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                        erlmcp Container                            │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Application Layer                          │  │
│  │  ┌───────────┐  ┌───────────┐  ┌─────────────────────────┐  │  │
│  │  │ JSON-RPC  │  │ Transport │  │    Session Management   │  │  │
│  │  │ Handler   │  │ Manager   │  │   (gen_statem)          │  │  │
│  │  └───────────┘  └───────────┘  └─────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Erlang/OTP Runtime                         │  │
│  │  ┌───────────┐  ┌───────────┐  ┌─────────────────────────┐  │  │
│  │  │  Process  │  │ Distribution │   │  Supervisor Tree    │  │  │
│  │  │  Pool     │  │  (inet_tls)│   │   (OTP supervision)   │  │  │
│  │  └───────────┘  └───────────┘  └─────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    Observability Layer                       │  │
│  │  ┌───────────┐  ┌───────────┐  ┌─────────────────────────┐  │  │
│  │  │ OpenTelemetry│ Prometheus │     │ Structured Logging    │  │  │
│  │  │  Exporter  │  │ Metrics   │     │   (JSON format)       │  │  │
│  │  └───────────┘  └───────────┘  └─────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  User: erlmcp (non-root)                                             │
│  Filesystem: Read-only (except /tmp, /var/run)                       │
│  Capabilities: ALL dropped                                           │
└─────────────────────────────────────────────────────────────────────┘
```

### 5.3 Data Flow Diagram

```
┌─────────────┐                                                     ┌─────────────┐
│   Client    │                                                     │   Client    │
│ (MCP Client)│                                                     │ (MCP Client)│
└──────┬──────┘                                                     └──────┬──────┘
       │                                                                   │
       │  1. Connect (TCP/HTTP/WebSocket)                                 │
       ▼                                                                   │
┌───────────────────────────────────────────────────────────────────────┐│
│                         Load Balancer                                   ││
│  - Global HTTPS LB (GKE/GCE)                                           ││
│  - Cloud Run ingress                                                   ││
└──────┬────────────────────────────────────────────────────────────────┘│
       │                                                                │
       │  2. Route to healthy instance                                   │
       ▼                                                                │
┌───────────────────────────────────────────────────────────────────────┐│
│                       erlmcp Instance                                  ││
│  ┌─────────────────────────────────────────────────────────────────┐  ││
│  │  3. Receive Request (Transport Layer)                            │  ││
│  │     - Validate TLS                                               │  ││
│  │     - Check rate limit                                           │  ││
│  └─────────────────────────────────────────────────────────────────┘  ││
│                           │                                           ││
│                           ▼                                           ││
│  ┌─────────────────────────────────────────────────────────────────┐  ││
│  │  4. Parse JSON-RPC (Handler Layer)                              │  ││
│  │     - Validate schema                                            │  ││
│  │     - Check authorization                                        │  ││
│  └─────────────────────────────────────────────────────────────────┘  ││
│                           │                                           ││
│                           ▼                                           ││
│  ┌─────────────────────────────────────────────────────────────────┐  ││
│  │  5. Execute Tool Call (Application Layer)                        │  ││
│  │     - Spawn Erlang process                                       │  ││
│  │     - Execute logic                                              │  ││
│  │     - Return result                                              │  ││
│  └─────────────────────────────────────────────────────────────────┘  ││
│                           │                                           ││
│                           ▼                                           ││
│  ┌─────────────────────────────────────────────────────────────────┐  ││
│  │  6. Emit Observability (OTEL Layer)                              │  ││
│  │     - Metrics to Cloud Monitoring                                │  ││
│  │     - Logs to Cloud Logging                                      │  ││
│  │     - Traces to Cloud Trace                                      │  ││
│  └─────────────────────────────────────────────────────────────────┘  ││
│                           │                                           ││
│                           ▼                                           ││
│  ┌─────────────────────────────────────────────────────────────────┐  ││
│  │  7. Response (Transport Layer)                                  │  ││
│  │     - Format JSON-RPC response                                   │  ││
│  │     - Send to client                                             │  ││
│  └─────────────────────────────────────────────────────────────────┘  ││
└─────────────────────────────────────────────────────┬─────────────────┘│
                                                    │                   │
                                                    │ 8. Response        │
                                                    ▼                   │
                                           ┌─────────────────┐         │
                                           │   Client        │◄────────┘
                                           │ (Result/Error)  │
                                           └─────────────────┘
```

---

## 6. Recommendations for Improvements

### 6.1 High Priority

1. **SSH Access Restriction**
   - Current: 0.0.0.0/0 allowed
   - Recommendation: Restrict to specific CIDR or use IAP
   - Impact: Security improvement

2. **Min Instances for Cloud Run**
   - Current: min_instances=0
   - Recommendation: min_instances=1 for production
   - Impact: Eliminate cold starts

3. **Regional MIG for Compute Engine**
   - Current: Single zone deployment
   - Recommendation: Regional MIG with 3 zones
   - Impact: Zone redundancy

4. **Default-Deny Network Policy**
   - Current: Explicit policies only
   - Recommendation: Add default-deny ingress
   - Impact: Zero-trust security

5. **Secret Rotation Automation**
   - Current: Manual process
   - Recommendation: Automated rotation with scheduled jobs
   - Impact: Reduced secret exposure

### 6.2 Medium Priority

1. **GPU Node Pool (GKE)**
   - For AI/ML workloads requiring GPU acceleration
   - Use T4 or A2 machine types
   - Separate node pool with taints

2. **Backup Cross-Region Replication**
   - Current: Regional backups
   - Recommendation: Multi-region replication
   - Impact: DR readiness

3. **Log-Based Metrics**
   - Add business KPIs as log-based metrics
   - Enable metric-based alerting on business events
   - Impact: Better observability

4. **Cost Management Alerts**
   - Implement budget alerts for deployments
   - Add SLO-based budget controls
   - Impact: Cost optimization

5. **Multi-Region Documentation**
   - Add multi-region deployment guide
   - Document traffic steering patterns
   - Impact: Operational readiness

### 6.3 Low Priority

1. **Custom Machine Types**
   - Right-size instances based on actual usage
   - Use custom machine types for cost optimization
   - Impact: Cost reduction

2. **VPC Service Controls**
   - Implement service perimeters for data egress prevention
   - Add context-aware access controls
   - Impact: Security enhancement

3. **Anthos Service Mesh**
   - Add service mesh for advanced traffic management
   - Implement mTLS for all service communication
   - Impact: Security and observability

4. **Terraform Module Refactoring**
   - Consider Terraform Cloud/Enterprise for state management
   - Add module versioning
   - Impact: Operational improvement

---

## 7. Risk Assessment with Mitigation Strategies

### 7.1 Risk Matrix

| Risk | Likelihood | Impact | Score | Mitigation |
|------|------------|--------|-------|------------|
| Public SSH access | Medium | High | 12 | Restrict to CIDR, use IAP |
| Single-zone GCE | Medium | High | 12 | Implement regional MIG |
| Cold starts (Cloud Run) | High | Medium | 12 | Set min_instances=1 |
| Secret exposure | Low | High | 8 | Implement rotation, audit logging |
| Zone failure (GKE) | Low | High | 8 | Regional cluster (already implemented) |
| Cost overrun | Medium | Medium | 8 | Budget alerts, quotas |
| API quota limits | Low | Medium | 4 | Monitor, request increases |
| Container vulnerabilities | Low | High | 8 | Regular scanning, patching |
| DDoS attack | Low | High | 8 | Cloud Armor, rate limiting |
| Data loss | Low | Critical | 10 | Backup, cross-region replication |

### 7.2 Mitigation Strategies

#### 7.2.1 Security Risks

**1. Unauthorized SSH Access**
- **Mitigation:**
  - Remove SSH rule or restrict to specific CIDR
  - Implement IAP for TCP tunneling
  - Use OS Login for centralized authentication
  - Enable audit logging for SSH attempts

**2. Secret Exposure**
- **Mitigation:**
  - Implement automatic secret rotation (90 days)
  - Add secret access alerts
  - Use Secret Manager CSI driver
  - Implement secret versioning

**3. Container Vulnerabilities**
- **Mitigation:**
  - Enable Binary Authorization
  - Implement vulnerability scanning in CI/CD
  - Use distroless images where possible
  - Regular patch management

#### 7.2.2 Availability Risks

**1. Single Zone Failure (Compute Engine)**
- **Mitigation:**
  - Implement regional MIG
  - Add health checks to all zones
  - Implement zone-aware routing
  - Test zone failure scenarios

**2. Cold Starts (Cloud Run)**
- **Mitigation:**
  - Set min_instances=1 for production
  - Implement request queuing
  - Use container startup optimization
  - Pre-warm instances during expected traffic

**3. Resource Exhaustion**
- **Mitigation:**
  - Implement autoscaling with appropriate limits
  - Add resource quotas
  - Monitor resource utilization
  - Implement priority classes

#### 7.2.3 Performance Risks

**1. Network Latency**
- **Mitigation:**
  - Deploy in regions close to users
  - Use Cloud CDN for static content
  - Implement caching where appropriate
  - Monitor latency metrics

**2. Memory Leaks**
- **Mitigation:**
  - Implement memory monitoring
  - Set up alerts for memory usage
  - Implement pod restart policy
  - Regular memory profiling

**3. Connection Exhaustion**
- **Mitigation:**
  - Implement connection pooling
  - Set appropriate concurrency limits
  - Monitor active connections
  - Implement circuit breakers

### 7.3 Testing Recommendations

**1. Chaos Engineering**
- Implement Pod chaos tests (random deletion)
- Test zone failure scenarios
- Test network partition scenarios
- Document MTTR for each scenario

**2. Load Testing**
- Test with realistic traffic patterns
- Identify breaking points
- Validate autoscaling behavior
- Document performance characteristics

**3. Security Testing**
- Regular penetration testing
- Vulnerability scanning
- Configuration audits
- Compliance validation

---

## 8. Deployment Comparison Matrix

### 8.1 Feature Comparison

| Feature | GKE | Cloud Run | Compute Engine |
|---------|-----|-----------|---------------|
| **SLA** | 99.95% | 99.9% | 99.9% |
| **Scalability** | 3-10 nodes | 0-100 instances | 2-10 instances |
| **Cold Starts** | No | Yes (if min=0) | No |
| **Managed Updates** | Yes | Yes | Manual |
| **Custom Networking** | Full | Limited | Full |
| **GPU Support** | Yes | No | Yes |
| **Private Cluster** | Yes | Internal-only | VPC |
| **Workload Identity** | Yes | Yes | Yes |
| **Network Policies** | Yes | N/A | Firewall |
| **Spot Instances** | Yes | N/A | Yes |
| **Complexity** | High | Low | Medium |
| **Best For** | Production, complex apps | Serverless, variable load | Legacy, custom config |

### 8.2 Cost Comparison

**Monthly Cost Estimates (us-central1):**

| Deployment | Base | Scale (10x) | Optimization |
|------------|------|-------------|--------------|
| GKE (3 nodes, e2-standard-4) | $150 | $300 | +$90 (spot: $60) |
| Cloud Run (0 instances) | $0 | $400 | N/A |
| Cloud Run (1 min instance) | $15 | $415 | N/A |
| Compute Engine (2 VMs, e2-standard-4) | $100 | $200 | +$60 (preemptible: $40) |

**Notes:**
- GKE includes control plane cost ($73/month)
- Cloud Run pricing based on requests/memory
- Compute Engine includes load balancer
- All prices approximate

---

## 9. Compliance and Security

### 9.1 Compliance Frameworks

**Supported Frameworks:**
- SOC 2 Type II
- ISO 27001
- GDPR (EU deployments)
- HIPAA (with BAA)
- PCI DSS (SAQ A)

**Compliance Features:**
- Audit logging (Cloud Audit Logs)
- Data encryption at rest (AES-256)
- Data encryption in transit (TLS 1.3)
- Access controls (IAM, Workload Identity)
- Secret management (Secret Manager)
- Network isolation (VPC, private clusters)

### 9.2 Security Controls

**Implemented Controls:**
1. Least privilege IAM roles
2. Network policy enforcement
3. Private clusters
4. Shielded nodes/VMs
5. Secret Manager integration
6. Audit logging
7. Vulnerability scanning
8. Binary authorization (optional)

**Recommended Additions:**
1. VPC Service Controls
2. Container Threat Detection
3. Security Command Center integration
4. Assured Workloads for government
5. Artifact Analysis for containers

---

## 10. Conclusion

The erlmcp GCP Marketplace deployment architecture is well-designed with enterprise-grade features across all three deployment options. The architecture demonstrates strong adherence to Google Cloud best practices for security, observability, and high availability.

### Strengths
- Comprehensive three-deployment model support
- Strong security posture with private clusters and Workload Identity
- Excellent observability integration with Cloud Operations
- Well-defined network architecture with VPC isolation
- Proper secret management with Secret Manager

### Areas for Improvement
- SSH access restriction required
- Regional MIG for Compute Engine
- Min instances setting for Cloud Run production
- Default-deny network policy
- Automated secret rotation

### Overall Recommendation
**APPROVED FOR PRODUCTION** with the implementation of high-priority recommendations listed in Section 6.1.

---

## Appendix A: Architecture Decision Records

### ADR-001: Deployment Model Selection
**Status:** Accepted
**Decision:** Support three deployment models (GKE, Cloud Run, Compute Engine)
**Rationale:** Customer choice based on requirements and expertise
**Consequences:** Increased maintenance burden, maximum flexibility

### ADR-002: Regional vs Zonal Clusters
**Status:** Accepted
**Decision:** Use regional GKE clusters
**Rationale:** 99.95% SLA requirement, zone redundancy
**Consequences:** Higher cost, improved availability

### ADR-003: Private Endpoint for Control Plane
**Status:** Accepted
**Decision:** Enable private endpoint for GKE control plane
**Rationale:** Security compliance, attack surface reduction
**Consequences:** Requires authorized network access, bastion host

### ADR-004: Workload Identity over Service Account Keys
**Status:** Accepted
**Decision:** Use Workload Identity for authentication
**Rationale:** Zero long-lived credentials, automatic rotation
**Consequences:** GKE-specific, learning curve

### ADR-005: Calico for Network Policies
**Status:** Accepted
**Decision:** Use Calico for network policy enforcement
**Rationale:** Google-recommended, feature-rich
**Consequences:** Additional component, complexity

---

## Appendix B: Configuration Reference

### B.1 Environment Variables

**Required Variables:**
```bash
ERLMCP_ENV=production
ERLMCP_VERSION=3.0.0
ERLMCP_NODE_NAME=erlmcp@<hostname>
ERL_AFLAGS=-proto_dist inet_tls
ERL_DIST_PORT=9100
```

**Optional Variables:**
```bash
OTEL_EXPORTER_OTLP_ENDPOINT=monitoring.googleapis.com:443
OTEL_SERVICE_NAME=erlmcp
GOOGLE_CLOUD_PROJECT=<project-id>
```

### B.2 Health Check Endpoints

| Endpoint | Purpose | Response |
|----------|---------|----------|
| GET /health | Liveness probe | {"status": "ok"} |
| GET /ready | Readiness probe | {"status": "ready"} |
| GET /metrics | Prometheus metrics | Text format |
| GET /status | Detailed status | JSON with components |

### B.3 Port References

| Port | Protocol | Purpose |
|------|----------|---------|
| 8080 | HTTP | JSON-RPC API |
| 9090 | HTTP | Metrics endpoint |
| 9100 | TCP | Erlang distribution |
| 443 | HTTPS | JSON-RPC API (TLS) |

---

**Document Version:** 1.0
**Last Updated:** 2026-02-02
**Next Review:** 2026-03-02
**Approved By:** System Architecture Designer
