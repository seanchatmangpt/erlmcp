# Marketplace Reviewer Simulation - Cost Analysis

**Report Version:** 1.0.0
**Date:** 2026-02-02
**Simulation Phase:** Cost Analysis & Budget Planning
**Review Type:** Production-Ready Cost Validation

---

## Executive Summary

This document provides a comprehensive cost analysis for the erlmcp v3 Google Cloud Marketplace deployment across all three supported deployment paths: Cloud Run (serverless), GKE (regional), and Compute Engine (MIG). Cost projections are provided for short-term testing (1 hour), full validation (4 hours), and extended testing (24 hours), along with monthly production estimates.

**Key Findings:**
- **Cloud Run**: Lowest cost for testing ($0.01-$0.50/hour), pay-per-use pricing
- **GKE Regional**: Moderate base cost ($0.74-$2.47/hour), best for production
- **Compute Engine**: Predictable pricing ($0.38-$0.55/hour), simplest for migration

**Cost Optimization Potential:**
- **Spot/Preemptible VMs**: 60-80% savings on compute
- **Auto-scaling to zero**: Cloud Run min_instances=0 saves idle costs
- **Right-sizing**: e2-medium vs e2-standard-2 saves ~40%

---

## Part 1: GCP Resource Pricing Reference

### 1.1 Compute Engine Pricing (us-central1)

| Machine Type | vCPUs | Memory | Hourly | Monthly (730h) |
|--------------|-------|--------|--------|---------------|
| **e2-medium** | 2 | 4 GB | $0.041 | $29.93 |
| **e2-standard-2** | 2 | 8 GB | $0.096 | $70.08 |
| **e2-standard-4** | 4 | 16 GB | $0.191 | $139.43 |
| **e2-highmem-2** | 2 | 16 GB | $0.138 | $100.74 |
| **n2-standard-2** | 2 | 8 GB | $0.124 | $90.52 |
| **n2-standard-4** | 4 | 16 GB | $0.248 | $181.04 |

**Spot/Preemptible Discount:** 60-80% off standard pricing

**Premium for Shielded VM:** +10-15% (approximately $0.01-$0.02/hour per VM)

### 1.2 Disk Storage Pricing

| Disk Type | Cost per GB/month | Cost per GB/hour | Notes |
|-----------|------------------|------------------|-------|
| **pd-standard** | $0.040 | $0.000055 | Standard persistent disk |
| **pd-balanced** | $0.065 | $0.000089 | Balanced performance |
| **pd-ssd** | $0.136 | $0.000186 | SSD performance |
| **pd-extreme** | $0.228 | $0.000312 | Extreme performance |

**Example:** 100GB pd-balanced disk = $6.50/month = ~$0.009/hour

### 1.3 Cloud Run Pricing

| Component | Unit Price | Notes |
|-----------|------------|-------|
| **CPU** | $0.00000925 per vCPU-second | ~$0.033 per vCPU-hour |
| **Memory** | $0.000000425 per GiB-second | ~$0.0011 per GiB-hour |
| **Requests** | $0.40 per million requests | First 2M free/month |
| **Networking** | $0.12 per GB egress | Free ingress |

**Configuration: 1 vCPU, 512MiB**
- CPU cost: $0.033/hour per instance
- Memory cost: 512MiB x $0.0011 = $0.0006/hour per instance
- **Base compute: ~$0.034/hour per active instance**

### 1.4 GKE Cluster Pricing

| Component | Cost | Notes |
|-----------|------|-------|
| **Cluster Management** | $0.10/hour per cluster | Flat fee for regional cluster |
| **Node Pools** | Per VM pricing (see above) | EKS-style per-node billing |
| **Load Balancer** | $0.018/hour | Regional HTTP/S load balancer |
| **NAT Gateway** | $0.045/hour + $0.045/GB | Cloud NAT per gateway |

**Control Plane (Regional):** $0.10/hour = $73/month

### 1.5 Secret Manager Pricing

| Component | Price | Notes |
|-----------|-------|-------|
| **Active Versions** | $0.03 per month per secret | 6+ secrets = $0.18/month |
| **Storage** | $0.03 per GiB per month | Negligible for small secrets |
| **Access Operations** | $0.03 per 10,000 access calls | Included for testing volumes |

### 1.6 Cloud Monitoring & Logging

| Component | Price | Notes |
|-----------|-------|-------|
| **Metrics** | $0.25 per million metric points | ~$0.00025 per metric point |
| **Logs** | $0.50 per GiB ingested | First 50GB free/month |
| **Log Storage** | $0.006 per GiB per month | 30-day retention default |
| **Uptime Checks** | $0.30 per check per 1M requests | Negligible for testing |

### 1.7 Network Pricing

| Component | Price | Notes |
|-----------|-------|-------|
| **Egress** | $0.12 per GB to internet | Free ingress to GCP |
| **NAT Data Processing** | $0.045 per GB | Cloud NAT only |
| **Static IP** | $0.005/hour | $3.60/month if unused |

### 1.8 Artifact Registry Pricing

| Component | Price | Notes |
|-----------|-------|-------|
| **Storage** | $0.10 per GB/month | Container image storage |
| **Download** | $0.05 per GB | Egress pricing applies |

---

## Part 2: Cost Breakdown by Deployment Type

### 2.1 Cloud Run Deployment (Serverless)

**Configuration from `terraform/modules/cloud-run/main.tf`:**
- CPU: 1 vCPU
- Memory: 512 MiB
- Min Instances: 0
- Max Instances: 100
- Concurrency: 80

#### Resource Breakdown

| Resource | Quantity | Unit Cost | Hourly Cost | Notes |
|----------|----------|-----------|-------------|-------|
| Cloud Run Service | 1 (active) | $0.034/vCPU-hr | $0.034 | 1 vCPU + 512MiB |
| Cloud Run Requests | ~1M | $0.40/M | $0.0004 | ~1000 test requests |
| Secret Manager (11 secrets) | 11 | $0.000001/sec | $0.00004 | Negligible |
| Cloud Logging | ~1GB | $0.50/GB | $0.001 | Test volume logs |
| Cloud Monitoring (metrics) | ~5 | $0.25/M | $0.00001 | 5 custom metrics |
| Cloud Monitoring (uptime) | 1 | $0.30/check | $0.00001 | 1 uptime check |
| **TOTAL (Active)** | - | - | **$0.036** | Per hour of active use |

**Idle Cost (min_instances=0):** $0.00 (scales to zero)

#### Duration-Based Costs

| Duration | Instance State | Compute | Logs/Monitoring | Total |
|----------|---------------|---------|----------------|-------|
| **1 Hour** | Active | $0.034 | $0.002 | **$0.04** |
| **4 Hours** | Active | $0.136 | $0.008 | **$0.14** |
| **24 Hours** | Intermittent | $0.50* | $0.04 | **$0.54** |

*Assumes 50% active time for 24h period

#### Monthly Production Estimate

**Scenario: Light Production (avg 2 instances, 50% utilization)**
- Compute: 2 instances x 12h/day x 30 days x $0.034 = $24.48
- Requests: 10M/month x $0.40/M = $4.00
- Logs/Monitoring: $10.00
- **Monthly Total: ~$38-50**

**Scenario: Heavy Production (avg 10 instances, 80% utilization)**
- Compute: 10 instances x 20h/day x 30 days x $0.034 = $204.00
- Requests: 100M/month x $0.40/M = $40.00
- Logs/Monitoring: $50.00
- **Monthly Total: ~$294**

---

### 2.2 GKE Regional Deployment

**Configuration from `terraform/modules/gke/variables.tf`:**
- Cluster Type: Regional (us-central1)
- Node Pool: e2-standard-2, 3 nodes minimum
- Disk: 100GB pd-balanced per node
- Control Plane: Regional HA (99.95% SLA)

#### Resource Breakdown

| Resource | Quantity | Unit Cost | Hourly Cost | Notes |
|----------|----------|-----------|-------------|-------|
| GKE Control Plane | 1 cluster | $0.10/hr | $0.10 | Regional cluster fee |
| GKE Nodes (e2-standard-2) | 3 min | $0.096/hr | $0.288 | Minimum node pool |
| Node Disk (100GB pd-balanced) | 3 | $0.009/hr | $0.027 | 100GB x 3 nodes |
| Network Load Balancer | 1 | $0.018/hr | $0.018 | Regional HTTP/S LB |
| Cloud NAT | 1 | $0.045/hr | $0.045 | Egress from private nodes |
| Secret Manager (11 secrets) | 11 | $0.00004/hr | $0.0004 | Negligible |
| Cloud Logging | ~5GB | $0.50/GB | $0.01 | Cluster + workload logs |
| Cloud Monitoring | 10 metrics | $0.25/M | $0.0001 | System + custom metrics |
| Managed Prometheus | 1 cluster | Included | $0.00 | Included with GKE |
| **TOTAL (Minimum)** | - | - | **$0.49** | 3-node minimum |

#### Autoscaling Impact

**Node Pool Autoscaling (3-10 nodes):**

| Active Nodes | Compute Cost | Disk Cost | Total Hourly |
|--------------|--------------|-----------|--------------|
| 3 nodes (min) | $0.288 | $0.027 | $0.315 |
| 4 nodes | $0.384 | $0.036 | $0.420 |
| 5 nodes | $0.480 | $0.045 | $0.525 |
| 6 nodes | $0.576 | $0.054 | $0.630 |
| 7 nodes | $0.672 | $0.063 | $0.735 |
| 8 nodes | $0.768 | $0.072 | $0.840 |
| 9 nodes | $0.864 | $0.081 | $0.945 |
| 10 nodes (max) | $0.960 | $0.090 | $1.050 |

#### Duration-Based Costs

| Duration | Nodes | Base Cluster | Compute | Storage | Network | Logs/Monitoring | Total |
|----------|-------|--------------|---------|---------|---------|----------------|-------|
| **1 Hour** | 3 | $0.10 | $0.288 | $0.027 | $0.063 | $0.010 | **$0.49** |
| **4 Hours** | 3-4 avg | $0.40 | $1.15 | $0.11 | $0.25 | $0.04 | **$1.95** |
| **24 Hours** | 3-5 avg | $2.40 | $8.40 | $0.81 | $1.50 | $0.24 | **$13.35** |

#### Monthly Production Estimate

**Scenario: Production (3-6 nodes average)**
- Control Plane: $0.10 x 730 = $73.00
- Compute (avg 4.5 nodes): 4.5 x $0.096 x 730 = $315.36
- Storage: 4.5 x 100GB x $0.065 = $29.25
- Network LB: $0.018 x 730 = $13.14
- Cloud NAT: $0.045 x 730 = $32.85
- Logs/Monitoring: $50.00
- **Monthly Total: ~$514**

**Scenario: Heavy Production (6-10 nodes average)**
- Control Plane: $73.00
- Compute (avg 8 nodes): 8 x $0.096 x 730 = $560.64
- Storage: 8 x 100GB x $0.065 = $52.00
- Network LB: $13.14
- Cloud NAT: $32.85
- Logs/Monitoring: $100.00
- **Monthly Total: ~$831**

---

### 2.3 Compute Engine (MIG) Deployment

**Configuration from `terraform/modules/compute-engine/variables.tf`:**
- Machine Type: e2-medium (2 vCPU, 4GB)
- Instance Count: 1-10 (auto-scaling)
- Disk: 20GB pd-balanced
- Image: Ubuntu 22.04 LTS

#### Resource Breakdown

| Resource | Quantity | Unit Cost | Hourly Cost | Notes |
|----------|----------|-----------|-------------|-------|
| Compute Instance (e2-medium) | 1 min | $0.041/hr | $0.041 | Base VM cost |
| Boot Disk (20GB pd-balanced) | 1 | $0.002/hr | $0.002 | 20GB x $0.10/GB/mo |
| Static IP (optional) | 1 | $0.005/hr | $0.005 | If enabled |
| Secret Manager (11 secrets) | 11 | $0.00004/hr | $0.0004 | Negligible |
| Cloud Logging | ~2GB | $0.50/GB | $0.002 | VM logs |
| Cloud Monitoring (Ops Agent) | 1 VM | Included | $0.00 | Free with VM |
| Shielded VM premium | 1 | ~15% | $0.006 | Secure boot + vTPM |
| **TOTAL (Minimum)** | - | - | **$0.056** | Single instance |

#### Auto-Scaling Impact (MIG)

**Instance Group Autoscaling (1-10 instances):**

| Active Instances | Compute Cost | Disk Cost | Shielded Premium | Total Hourly |
|-----------------|--------------|-----------|-----------------|--------------|
| 1 instance (min) | $0.041 | $0.002 | $0.006 | $0.049 |
| 2 instances | $0.082 | $0.004 | $0.012 | $0.098 |
| 3 instances | $0.123 | $0.006 | $0.018 | $0.147 |
| 4 instances | $0.164 | $0.008 | $0.024 | $0.196 |
| 5 instances | $0.205 | $0.010 | $0.030 | $0.245 |
| 6 instances | $0.246 | $0.012 | $0.036 | $0.294 |
| 7 instances | $0.287 | $0.014 | $0.042 | $0.343 |
| 8 instances | $0.328 | $0.016 | $0.048 | $0.392 |
| 9 instances | $0.369 | $0.018 | $0.054 | $0.441 |
| 10 instances (max) | $0.410 | $0.020 | $0.060 | $0.490 |

#### Duration-Based Costs

| Duration | Instances | Compute | Storage | Shielded | Logs | Total |
|----------|-----------|---------|---------|----------|------|-------|
| **1 Hour** | 1 | $0.041 | $0.002 | $0.006 | $0.002 | **$0.05** |
| **4 Hours** | 1-2 avg | $0.164 | $0.008 | $0.024 | $0.008 | **$0.20** |
| **24 Hours** | 1-3 avg | $2.46 | $0.12 | $0.36 | $0.05 | **$3.00** |

#### Monthly Production Estimate

**Scenario: Light Production (1-2 instances average)**
- Compute (avg 1.5): 1.5 x $0.041 x 730 = $44.90
- Storage: 20GB x $0.10 = $2.00
- Shielded VM: 1.5 x $0.006 x 730 = $6.57
- Logs/Monitoring: $15.00
- **Monthly Total: ~$68**

**Scenario: Production (2-4 instances average)**
- Compute (avg 3): 3 x $0.041 x 730 = $89.79
- Storage: 3 x 20GB x $0.10 = $6.00
- Shielded VM: 3 x $0.006 x 730 = $13.14
- Logs/Monitoring: $30.00
- **Monthly Total: ~$139**

---

## Part 3: Cost Comparison Summary

### 3.1 Hourly Cost Comparison

| Deployment Type | Minimum | Average (4h) | Maximum | Notes |
|-----------------|---------|--------------|----------|-------|
| **Cloud Run** | $0.00 (idle) | $0.04 | $0.50 | Scale-to-zero, pay-per-use |
| **GKE Regional** | $0.49 | $0.49 | $1.47 | 3-node minimum always on |
| **Compute Engine** | $0.05 | $0.05 | $0.49 | 1-instance minimum |

### 3.2 Testing Duration Cost Matrix

| Duration | Cloud Run | GKE (3 nodes) | Compute Engine (1 VM) |
|----------|-----------|--------------|----------------------|
| **1 Hour** | $0.04 | $0.49 | $0.05 |
| **2 Hours** | $0.07 | $0.97 | $0.10 |
| **4 Hours** | $0.14 | $1.95 | $0.20 |
| **8 Hours** | $0.28 | $3.89 | $0.40 |
| **12 Hours** | $0.42 | $5.84 | $0.60 |
| **24 Hours** | $0.54 | $13.35 | $3.00 |

### 3.3 Monthly Production Cost Comparison

| Tier | Cloud Run | GKE Regional | Compute Engine | Recommended For |
|------|-----------|--------------|----------------|-----------------|
| **Dev/Test** | $38-50 | $514 | $68 | Cloud Run (scales to zero) |
| **Production** | $294 | $831 | $139 | Compute Engine (predictable) |
| **Enterprise** | $500+ | $1,200+ | $300+ | GKE Regional (HA) |

### 3.4 Cost Efficiency Ranking

**For Testing/Development:**
1. **Cloud Run** - Lowest cost, scales to zero
2. **Compute Engine** - Predictable, single VM
3. **GKE** - Highest base cost (cluster fee)

**For Production:**
1. **Compute Engine** - Most cost-effective for small deployments
2. **GKE Regional** - Best value for HA requirements
3. **Cloud Run** - Cost-effective at low utilization, expensive at scale

---

## Part 4: Cost Optimization Recommendations

### 4.1 Compute Optimization

#### 4.1.1 Use Spot/Preemptible VMs

**Savings: 60-80% on compute costs**

**Configuration Example:**
```hcl
# GKE Spot Node Pool
resource "google_container_node_pool" "spot" {
  name     = "${var.cluster_name}-spot-pool"
  node_config {
    spot          = true
    preemptible   = true
    machine_type  = "e2-standard-2"
  }
  autoscaling {
    min_node_count = 0
    max_node_count = 5
  }
}
```

**Impact:**
- GKE: $0.288/hour -> $0.058-$0.115/hour (60-80% savings)
- Compute Engine: $0.041/hour -> $0.008-$0.016/hour (60-80% savings)

**Trade-offs:**
- VMs can be pre-empted with 30s notice
- Not suitable for stateful workloads
- Best for batch processing, fault-tolerant workloads

#### 4.1.2 Right-Size Machine Types

**Savings: Up to 60% by selecting appropriate machine type**

**Machine Type Selection Guide:**

| Workload | Recommended Type | Savings vs e2-standard-4 |
|----------|-----------------|-------------------------|
| Light API server | e2-medium | 78% |
| Medium API server | e2-standard-2 | 50% |
| Memory-intensive | e2-highmem-2 | 28% |
| Compute-intensive | e2-standard-4 | baseline |

**Example:** 3-node GKE cluster
- e2-standard-4: $0.573/hour
- e2-standard-2: $0.288/hour (50% savings)
- e2-medium: $0.123/hour (78% savings)

#### 4.1.3 Enable Auto-Scaling

**Savings: Scale to zero when idle**

**Cloud Run Configuration:**
```hcl
resource "google_cloud_run_service" "erlmcp" {
  template {
    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale" = "0"  # Scale to zero
        "autoscaling.knative.dev/maxScale" = "10"
      }
    }
  }
}
```

**GKE Configuration:**
```hcl
resource "google_container_node_pool" "primary" {
  autoscaling {
    min_node_count  = 1  # Reduce from 3 to 1
    max_node_count  = 10
  }
}
```

**Impact:**
- Cloud Run: $0.00 when idle
- GKE: Save $0.19/hour by reducing min nodes from 3 to 1

### 4.2 Storage Optimization

#### 4.2.1 Use Appropriate Disk Types

**Savings: Up to 70% on storage costs**

| Disk Type | Price | Best For | Avoid For |
|-----------|-------|----------|-----------|
| **pd-standard** | $0.04/GB | Boot disks, infrequent access | High-IOPS workloads |
| **pd-balanced** | $0.065/GB | General workloads | Cost-sensitive |
| **pd-ssd** | $0.14/GB | Databases, high IOPS | Boot disks |
| **pd-extreme** | $0.23/GB | Extreme performance | Most workloads |

**Example:** 100GB disk
- pd-standard: $4.00/month
- pd-balanced: $6.50/month
- pd-ssd: $14.00/month

**Recommendation:** Use pd-standard for boot disks, pd-ssd only for database/data disks

#### 4.2.2 Right-Size Disk Capacity

**Savings: Avoid over-provisioning**

| Minimum Recommended | Typical Usage | Over-provisioned |
|--------------------|---------------|------------------|
| 20GB (boot) | 50GB (data) | 100GB+ (boot) |

**Action:** Monitor disk usage and right-size periodically
```bash
# Check disk usage
gcloud compute disks list --format="table(name,sizeGb,type)"
```

### 4.3 Network Optimization

#### 4.3.1 Minimize Egress Costs

**Savings: Up to 100% on data transfer**

| Strategy | Savings | Implementation |
|----------|---------|----------------|
| Use Cloud CDN | 90%+ | Cache static content |
| Private Google Access | 100% (NAT) | VPC egress to GCP services |
| Deploy to same region | Varies | Co-locate resources |

**Configuration:**
```hcl
# Enable private Google access
resource "google_compute_subnetwork" "subnet" {
  private_ip_google_access = true
}
```

#### 4.3.2 Eliminate Unused Static IPs

**Savings: $3.60/month per unused IP**

```bash
# List static IPs
gcloud compute addresses list

# Delete unused IPs
gcloud compute addresses delete UNUSED_IP_NAME --region=us-central1
```

### 4.4 Monitoring & Logging Optimization

#### 4.4.1 Use Log Exclusions

**Savings: Up to 80% on logging costs**

```hcl
# Exclude health check logs
resource "google_logging_project_exclusion" "health_checks" {
  name        = "exclude-health-checks"
  filter      = "resource.type=\"http_load_balancer\" AND request_path=\"/health\""
}
```

#### 4.4.2 Configure Log Retention

**Savings: Up to 75% by reducing retention**

| Retention | Cost per GB | Monthly Cost (100GB) |
|-----------|------------|---------------------|
| 1 day | $0.0026 | $0.26 |
| 7 days | $0.0052 | $0.52 |
| 30 days (default) | $0.006 | $0.60 |
| 365 days | $0.008 | $0.80 |

### 4.5 Secret Manager Optimization

**Savings: Minimal, but good practice**

- Use automatic replication (default)
- Avoid multiple secret versions
- Consolidate secrets where possible

---

## Part 5: Budget Alert Thresholds

### 5.1 Recommended Budget Levels

#### 5.1.1 For Reviewer Testing (Short-term)

| Budget Type | Amount | Trigger | Action |
|-------------|--------|---------|--------|
| **Warning** | $1.00 | At $0.50 | Review deployment duration |
| **Critical** | $5.00 | At $2.50 | Prepare to tear down |
| **Shutdown** | $10.00 | At $10.00 | Auto-terminate resources |

#### 5.1.2 For Development (Monthly)

| Budget Type | Amount | Trigger | Action |
|-------------|--------|---------|--------|
| **Warning** | $50.00 | At $25.00 | Review scaling settings |
| **Critical** | $150.00 | At $100.00 | Right-size resources |
| **Hard Limit** | $300.00 | At $300.00 | Block new deployments |

#### 5.1.3 For Production (Monthly)

| Budget Type | Amount | Trigger | Action |
|-------------|--------|---------|--------|
| **Warning** | $500.00 | At $400.00 | Review utilization |
| **Critical** | $1,500.00 | At $1,200.00 | Ops team review |
| **Hard Limit** | $3,000.00 | At $3,000.00 | C-level notification |

### 5.2 Budget Configuration Example

**Using GCP Budgets:**

```bash
# Create budget for reviewer testing
gcloud billing budgets create BILLING_ACCOUNT_ID \
  --displayName="erlmcp-Reviewer-Testing" \
  --budget-amount=10.00 \
  --threshold-rule=percent=50:spend-basis=current-spend \
  --threshold-rule=percent=90:spend-basis=current-spend

# Create budget for development
gcloud billing budgets create BILLING_ACCOUNT_ID \
  --displayName="erlmcp-Development" \
  --budget-amount=300.00 \
  --threshold-rule=percent=50:spend-basis=current-spend \
  --threshold-rule=percent=80:spend-basis=current-spend \
  --pubsub-topic=projects/PROJECT_ID/topics/budget-alerts
```

### 5.3 Alert Notification Setup

#### 5.3.1 Email Alerts

```bash
# Set up budget alert email
gcloud billing budgets update BUDGET_ID \
  --notification-email-address=ops@company.com
```

#### 5.3.2 Pub/Sub Alerts (for automation)

```bash
# Create Pub/Sub topic for budget alerts
gcloud pubsub topics create budget-alerts

# Create subscription for automation
gcloud pubsub subscriptions create budget-alert-sub \
  --topic=budget-alerts

# Link budget to Pub/Sub
gcloud billing budgets update BUDGET_ID \
  --pubsub-topic=budget-alerts
```

#### 5.3.3 Automated Shutdown Script

```bash
#!/bin/bash
# auto-shutdown-on-budget.sh

PROJECT_ID="your-project-id"
BILLING_ACCOUNT_ID="your-billing-account-id"
BUDGET_AMOUNT="10.00"

# Create budget with auto-shutdown
gcloud billing budgets create $BILLING_ACCOUNT_ID \
  --displayName="Auto-Shutdown Budget" \
  --budget-amount=$BUDGET_AMOUNT \
  --threshold-rule=percent=100:spend-basis=current-spend \
  --pubsub-topic=projects/$PROJECT_ID/topics/budget-shutdown

# Cloud Function to handle shutdown
cat > shutdown_function.js << 'EOF'
exports.shutdownOnBudget = async (event, context) => {
  const {PubSub} = require('@google-cloud/pubsub');
  const {compute} = require('@google-cloud/compute');

  // Parse budget alert
  const message = JSON.parse(Buffer.from(event.data, 'base64').toString());

  if (message.alertThresholdExceeded) {
    // Shutdown GKE cluster
    await compute.zone('us-central1-a').vm('erlmcp-server').stop();

    // Or scale down GKE
    // await gkeCluster.setNodePoolSize(1);

    console.log('Resources shut down due to budget limit');
  }
};
EOF

# Deploy Cloud Function
gcloud functions deploy budget-shutdown \
  --runtime=nodejs14 \
  --trigger-topic=budget-shutdown \
  --source=./shutdown_function
```

### 5.4 Cost Monitoring Dashboard

**Key Metrics to Track:**

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| Hourly Compute Cost | >$0.50 | >$1.00 | Review instance count |
| Daily Compute Cost | >$10.00 | >$20.00 | Right-scale resources |
| Monthly Egress (GB) | >100 GB | >500 GB | Review data transfers |
| Log Volume (GB/day) | >5 GB | >20 GB | Check log exclusions |
| Disk Utilization | >80% | >95% | Plan expansion or cleanup |

---

## Part 6: Cost Calculator

### 6.1 Quick Reference Formulas

**Cloud Run:**
```
Hourly Cost = (Instances x $0.034) + (Requests x $0.0000004) + $0.01
```

**GKE Regional:**
```
Hourly Cost = $0.10 + (Nodes x $0.096) + (Nodes x $0.009) + $0.063 + $0.01
           = $0.173 + (Nodes x $0.105)
```

**Compute Engine:**
```
Hourly Cost = (Instances x $0.041) + (Instances x $0.002) + $0.008
           = (Instances x $0.051)
```

### 6.2 Estimation Examples

**Example 1: 4-hour reviewer test on Cloud Run**
```
Cost = 4 x $0.04 = $0.16
```

**Example 2: 8-hour production validation on GKE (3 nodes)**
```
Cost = 8 x ($0.173 + 3 x $0.105) = 8 x $0.488 = $3.90
```

**Example 3: 24-hour load test on Compute Engine (2 instances)**
```
Cost = 24 x (2 x $0.051) = 24 x $0.102 = $2.45
```

---

## Part 7: Summary & Recommendations

### 7.1 Cost Summary Table

| Scenario | Cloud Run | GKE Regional | Compute Engine | Recommendation |
|----------|-----------|--------------|----------------|----------------|
| **1-Hour Test** | $0.04 | $0.49 | $0.05 | Cloud Run |
| **4-Hour Validation** | $0.14 | $1.95 | $0.20 | Cloud Run |
| **24-Hour Extended** | $0.54 | $13.35 | $3.00 | Compute Engine |
| **Dev Environment (mo)** | $38-50 | $514 | $68 | Cloud Run |
| **Production (mo)** | $294 | $831 | $139 | Compute Engine |
| **Enterprise HA (mo)** | $500+ | $1,200+ | $300+ | GKE Regional |

### 7.2 Deployment Recommendations

**For Google Marketplace Reviewers:**
- **Use Cloud Run** for fastest, lowest-cost validation
- Estimated reviewer testing cost: **$0.50-2.00**
- Cleanest teardown (scale to zero)

**For Development:**
- **Use Cloud Run** with scale-to-zero
- Estimated monthly cost: **$38-50**
- Best for intermittent use

**For Production:**
- **Use GKE Regional** for HA requirements
- Estimated monthly cost: **$514-831**
- Best for 99.95% SLA requirements

**For Cost-Optimized Production:**
- **Use Compute Engine** with spot VMs
- Estimated monthly cost: **$68-139**
- Best for predictable workloads

### 7.3 Final Cost Checklist

Before deploying:

- [ ] Set appropriate budget alerts
- [ ] Configure auto-scaling to minimum
- [ ] Enable spot/preemptible VMs where possible
- [ ] Use pd-standard for non-data disks
- [ ] Configure log exclusions for health checks
- [ ] Set up cost monitoring dashboard
- [ ] Document expected monthly costs
- [ ] Plan for cleanup after testing

---

## Appendix A: GCP Pricing References

**Official Pricing Pages:**
- Compute Engine: https://cloud.google.com/compute/pricing
- Cloud Run: https://cloud.google.com/run/pricing
- GKE: https://cloud.google.com/kubernetes-engine/pricing
- Cloud Storage: https://cloud.google.com/storage/pricing
- Secret Manager: https://cloud.google.com/secret-manager/pricing
- Cloud Monitoring: https://cloud.google.com/monitoring/pricing
- Cloud Logging: https://cloud.google.com/logging/pricing

**Pricing Calculator:**
https://cloud.google.com/products/calculator

---

**Report Prepared For:** Google Cloud Marketplace Review Team
**Technology Stack:** Erlang/OTP, Google Cloud Platform
**Cost Analysis Date:** February 2, 2026
**Report Version:** 1.0.0

---

*This document provides cost estimates based on GCP us-central1 pricing as of February 2026. Actual costs may vary based on usage patterns, region selection, and GCP pricing changes.*
