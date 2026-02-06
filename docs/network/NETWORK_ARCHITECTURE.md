# erlmcp v3 Network Architecture

## Overview

This document describes the enterprise-grade network architecture for erlmcp v3, supporting both GCP and AWS cloud deployments with zero-trust security, high availability, and comprehensive observability.

## Architecture Principles

1. **Zero-Trust Security**: Default-deny firewalls, private endpoints, no public IPs
2. **Defense in Depth**: Multiple security layers (SGs, NACLs, firewalls, Cloud Armor/WAF)
3. **High Availability**: Multi-AZ deployment, redundant NAT gateways
4. **Observable**: 100% flow logging, structured logs, real-time metrics
5. **Cost-Optimized**: Gateway endpoints where possible, efficient NAT usage
6. **Compliant**: SOC 2, HIPAA, GDPR ready with audit trails

## Network Topology

### GCP Network Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ erlmcp-vpc (10.0.0.0/16) - GCP                                      │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Control Plane Subnet (10.0.0.0/24)                             │ │
│ │ ┌──────────────────┐  ┌──────────────────┐                     │ │
│ │ │ GKE Masters      │  │ Management VMs   │                     │ │
│ │ │ Private Endpoint │  │ Jump Hosts (IAP) │                     │ │
│ │ └──────────────────┘  └──────────────────┘                     │ │
│ │                                                                  │ │
│ │ Secondary Ranges:                                               │ │
│ │ • gke-pods (10.1.0.0/16) - 65k IPs                             │ │
│ │ • gke-services (10.2.0.0/16) - 65k IPs                         │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Data Plane Subnet (10.0.1.0/24)                                │ │
│ │ ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │ │
│ │ │ Erlang Node 1│  │ Erlang Node 2│  │ Erlang Node N│          │ │
│ │ │ (GCE/GKE)    │  │ (GCE/GKE)    │  │ (GCE/GKE)    │          │ │
│ │ └──────────────┘  └──────────────┘  └──────────────┘          │ │
│ │ Ports: 4369 (EPMD), 9000-9999 (Distributed Erlang)            │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Management Subnet (10.0.10.0/24)                               │ │
│ │ ┌────────────────────┐  ┌────────────────────┐                │ │
│ │ │ Observability Stack│  │ CI/CD Runners      │                │ │
│ │ │ Prometheus/Grafana │  │ CloudBuild Triggers│                │ │
│ │ └────────────────────┘  └────────────────────┘                │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Network Services                                                │ │
│ │ ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │ │
│ │ │ Cloud NAT       │  │ Cloud Router    │  │ Cloud Armor     │ │ │
│ │ │ (Regional HA)   │  │ (BGP ASN 65001) │  │ DDoS Protection │ │ │
│ │ └─────────────────┘  └─────────────────┘  └─────────────────┘ │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Private Service Access                                          │ │
│ │ ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │ │
│ │ │ Cloud SQL   │  │ Memorystore │  │ Secret Manager          │ │ │
│ │ │ (PostgreSQL)│  │ (Redis)     │  │ (Private Connection)    │ │ │
│ │ └─────────────┘  └─────────────┘  └─────────────────────────┘ │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Firewall Rules (Hierarchical)                                   │ │
│ │ Priority 1000: Allow Internal (10.0.0.0/8)                     │ │
│ │ Priority 1001: Allow SSH (IAP tunnel 35.235.240.0/20)         │ │
│ │ Priority 1002: Allow Health Checks (130.211.0.0/22)           │ │
│ │ Priority 1003: Allow HTTPS (Conditional)                       │ │
│ │ Priority 1004: Allow GKE Master (10.0.0.0/24)                  │ │
│ │ Priority 1010: Allow Erlang Distribution (4369, 9000-9999)     │ │
│ │ Priority 1011: Allow MCP Protocol (8080, 8443, 9090)           │ │
│ │ Priority 65534: Deny All (Default)                             │ │
│ └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ VPC Flow Logs → Cloud Logging → BigQuery (Analysis)               │
│ 100% Sampling | 5-second aggregation | INCLUDE_ALL_METADATA       │
└────────────────────────────────────────────────────────────────────┘
```

### AWS Network Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ erlmcp-vpc (10.0.0.0/16) - AWS                                      │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Public Subnets (3 AZs)                                          │ │
│ │ 10.0.101.0/24 (us-east-1a)                                      │ │
│ │ 10.0.102.0/24 (us-east-1b)                                      │ │
│ │ 10.0.103.0/24 (us-east-1c)                                      │ │
│ │                                                                  │ │
│ │ ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────────────┐  │ │
│ │ │ NAT GW 1 │  │ NAT GW 2 │  │ NAT GW 3 │  │ Internet GW    │  │ │
│ │ │ (AZ-a)   │  │ (AZ-b)   │  │ (AZ-c)   │  │                │  │ │
│ │ └──────────┘  └──────────┘  └──────────┘  └────────────────┘  │ │
│ │                                                                  │ │
│ │ ┌──────────────────────────────────────────────────────────┐   │ │
│ │ │ Application Load Balancer (ALB)                          │   │ │
│ │ │ HTTPS (443) → Target Groups → ECS/EKS/EC2              │   │ │
│ │ │ AWS WAF Attached | SSL/TLS Termination                  │   │ │
│ │ └──────────────────────────────────────────────────────────┘   │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Private Subnets (3 AZs)                                         │ │
│ │ 10.0.1.0/24 (us-east-1a)                                        │ │
│ │ 10.0.2.0/24 (us-east-1b)                                        │ │
│ │ 10.0.3.0/24 (us-east-1c)                                        │ │
│ │                                                                  │ │
│ │ ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │ │
│ │ │ EKS Workers │  │ ECS Tasks   │  │ EC2 Erlang  │             │ │
│ │ │ erlmcp pods │  │ erlmcp apps │  │ Nodes       │             │ │
│ │ └─────────────┘  └─────────────┘  └─────────────┘             │ │
│ │                                                                  │ │
│ │ Network Access: NAT Gateway → Internet                         │ │
│ │ AWS Services: VPC Endpoints (Private)                          │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Database Subnets (3 AZs)                                        │ │
│ │ 10.0.11.0/24 (us-east-1a)                                       │ │
│ │ 10.0.12.0/24 (us-east-1b)                                       │ │
│ │ 10.0.13.0/24 (us-east-1c)                                       │ │
│ │                                                                  │ │
│ │ ┌────────────┐  ┌────────────┐  ┌────────────┐                │ │
│ │ │ RDS Multi-AZ│  │ ElastiCache│  │ DocumentDB │                │ │
│ │ │ (Primary)  │  │ (Redis)    │  │ (MongoDB)  │                │ │
│ │ └────────────┘  └────────────┘  └────────────┘                │ │
│ │                                                                  │ │
│ │ Network Access: NO INTERNET ACCESS                             │ │
│ │ Security: Database Security Group (Port-specific)              │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ VPC Endpoints (Interface - PrivateLink)                         │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌──────────┐ │ │
│ │ │ ECR API │ │ ECR DKR │ │ EC2     │ │ ECS     │ │ Logs     │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └──────────┘ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌──────────┐ │ │
│ │ │ SSM     │ │ SSM Msg │ │ EC2 Msg │ │ KMS     │ │ Secrets  │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └──────────┘ │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ VPC Endpoints (Gateway - Free)                                  │ │
│ │ ┌────────────────────┐  ┌────────────────────┐                │ │
│ │ │ S3 Endpoint        │  │ DynamoDB Endpoint  │                │ │
│ │ │ (All Route Tables) │  │ (Private RT Only)  │                │ │
│ │ └────────────────────┘  └────────────────────┘                │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Security Groups (Layered Defense)                               │ │
│ │                                                                  │ │
│ │ ALB SG:           HTTP/HTTPS from 0.0.0.0/0                    │ │
│ │                   → Application SG                              │ │
│ │                                                                  │ │
│ │ Application SG:   8080 from ALB SG                             │ │
│ │                   4369, 9000-9999 from self (Erlang)           │ │
│ │                   8080, 8443, 9090 from VPC (MCP)              │ │
│ │                   → Database SG                                 │ │
│ │                                                                  │ │
│ │ Database SG:      5432 (PostgreSQL) from Application SG        │ │
│ │                   6379 (Redis) from Application SG             │ │
│ │                   NO internet access                            │ │
│ │                                                                  │ │
│ │ VPC Endpoints SG: 443 from VPC CIDR                            │ │
│ │                                                                  │ │
│ │ Management SG:    22 from specific IPs (prefer SSM)            │ │
│ └────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│ ┌────────────────────────────────────────────────────────────────┐ │
│ │ Network ACLs (Subnet-level firewall)                            │ │
│ │                                                                  │ │
│ │ Private Subnets:  Inbound: VPC CIDR (all), Ephemeral (1024+)  │ │
│ │                   Outbound: All                                 │ │
│ │                                                                  │ │
│ │ Database Subnets: Inbound: VPC CIDR only                       │ │
│ │                   Outbound: VPC CIDR only                       │ │
│ └────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ VPC Flow Logs → CloudWatch Logs → S3 (Archive) → Athena (Query)   │
│ ALL traffic | 60-second aggregation | 30-day retention             │
└────────────────────────────────────────────────────────────────────┘
```

## Security Architecture

### Defense in Depth Layers

1. **Edge Layer**
   - GCP: Cloud Armor (rate limiting, geo-blocking, OWASP rules)
   - AWS: AWS WAF (SQL injection, XSS protection)
   - DDoS protection (automatic mitigation)

2. **Network Layer**
   - VPC isolation with private subnets
   - Network ACLs (stateless firewall)
   - VPC Flow Logs (100% sampling)

3. **Instance Layer**
   - Security Groups (stateful firewall)
   - Hierarchical firewall policies (GCP)
   - Least-privilege rules

4. **Application Layer**
   - Mutual TLS for inter-service communication
   - Service mesh (future: Istio/Linkerd)
   - API gateway authentication

### Zero-Trust Implementation

```
┌─────────────────────────────────────────────────────────────────┐
│ Zero-Trust Network Model                                        │
│                                                                 │
│ 1. No Trust Zones: Every request authenticated/authorized     │
│ 2. Least Privilege: Minimal firewall rules, explicit allow    │
│ 3. Micro-segmentation: Service-to-service firewalls           │
│ 4. Continuous Verification: Network flow monitoring            │
│ 5. Assume Breach: Lateral movement prevention                  │
└─────────────────────────────────────────────────────────────────┘

Default Behavior:
┌──────────┐
│ Request  │
└────┬─────┘
     │
     ▼
┌─────────────────┐
│ Cloud Armor/WAF │  ← Rate limiting, geo-block
└────┬────────────┘
     │
     ▼
┌─────────────────┐
│ Network ACL     │  ← Subnet-level deny
└────┬────────────┘
     │
     ▼
┌─────────────────┐
│ Security Group  │  ← Instance-level deny
└────┬────────────┘
     │
     ▼
┌─────────────────┐
│ Application     │  ← Service-level auth
└─────────────────┘

ALL denies are logged for security analysis
```

## Erlang Distribution Protocol

Erlang nodes communicate using the Erlang Distribution Protocol:

```
┌─────────────────────────────────────────────────────────────┐
│ Erlang Cluster Communication                                │
│                                                             │
│ EPMD (Erlang Port Mapper Daemon) - Port 4369               │
│ ├─ Node discovery                                          │
│ ├─ Port registration                                       │
│ └─ Health checks                                           │
│                                                             │
│ Distributed Erlang - Ports 9000-9999                       │
│ ├─ Inter-node RPC                                          │
│ ├─ Process messaging                                       │
│ ├─ State replication                                       │
│ └─ Cluster formation                                       │
└─────────────────────────────────────────────────────────────┘

Firewall Rules:
┌────────────────────────────────────────────────────────────┐
│ Source: erlang-node-* tags (GCP) / self security group (AWS)│
│ Target: erlang-node-* tags (GCP) / self security group (AWS)│
│ Protocol: TCP                                                │
│ Ports: 4369, 9000-9999                                      │
│ Action: ALLOW                                                │
│ Priority: 1010                                               │
│ Logging: ENABLED (INCLUDE_ALL_METADATA)                     │
└────────────────────────────────────────────────────────────┘
```

## MCP Protocol Support

erlmcp v3 supports multiple transport protocols:

```
┌──────────────────────────────────────────────────────────────┐
│ MCP Transport Protocols                                      │
│                                                              │
│ ┌────────────┐  ┌────────────┐  ┌────────────┐            │
│ │ stdio      │  │ TCP        │  │ HTTP       │            │
│ │ (local)    │  │ Port 8080  │  │ Port 8080  │            │
│ └────────────┘  └────────────┘  └────────────┘            │
│                                                              │
│ ┌────────────┐  ┌────────────┐                            │
│ │ WebSocket  │  │ SSE        │                            │
│ │ Port 8443  │  │ Port 8080  │                            │
│ └────────────┘  └────────────┘                            │
│                                                              │
│ ┌────────────────────────────────────────────────────────┐ │
│ │ Metrics Endpoint: Port 9090 (Prometheus)               │ │
│ └────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘

Firewall Rules:
┌────────────────────────────────────────────────────────────┐
│ MCP API (8080/tcp):     Allow from VPC CIDR                 │
│ MCP WebSocket (8443/tcp): Allow from VPC CIDR               │
│ MCP Metrics (9090/tcp):   Allow from monitoring subnet      │
└────────────────────────────────────────────────────────────┘
```

## High Availability Design

### Multi-AZ Deployment

```
┌─────────────────────────────────────────────────────────────┐
│ High Availability Configuration                             │
│                                                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Availability Zone 1 (us-east-1a / us-central1-a)       │ │
│ │ ├─ Public Subnet (NAT Gateway)                         │ │
│ │ ├─ Private Subnet (Erlang nodes, EKS/GKE)             │ │
│ │ └─ Database Subnet (RDS/Cloud SQL primary)            │ │
│ └─────────────────────────────────────────────────────────┘ │
│                                                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Availability Zone 2 (us-east-1b / us-central1-b)       │ │
│ │ ├─ Public Subnet (NAT Gateway)                         │ │
│ │ ├─ Private Subnet (Erlang nodes, EKS/GKE)             │ │
│ │ └─ Database Subnet (RDS/Cloud SQL replica)            │ │
│ └─────────────────────────────────────────────────────────┘ │
│                                                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Availability Zone 3 (us-east-1c / us-central1-c)       │ │
│ │ ├─ Public Subnet (NAT Gateway)                         │ │
│ │ ├─ Private Subnet (Erlang nodes, EKS/GKE)             │ │
│ │ └─ Database Subnet (RDS/Cloud SQL replica)            │ │
│ └─────────────────────────────────────────────────────────┘ │
│                                                             │
│ Load Balancing:                                            │
│ ├─ ALB/Cloud Load Balancer distributes traffic            │
│ ├─ Health checks every 30 seconds                         │
│ ├─ Automatic failover < 30 seconds                        │
│ └─ Cross-AZ traffic minimized                             │
└─────────────────────────────────────────────────────────────┘
```

### Disaster Recovery

```
┌──────────────────────────────────────────────────────────────┐
│ Disaster Recovery Strategy                                   │
│                                                              │
│ RTO (Recovery Time Objective): 15 minutes                   │
│ RPO (Recovery Point Objective): 5 minutes                   │
│                                                              │
│ ┌────────────────────────────────────────────────────────┐  │
│ │ Automated Backups                                      │  │
│ │ ├─ VPC configuration (Terraform state)                │  │
│ │ ├─ Database snapshots (every 5 minutes)               │  │
│ │ ├─ Application state (continuous replication)         │  │
│ │ └─ Secrets (versioned in Secret Manager)              │  │
│ └────────────────────────────────────────────────────────┘  │
│                                                              │
│ ┌────────────────────────────────────────────────────────┐  │
│ │ Failover Process                                       │  │
│ │ 1. Health check failure detected (30 seconds)         │  │
│ │ 2. Load balancer removes unhealthy targets            │  │
│ │ 3. Auto-scaling launches replacement instances        │  │
│ │ 4. New instances join Erlang cluster                  │  │
│ │ 5. Traffic resumes (total: < 15 minutes)              │  │
│ └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

## Observability Stack

### VPC Flow Logs Analysis

```
┌──────────────────────────────────────────────────────────────┐
│ Flow Logs Pipeline                                           │
│                                                              │
│ VPC Flow Logs                                                │
│      ↓                                                       │
│ Cloud Logging / CloudWatch Logs                             │
│      ↓                                                       │
│ Export (hourly)                                              │
│      ↓                                                       │
│ BigQuery / Athena                                            │
│      ↓                                                       │
│ Analysis & Dashboards                                        │
│   ├─ Top talkers (source/destination IPs)                   │
│   ├─ Denied connections (security incidents)                │
│   ├─ Bandwidth usage by subnet                              │
│   ├─ Anomaly detection (ML-based)                           │
│   └─ Compliance reports (audit trail)                       │
└──────────────────────────────────────────────────────────────┘

Sample Queries:
┌────────────────────────────────────────────────────────────┐
│ -- Top denied connections (security monitoring)            │
│ SELECT srcaddr, dstaddr, dstport, COUNT(*) as count        │
│ FROM flow_logs                                             │
│ WHERE action = 'REJECT'                                    │
│ GROUP BY srcaddr, dstaddr, dstport                         │
│ ORDER BY count DESC                                        │
│ LIMIT 100                                                  │
│                                                            │
│ -- Bandwidth by subnet (cost analysis)                     │
│ SELECT subnet_id, SUM(bytes) as total_bytes               │
│ FROM flow_logs                                             │
│ WHERE timestamp > CURRENT_TIMESTAMP - INTERVAL '1 day'    │
│ GROUP BY subnet_id                                         │
│ ORDER BY total_bytes DESC                                  │
└────────────────────────────────────────────────────────────┘
```

## Cost Optimization

### Monthly Network Costs Breakdown

#### GCP Estimated Monthly Costs
```
Resource                   Quantity  Unit Cost      Monthly
─────────────────────────────────────────────────────────────
Cloud NAT                  1         $0.044/hour    $32
NAT Data Processing        1 TB      $0.045/GB      $45
VPC Flow Logs              500 GB    $0.50/GB       $250
Cloud Armor Policy         1         $5/policy      $5
PSC Endpoints              5         $0.01/hour     $36
Private Google Access      -         Free           $0
VPC Peering Data (same)    1 TB      $0.01/GB       $10
─────────────────────────────────────────────────────────────
Total GCP                                           $378/month
```

#### AWS Estimated Monthly Costs
```
Resource                   Quantity  Unit Cost      Monthly
─────────────────────────────────────────────────────────────
NAT Gateway                3         $0.045/hour    $97
NAT Data Processing        1 TB      $0.045/GB      $45
VPC Flow Logs (CW)         500 GB    $0.50/GB       $250
Interface Endpoints        13        $0.01/hour     $94
Gateway Endpoints (S3)     1         Free           $0
AWS WAF                    1         $5/policy      $5
Data Transfer (inter-AZ)   100 GB    $0.01/GB       $1
─────────────────────────────────────────────────────────────
Total AWS                                           $492/month
```

#### Cost Optimization Strategies
```
┌────────────────────────────────────────────────────────────┐
│ Low-cost configuration (dev/staging):                      │
│ ├─ Single NAT Gateway per VPC: Save $64/month (AWS)      │
│ ├─ Reduced Flow Log sampling (10%): Save $225/month      │
│ ├─ Fewer VPC endpoints (essential only): Save $60/month  │
│ └─ Total savings: ~$350/month (40% reduction)            │
│                                                            │
│ Production configuration (as designed):                    │
│ ├─ Multi-AZ NAT Gateways: High availability              │
│ ├─ 100% Flow Log sampling: Compliance & security         │
│ ├─ Full VPC endpoint coverage: Performance & security    │
│ └─ Total cost: $378 (GCP) / $492 (AWS) per month         │
└────────────────────────────────────────────────────────────┘
```

## Deployment Instructions

### GCP VPC Module Usage

```hcl
module "vpc" {
  source = "./marketplace/gcp/terraform/modules/vpc"

  project_id   = "erlmcp-prod"
  network_name = "erlmcp-vpc"

  # Network configuration
  routing_mode = "REGIONAL"
  mtu          = 1460

  # Subnets
  subnets = [
    {
      name                     = "erlmcp-subnet-us-central1"
      region                   = "us-central1"
      ip_cidr_range            = "10.0.0.0/24"
      private_ip_google_access = true
      purpose                  = null
      role                     = null
      secondary_ip_ranges = [
        {
          range_name    = "gke-pods"
          ip_cidr_range = "10.1.0.0/16"
        },
        {
          range_name    = "gke-services"
          ip_cidr_range = "10.2.0.0/16"
        }
      ]
    }
  ]

  # Enable features
  enable_private_service_access = true
  enable_dns_policies           = true
  enable_cloud_armor            = true
  enable_erlang_distribution    = true

  # Cloud Armor configuration
  cloud_armor_rate_limit_threshold = 1000
  cloud_armor_enable_adaptive_protection = true

  # Flow logs
  enable_flow_log_export = true
  flow_log_bucket_name   = "erlmcp-flow-logs"
}
```

### AWS Network Module Usage

```hcl
module "network" {
  source = "./docs/network/NETWORK_SCRIPTS/terraform"

  region      = "us-east-1"
  environment = "production"

  # VPC configuration
  vpc_cidr             = "10.0.0.0/16"
  private_subnet_cidrs = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnet_cidrs  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
  database_subnet_cidrs = ["10.0.11.0/24", "10.0.12.0/24", "10.0.13.0/24"]

  # High availability
  enable_nat_gateway     = true
  one_nat_gateway_per_az = true

  # Security
  enable_flow_logs          = true
  enable_interface_endpoints = true
  create_network_acls       = true

  # Security groups
  create_alb_sg  = true
  create_app_sg  = true
  create_db_sg   = true
  create_mgmt_sg = true

  ssh_allowed_cidrs = [] # Use SSM Session Manager
}
```

## Validation & Testing

### Network Connectivity Tests

```bash
# Test NAT Gateway connectivity
docker compose run erlmcp-build curl -I https://www.google.com

# Test VPC endpoint connectivity (AWS)
docker compose run erlmcp-build aws s3 ls --region us-east-1

# Test Erlang node connectivity
docker compose run erlmcp-node-1 erl -eval 'net_adm:ping("erlmcp@node2").'

# Test MCP protocol
docker compose run erlmcp-build curl http://erlmcp-api:8080/health
```

### Security Validation

```bash
# Verify no public IPs on private instances
docker compose run erlmcp-build aws ec2 describe-instances \
  --filters "Name=subnet-id,Values=subnet-private*" \
  --query 'Reservations[].Instances[].PublicIpAddress'

# Verify VPC Flow Logs enabled
docker compose run erlmcp-build aws ec2 describe-flow-logs \
  --filter "Name=resource-type,Values=VPC"

# Verify firewall rules (GCP)
docker compose run erlmcp-build gcloud compute firewall-rules list \
  --filter="network:erlmcp-vpc"
```

## References

- [ADR-001: VPC Network Architecture](/home/user/erlmcp/docs/network/ADR-001-VPC-NETWORK-ARCHITECTURE.md)
- [GCP VPC Best Practices](https://cloud.google.com/vpc/docs/best-practices)
- [AWS VPC Security Best Practices](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-security-best-practices.html)
- [NIST Zero Trust Architecture](https://csrc.nist.gov/publications/detail/sp/800-207/final)
- [Erlang Distribution Protocol](https://www.erlang.org/doc/apps/erts/erl_dist_protocol.html)

## Maintenance

### Regular Tasks
- Weekly: Review VPC Flow Logs for anomalies
- Monthly: Cost optimization review
- Quarterly: Security audit and firewall rule cleanup
- Annually: Architecture review and capacity planning

### Emergency Contacts
- Network Team: network-team@erlmcp.io
- Security Team: security@erlmcp.io
- On-call: oncall@erlmcp.io
