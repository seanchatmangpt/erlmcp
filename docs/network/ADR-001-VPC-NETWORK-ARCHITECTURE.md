# ADR-001: VPC Network Architecture for erlmcp v3

**Status:** Accepted
**Date:** 2026-02-06
**Decision Makers:** System Architecture Team
**Context:** Enterprise-grade networking for Erlang/OTP MCP SDK deployment

## Context and Problem Statement

erlmcp v3 requires a robust, scalable, and secure network architecture to support:
- Distributed Erlang/OTP clusters across multiple regions
- Multi-cloud deployment (GCP + AWS)
- Zero-trust security model with least-privilege access
- High availability with 99.99% uptime SLA
- Compliance with SOC 2, HIPAA, and GDPR requirements
- Real-time message passing between distributed nodes
- Observable and auditable network traffic

## Decision Drivers

1. **Security First**: Zero-trust networking, encrypted communication, minimal attack surface
2. **Cluster Safety**: Network partitions expected, overlay DNS unreliable
3. **Performance**: Low latency for distributed Erlang communication (<5ms intra-region)
4. **Compliance**: Audit logs, encryption in transit, network segmentation
5. **Cost Optimization**: Efficient NAT gateway usage, reserved IPs where applicable
6. **Operational Excellence**: Deterministic deployments, observable traffic flows
7. **Multi-tenant Isolation**: Network-level isolation for different tenants

## Considered Options

### Option 1: Flat Network with Security Groups (REJECTED)
- Single VPC with all resources
- Security via security groups only
- **Rejected**: Insufficient isolation, blast radius too large

### Option 2: Hub-and-Spoke with Transit Gateway (CONSIDERED)
- Central hub VPC with transit gateway
- Spoke VPCs for different environments
- **Pros**: Good isolation, centralized egress
- **Cons**: Additional cost, complexity for initial deployment

### Option 3: Segmented VPC with Private Service Access (SELECTED)
- Multiple subnets with different purposes
- Private Google Access / VPC Endpoints for cloud services
- VPC peering for cross-VPC communication
- **Selected**: Balance of security, cost, and operational simplicity

## Decision Outcome

**Chosen Option: Segmented VPC with Private Service Access**

### Network Architecture

#### GCP Architecture
```
erlmcp-vpc (10.0.0.0/16)
├── Control Plane Subnet (10.0.0.0/24)
│   ├── GKE Masters
│   ├── Private Google Access: ENABLED
│   └── Secondary Ranges:
│       ├── gke-pods (10.1.0.0/16)
│       └── gke-services (10.2.0.0/16)
├── Data Plane Subnet (10.0.1.0/24)
│   ├── Erlang nodes (worker VMs)
│   ├── Private Google Access: ENABLED
│   └── Cloud NAT for egress
├── Management Subnet (10.0.10.0/24)
│   ├── Bastion hosts (IAP tunnel preferred)
│   └── Observability infrastructure
└── Private Service Connect
    ├── Cloud SQL
    ├── Memorystore (Redis)
    └── Secret Manager
```

#### AWS Architecture
```
erlmcp-vpc (10.0.0.0/16)
├── Public Subnets (10.0.101-103.0/24)
│   ├── NAT Gateways (one per AZ for HA)
│   ├── Application Load Balancers
│   └── Internet Gateway attached
├── Private Subnets (10.0.1-3.0/24)
│   ├── EKS Worker Nodes
│   ├── EC2 Erlang Nodes
│   └── VPC Endpoints for AWS services
├── Database Subnets (10.0.11-13.0/24)
│   ├── RDS instances
│   ├── ElastiCache
│   └── No internet access
└── Management Subnets (10.0.21-23.0/24)
    ├── Systems Manager Session Manager
    ├── CloudWatch endpoints
    └── Monitoring infrastructure
```

### Key Design Decisions

#### 1. Zero-Trust Network Security
**Decision**: Implement defense-in-depth with multiple security layers

**Implementation**:
- Default-deny firewall rules (explicit allow only)
- Network segmentation by tier (control, data, management)
- Private endpoints for cloud services (no internet traversal)
- VPC Flow Logs for all traffic (100% sampling for compliance)
- Cloud Armor / AWS WAF for DDoS protection
- Identity-Aware Proxy (GCP) / Session Manager (AWS) instead of SSH bastions

**Rationale**: Per CLAUDE.md constitution - "zero-trust, least privilege, auditability mandatory"

#### 2. Private Google Access / VPC Endpoints
**Decision**: Use private connectivity for all cloud service access

**GCP Implementation**:
- Private Google Access enabled on all subnets
- Private Service Connect for managed services
- Cloud NAT for internet egress (auditability)
- DNS policies for *.googleapis.com resolution

**AWS Implementation**:
- VPC Endpoints for S3, DynamoDB, ECR, CloudWatch, SSM, KMS, Secrets Manager
- PrivateLink for custom services
- Interface endpoints in all private subnets
- Gateway endpoints for S3 and DynamoDB

**Rationale**: Eliminates internet exposure, reduces NAT costs, improved security

#### 3. High Availability NAT Strategy
**Decision**:
- GCP: Regional Cloud NAT (automatic HA)
- AWS: NAT Gateway per AZ (explicit HA)

**Trade-offs**:
- **Cost**: AWS costs higher ($0.045/hour per NAT GW × 3 AZs = ~$97/month)
- **Reliability**: Eliminates single point of failure
- **Performance**: Local AZ egress, no cross-AZ charges

**Rationale**: Production deployment requires HA, cost justified by uptime gains

#### 4. Network Observability
**Decision**: Comprehensive flow logging and metrics

**Implementation**:
- VPC Flow Logs: 100% sampling (compliance requirement)
- Aggregation interval: 5 seconds (real-time detection)
- Metadata: INCLUDE_ALL_METADATA
- Export to Cloud Logging / CloudWatch Logs
- Structured logs for automated analysis
- Real-time dashboards in monitoring system

**Cost Impact**: ~$0.50 per GB ingested (estimated $200-500/month)

**Rationale**: "Observability: signals > logs, metrics, traces. Operator-first."

#### 5. Erlang Cluster Communication
**Decision**: Dedicated subnet ranges for Erlang distribution protocol

**Requirements**:
- Port 4369 (EPMD) between nodes
- Dynamic port range 9000-9999 for distributed Erlang
- Multicast support for discovery (optional)
- Low latency requirements (<5ms p99)

**Firewall Rules**:
```hcl
# Erlang inter-node communication
source: erlang-node-subnet
ports: 4369, 9000-9999
protocol: TCP
```

**Rationale**: Support "Cluster-churn safe. No stable node assumptions."

#### 6. Subnet Sizing Strategy
**Decision**: Conservative IP allocation with room for growth

**Rationale**:
- Control Plane (/24): 251 usable IPs
  - Expected: 20-30 GKE/EKS control plane IPs
  - Growth: 10x headroom
- Data Plane (/24): 251 usable IPs
  - Expected: 50-100 Erlang nodes
  - Pods: /16 secondary range (65k pods)
  - Services: /16 secondary range (65k services)
- Database (/24): 251 usable IPs
  - Expected: 5-10 database instances per AZ
- Management (/24): 251 usable IPs
  - Observability, bastions, CI/CD runners

**Total Allocation**: 16 /24 subnets from /16 VPC CIDR (only 4 initially used)

#### 7. VPC Peering vs Transit Gateway
**Decision**: VPC Peering for MVP, Transit Gateway for scale

**Current**: VPC Peering
- Simplicity: Direct VPC-to-VPC connectivity
- Cost: $0.01 per GB (same region), $0.02 per GB (cross-region)
- Limitation: Non-transitive routing

**Future**: AWS Transit Gateway / GCP Network Connectivity Center
- At scale (>10 VPCs)
- Centralized routing and security
- Transitive routing support

#### 8. MTU Configuration
**Decision**: 1460 bytes for GCP, 1500 bytes for AWS

**GCP**: 1460 bytes (Google Cloud default, accounts for GRE tunneling overhead)
**AWS**: 1500 bytes (standard Ethernet MTU)

**Rationale**: Follow cloud provider recommendations, avoid fragmentation

## Security Considerations

### Threat Model
1. **External Attack**: Mitigated by Private endpoints, no public IPs, WAF/Cloud Armor
2. **Lateral Movement**: Mitigated by network segmentation, micro-segmentation firewall rules
3. **Data Exfiltration**: Mitigated by NAT logging, VPC Flow Logs, DLP policies
4. **DDoS**: Mitigated by Cloud Armor rate limiting, AWS Shield Standard
5. **Insider Threat**: Mitigated by audit logging, least privilege IAM

### Compliance Mapping
- **SOC 2 CC6.1**: Network segmentation, encrypted communication
- **SOC 2 CC6.6**: Logical access controls via firewall rules
- **SOC 2 CC7.2**: System monitoring via VPC Flow Logs
- **HIPAA §164.312(e)(1)**: Transmission security via private endpoints
- **GDPR Article 32**: Network security and encryption

## Performance Characteristics

### Latency Targets
- Intra-region VPC: <1ms p50, <5ms p99
- Cross-region VPC peering: <50ms p50 (US East-West), <100ms p99
- Cloud service access: <10ms p99 (via private endpoints)
- NAT Gateway: <1ms added latency

### Throughput
- NAT Gateway: Up to 45 Gbps (AWS), 100 Gbps (GCP)
- VPC Peering: Up to 100 Gbps
- VPC Endpoints: Up to 100 Gbps

### Scalability Limits
- GCP VPC: 15,000 instances per VPC, 100 VPC peering connections
- AWS VPC: 500 ENIs per subnet (adjustable), 125 VPC peering connections

## Cost Analysis

### Monthly Network Costs (Estimated)

#### GCP
| Resource | Quantity | Unit Cost | Monthly Cost |
|----------|----------|-----------|--------------|
| Cloud NAT | 1 | $0.044/hour | $32 |
| NAT Data Processing | 1 TB | $0.045/GB | $45 |
| VPC Flow Logs | 500 GB | $0.50/GB | $250 |
| Cloud Armor | 1 policy | $5/policy | $5 |
| Private Service Connect | 5 endpoints | $0.01/hour | $36 |
| **Total GCP** | | | **$368/month** |

#### AWS
| Resource | Quantity | Unit Cost | Monthly Cost |
|----------|----------|-----------|--------------|
| NAT Gateway | 3 | $0.045/hour | $97 |
| NAT Data Processing | 1 TB | $0.045/GB | $45 |
| VPC Flow Logs | 500 GB | $0.50/GB | $250 |
| VPC Endpoints | 10 | $0.01/hour | $73 |
| **Total AWS** | | | **$465/month** |

**Combined Monthly**: ~$833/month baseline networking cost

**Optimization Opportunities**:
- Single NAT Gateway (AWS): Save $64/month (reduces HA)
- Reduced Flow Log sampling: Save $125/month (reduces observability)
- S3 Gateway Endpoint: Free (no change needed)

**Decision**: Accept full cost for production-grade reliability and observability

## Operational Considerations

### Deployment
- All changes via Terraform (deterministic)
- State stored in encrypted GCS/S3 backend
- Terraform Cloud/GitLab CI for execution
- Approval required for production changes

### Monitoring
- VPC Flow Log dashboards (Grafana/Cloud Monitoring)
- NAT Gateway metrics (CloudWatch/Cloud Monitoring)
- Firewall hit counts and denies
- Network throughput and latency metrics

### Backup and Recovery
- Terraform state versioning
- VPC configuration exported daily
- Disaster recovery plan: Rebuild from Terraform in 30 minutes

### Change Management
- All firewall rule changes require security review
- Network changes tested in staging first
- Rollback plan required for production changes

## Consequences

### Positive
- Enterprise-grade security posture
- Full audit trail of network traffic
- Compliance-ready architecture
- High availability and fault tolerance
- Scalable to 1000+ nodes
- Observable and debuggable

### Negative
- Higher operational cost (~$850/month for networking alone)
- Increased complexity vs flat network
- Requires network expertise to manage
- VPC Flow Logs generate significant data volume

### Neutral
- Requires Terraform expertise
- Multi-cloud requires managing two network architectures
- Ongoing cost optimization needed

## Follow-up Decisions Required

1. **ADR-002**: Service Mesh Architecture (Istio/Linkerd vs Native Cloud Service Mesh)
2. **ADR-003**: DNS Strategy (Cloud DNS / Route53 configuration)
3. **ADR-004**: Certificate Management (Let's Encrypt vs ACM/Google Managed Certs)
4. **ADR-005**: DDoS Protection Level (Standard vs Advanced Shield/Cloud Armor)
5. **ADR-006**: VPN/Hybrid Connectivity (Cloud VPN/VPN Gateway for on-premises)

## References

- [GCP VPC Best Practices](https://cloud.google.com/vpc/docs/best-practices)
- [AWS VPC Security Best Practices](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-security-best-practices.html)
- [NIST Zero Trust Architecture](https://csrc.nist.gov/publications/detail/sp/800-207/final)
- erlmcp v3 CLAUDE.md constitution
- [Google Cloud Architecture Framework: Security](https://cloud.google.com/architecture/framework/security)
- [AWS Well-Architected Framework: Security Pillar](https://docs.aws.amazon.com/wellarchitected/latest/security-pillar/welcome.html)

## Approval

- [ ] Security Team Review
- [ ] Compliance Team Review
- [ ] Cloud Architecture Review
- [ ] Cost Analysis Approval
- [ ] Engineering Leadership Sign-off

---

**Next Steps**: Implement Terraform modules per this architecture, validate in staging environment, schedule production rollout.
