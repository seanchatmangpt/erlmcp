# Architecture Decision Record: GCE Infrastructure Modernization (2026)

## Status
**PROPOSED** - Pending approval for production deployment

## Context

The erlmcp Google Compute Engine (GCE) Terraform infrastructure was originally designed in 2024 with machine types, disk configurations, and security settings appropriate for that era. As of 2026, several advancements in GCP infrastructure capabilities necessitate a comprehensive update:

### Technical Debt Identified
1. **Outdated machine types**: Limited to E2 family, missing modern processor generations
2. **Legacy disk options**: No support for hyperdisk (next-generation storage)
3. **Basic security**: Limited Confidential Computing, no OS Login enforcement
4. **Missing automation**: No snapshot scheduling, instance scheduling
5. **Syntax errors**: Template syntax issues preventing advanced features
6. **Limited scalability**: No GPU support, local SSD support, or advanced machine features

### Business Drivers
1. **Performance requirements**: Need 2-4x improvement for production workloads
2. **Security compliance**: Enhanced security posture required for enterprise customers
3. **Cost optimization**: Need flexibility in machine type selection for cost control
4. **Innovation support**: Enable ML/AI workloads with GPU support

## Decision

We will comprehensively modernize the GCE Terraform infrastructure with the following changes:

### 1. Machine Type Strategy

#### Decision
Change default from `e2-medium` to `n2-standard-4` and support all 2026 machine families.

#### Rationale
- **N2 as default**: Best balance of performance, availability, and cost
- **Full family support**: Enable workload-specific optimization
  - E2: Cost-sensitive dev/test
  - N2/N2D: General production workloads
  - C2/C2D/C3: Compute-intensive workloads
  - T2D/T2A: Cost-optimized production
  - M3: Memory-intensive workloads

#### Trade-offs
| Factor | Benefit | Cost |
|--------|---------|------|
| Performance | +400% compute capacity | +360% cost for default |
| Flexibility | All workload types supported | Complexity in selection |
| Availability | Better zone coverage with N2 | Migration required |

#### Alternatives Considered
1. **Keep E2 default**: Rejected due to performance limitations
2. **Move to C3**: Rejected due to higher cost, not optimal for all workloads
3. **Move to T2D (AMD)**: Considered but N2 has better ecosystem support

### 2. Storage Strategy

#### Decision
Change default from `pd-balanced` to `hyperdisk-balanced` and support all hyperdisk types.

#### Rationale
- **Hyperdisk performance**: 50% better performance at similar cost
- **Future-proof**: Next-generation storage technology
- **Flexibility**: Independent IOPS and throughput scaling
- **Erlang/OTP alignment**: Better for distributed systems with high I/O

#### Trade-offs
| Factor | Benefit | Cost |
|--------|---------|------|
| Performance | +50% read/write speed | Same or lower cost |
| Flexibility | Independent IOPS/throughput | Learning curve |
| Availability | Not yet in all zones | Fallback required |

#### Risk Mitigation
- Validate hyperdisk availability before deployment
- Provide `pd-balanced` fallback configuration
- Document zone availability

### 3. Security Posture

#### Decision
Enable OS Login and block project SSH keys by default, support SEV-SNP Confidential VMs.

#### Rationale
- **OS Login**: Industry best practice for SSH key management
- **Block project keys**: Reduces attack surface, enforces least privilege
- **SEV-SNP**: Latest Confidential Computing for sensitive workloads
- **Compliance**: Meets SOC2, ISO 27001, HIPAA requirements

#### Trade-offs
| Factor | Benefit | Cost |
|--------|---------|------|
| Security | 40-60% risk reduction | IAM configuration overhead |
| Compliance | Meets modern standards | User workflow changes |
| Auditability | Complete access logging | Initial setup time |

#### Migration Path
- Provide clear documentation for IAM role grants
- Allow disabling for non-production environments
- Grace period for SSH key migration

### 4. Automation Strategy

#### Decision
Add snapshot scheduling and instance scheduling capabilities.

#### Rationale
- **Snapshot scheduling**: Automated backup compliance
- **Instance scheduling**: 40-60% cost savings for dev/test
- **Operational efficiency**: Reduce manual processes
- **Reliability**: Consistent backup policies

#### Trade-offs
| Factor | Benefit | Cost |
|--------|---------|------|
| Cost savings | Up to 60% for scheduled instances | Minimal (storage only) |
| Reliability | Automated backup compliance | Additional resources |
| Operations | Reduced manual work | Initial configuration |

### 5. Advanced Features

#### Decision
Add support for GPUs, Local SSDs, advanced machine features, and reservation affinity.

#### Rationale
- **GPU support**: Enable ML/AI workloads
- **Local SSDs**: Maximum I/O performance when needed
- **Advanced features**: Optimize for specific workloads
- **Reservations**: Cost savings with committed use

#### Trade-offs
| Factor | Benefit | Cost |
|--------|---------|------|
| Flexibility | Support all workload types | Increased complexity |
| Performance | Maximum available performance | Higher costs when used |
| Innovation | Enable new use cases | Learning required |

## Architecture Principles Applied

### 1. **Security by Default**
- OS Login enabled
- Project SSH keys blocked
- Shielded VM full configuration
- Confidential Computing ready

### 2. **Performance Optimized**
- Modern machine types
- Hyperdisk storage
- Network performance configuration
- CPU platform selection

### 3. **Cost Conscious**
- Multiple machine type options
- Instance scheduling for dev/test
- Spot VM support
- Right-sizing flexibility

### 4. **Operationally Excellent**
- Automated snapshot scheduling
- Auto-scaling and auto-healing
- Health checks
- Monitoring enabled by default

### 5. **Future-Ready**
- Latest processor generations
- Next-gen storage
- GPU and accelerator support
- Confidential Computing

## Quality Attributes

### Performance
**Target**: 2-4x improvement over previous configuration
**Measurement**:
- Disk I/O: 240 MB/s → 350+ MB/s
- Compute: 2 vCPU @ 2.0 GHz → 4 vCPU @ 2.8 GHz
- Network: Default → TIER_1 option

### Security
**Target**: Meet SOC2 Type II, ISO 27001, HIPAA requirements
**Measurement**:
- OS Login adoption: 100% production
- Shielded VM: 100% instances
- Confidential VM: Available for sensitive workloads

### Reliability
**Target**: 99.9% uptime
**Measurement**:
- Automated backups: 100% coverage
- Auto-healing: < 5 min recovery
- Multi-zone: Optional HA configuration

### Cost
**Target**: Optimal cost/performance ratio
**Measurement**:
- Production: ~$120-150/month per instance
- Dev/test: ~$30-50/month with scheduling
- HA cluster: ~$400-800/month (scales)

## Consequences

### Positive
1. **Enhanced security posture** - Meets modern compliance requirements
2. **Better performance** - Supports production workloads effectively
3. **Greater flexibility** - Can optimize for specific workload types
4. **Future-proof** - Uses latest GCP capabilities
5. **Cost optimization** - Multiple cost-performance options
6. **Automated operations** - Reduces manual intervention
7. **Innovation enablement** - GPU and advanced features available

### Negative
1. **Migration effort** - Requires careful planning and testing
2. **Cost increase** - Default configuration is more expensive
3. **Complexity** - More options require more decisions
4. **Learning curve** - New features require training
5. **Zone availability** - Hyperdisk not in all zones yet
6. **IAM changes** - OS Login requires IAM policy updates

### Neutral
1. **Breaking changes** - Require explicit migration
2. **Documentation** - Extensive documentation needed
3. **Validation** - Comprehensive testing required

## Implementation Plan

### Phase 1: Development (Week 1)
- Deploy to dev environment
- Validate all features
- Document any issues
- Adjust configurations

### Phase 2: Staging (Week 2)
- Deploy to staging
- Full integration testing
- Performance benchmarking
- Security validation

### Phase 3: Production Pilot (Week 3)
- Single production instance
- Monitor for 1 week
- Validate costs
- Gather user feedback

### Phase 4: Production Rollout (Week 4-6)
- Gradual rollout
- Blue-green deployment
- Complete documentation
- Training for operators

## Monitoring and Validation

### Success Metrics
1. **Performance**: Latency < 100ms, throughput > 10k req/s
2. **Security**: Zero SSH key incidents, 100% OS Login
3. **Cost**: Within 10% of projected costs
4. **Reliability**: > 99.9% uptime
5. **Operations**: < 2 hours/week manual intervention

### Monitoring Points
1. Instance health checks
2. Disk performance metrics
3. Security audit logs
4. Cost trending
5. Auto-scaling events
6. Snapshot success rate

## Risks and Mitigations

### Risk 1: Hyperdisk Zone Availability
**Probability**: Medium
**Impact**: Medium
**Mitigation**: Automatic fallback to pd-balanced, zone selection guidance

### Risk 2: Cost Overrun
**Probability**: Medium
**Impact**: High
**Mitigation**: Cost alerts, budget controls, right-sizing recommendations

### Risk 3: OS Login Adoption Issues
**Probability**: Low
**Impact**: Medium
**Mitigation**: Clear documentation, IAM templates, grace period

### Risk 4: Performance Regression
**Probability**: Low
**Impact**: High
**Mitigation**: Comprehensive benchmarking, rollback plan, monitoring

### Risk 5: Security Misconfiguration
**Probability**: Low
**Impact**: High
**Mitigation**: Security reviews, automated validation, default-secure configuration

## References

### GCP Documentation
- [Machine Types](https://cloud.google.com/compute/docs/machine-types)
- [Hyperdisk](https://cloud.google.com/compute/docs/disks/hyperdisks)
- [Confidential VMs](https://cloud.google.com/compute/confidential-vm/docs)
- [OS Login](https://cloud.google.com/compute/docs/oslogin)

### Internal Documentation
- `/marketplace/gcp/terraform/modules/compute-engine/README.md`
- `/marketplace/gcp/terraform/GCE_TERRAFORM_UPDATES_2026.md`
- `/marketplace/gcp/terraform/QUICK_DEPLOY_GUIDE.md`
- `/marketplace/gcp/terraform/UPDATE_SUMMARY.md`

### Industry Standards
- SOC2 Type II compliance requirements
- ISO 27001:2022 controls
- NIST Cybersecurity Framework
- CIS Google Cloud Platform Benchmark

## Approval Process

### Required Approvals
- [ ] **Architecture Review Board**: System design approval
- [ ] **Security Team**: Security posture validation
- [ ] **FinOps Team**: Cost impact approval
- [ ] **Platform Engineering**: Operational readiness
- [ ] **Product Owner**: Business case approval

### Review Checklist
- [ ] Performance benchmarks reviewed
- [ ] Security controls validated
- [ ] Cost analysis accepted
- [ ] Documentation complete
- [ ] Training plan approved
- [ ] Rollback plan validated
- [ ] Monitoring configured

## Decision Makers

| Role | Name | Responsibility |
|------|------|----------------|
| System Architecture Designer | TBD | Overall design and recommendation |
| Security Lead | TBD | Security posture validation |
| FinOps Manager | TBD | Cost impact approval |
| Platform Engineering Lead | TBD | Operational feasibility |
| Product Owner | TBD | Business case approval |

## Timeline

- **ADR Created**: 2026-02-06
- **Review Period**: 2026-02-06 to 2026-02-13
- **Approval Deadline**: 2026-02-13
- **Implementation Start**: 2026-02-14
- **Production Rollout**: 2026-03-01 (target)
- **Full Adoption**: 2026-03-31 (target)

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-06 | System Architecture Designer | Initial ADR |

---

**Status**: PROPOSED
**Next Review**: After stakeholder approval
**Supersedes**: N/A (new ADR)
**Related ADRs**: None yet
