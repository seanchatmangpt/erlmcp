# erlmcp v3 Runbooks Index

**Version**: 3.0.0
**Last Updated**: 2026-02-02

## Overview

This directory contains comprehensive incident response and disaster recovery runbooks for erlmcp v3. Each runbook provides step-by-step procedures for diagnosing, resolving, and preventing specific failure scenarios.

## Runbook Categories

### Critical Incidents (SEV-1)

| Runbook | Title | Response Time |
|---------|-------|---------------|
| [RB-007](INCIDENT_RESPONSE.md#runbook-rb-007---full-cluster-outage) | Full Cluster Outage | < 15 min |
| [RB-009](DATABASE_FAILOVER.md) | Database Failover | < 15 min |
| [RB-010](NETWORK_PARTITION.md) | Network Partition Recovery | < 15 min |
| [RB-011](CASCADING_FAILURES.md) | Cascading Failures Recovery | < 15 min |
| [RB-012](SESSION_FAILOVER.md) | Session Failover and State Recovery | < 15 min |

### High Severity (SEV-2)

| Runbook | Title | Response Time |
|---------|-------|---------------|
| [RB-002](INCIDENT_RESPONSE.md#runbook-rb-002---high-memory-usage) | High Memory Usage | < 1 hour |
| [RB-003](INCIDENT_RESPONSE.md#runbook-rb-003---high-cpu-usage) | High CPU Usage | < 1 hour |
| [RB-005](INCIDENT_RESPONSE.md#runbook-rb-005---database-connection-issues) | Database Connection Issues | < 1 hour |
| [RB-006](INCIDENT_RESPONSE.md#runbook-rb-006---deployment-rollback) | Deployment Rollback | < 1 hour |

### Medium Severity (SEV-3)

| Runbook | Title | Response Time |
|---------|-------|---------------|
| [RB-001](INCIDENT_RESPONSE.md#runbook-rb-001---pod-crashloopbackoff) | Pod CrashLoopBackOff | < 4 hours |
| [RB-004](INCIDENT_RESPONSE.md#runbook-rb-004---network-partition) | Network Partition (Basic) | < 4 hours |
| [RB-008](RB-008-minority-partition-handling.md) | Minority Partition Handling | < 4 hours |

## Quick Reference

### Common Commands

#### Health Checks
```bash
# Pod health
kubectl get pods -n erlmcp-prod
kubectl describe pod <pod-name> -n erlmcp-prod

# Service health
kubectl get svc -n erlmcp-prod
kubectl endpoints -n erlmcp-prod

# Application health
kubectl exec -n erlmcp-prod <pod-name> -- curl http://localhost:8081/health
```

#### Logs and Debugging
```bash
# Pod logs
kubectl logs -n erlmcp-prod <pod-name> --tail=100 -f

# All pods logs
kubectl logs -n erlmcp-prod -l app=erlmcp --tail=100

# Previous container logs
kubectl logs -n erlmcp-prod <pod-name> --previous
```

#### Erlang/OTP Specific
```bash
# Check node connectivity
docker exec <container> /opt/erlmcp/bin/erlmcp eval "net_adm:ping('nodename@host')."

# Check cluster membership
docker exec <container> /opt/erlmcp/bin/erlmcp eval "erlmcp_cluster_membership:get_members()."

# Check memory
docker exec <container> /opt/erlmcp/bin/erlmcp eval "erlang:memory(total)."

# Check process count
docker exec <container> /opt/erlmcp/bin/erlmcp eval "erlang:system_info(process_count)."
```

### Escalation Procedures

#### SEV-1 Escalation Timeline
1. **0-5 min**: Page on-call engineer
2. **5-15 min**: Page engineering manager
3. **15-30 min**: Page CTO
4. **30+ min**: Executive notification

#### War Room Setup
```bash
# Create incident channel
slack channel create "#incident-$(date +%Y%m%d-%H%M%S)"

# Set up Zoom bridge
zoom create meeting --topic "Incident Response"

# Assign roles
incident commander: <name>
technical lead: <name>
communications lead: <name>
```

## Testing and Validation

### Automated Testing
```bash
# Run failover tests
./scripts/disaster-recovery/failover-test.sh all

# Run specific test
./scripts/disaster-recovery/failover-test.sh database-failover

# Smoke test
./scripts/disaster-recovery/failover-test.sh smoke
```

### Manual Testing Checklist
- [ ] Verify health endpoints
- [ ] Check cluster membership
- [ ] Test database connectivity
- [ ] Validate session consistency
- [ ] Monitor resource usage
- [ ] Check alerting systems
- [ ] Verify logging pipeline
- [ ] Test communication channels

## Maintenance

### Daily
- Review alerting dashboards
- Check system logs for anomalies
- Verify backup completion
- Monitor resource trends

### Weekly
- Review runbook effectiveness
- Update contact information
- Test notification systems
- Review incident metrics

### Monthly
- Conduct failover drills
- Update runbooks based on learnings
- Review and update escalation paths
- Test disaster recovery procedures

### Quarterly
- Full disaster recovery test
- Review and update all runbooks
- Conduct team training
- Update documentation

## Metrics and KPIs

### Response Time Targets
| Severity | Target | Current |
|----------|--------|---------|
| SEV-1 | < 15 min | TBD |
| SEV-2 | < 1 hour | TBD |
| SEV-3 | < 4 hours | TBD |
| SEV-4 | < 1 day | TBD |

### Resolution Time Targets
| Category | MTTR | Current |
|----------|------|---------|
| Database | < 30 min | TBD |
| Network | < 1 hour | TBD |
| Application | < 2 hours | TBD |
| Full Cluster | < 4 hours | TBD |

## Emergency Contacts

### On-Call
- **Primary**: +1-XXX-XXX-XXXX
- **Secondary**: +1-XXX-XXX-XXXX
- **Escalation**: +1-XXX-XXX-XXXX

### Leadership
- **CTO**: cto@erlmcp.com
- **VP Engineering**: vp-eng@erlmcp.com
- **Director of Infrastructure**: infra@erlmcp.com

### External
- **AWS Support**: 1-800-818-4504
- **Cloudflare Support**: 1-888-613-8407
- **Database Support**: dba@erlmcp.com

## Contributing

When updating runbooks:

1. Update the "Last Updated" date
2. Increment version number for significant changes
3. Add clear "What Changed" section at top
4. Test new procedures in non-production first
5. Update related runbooks for cross-references
6. Notify team of changes via Slack

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 3.0.0 | Initial comprehensive runbook suite |

## Additional Resources

- [Architecture Documentation](../ha-architecture.md)
- [Deployment Guide](../deployment-guide.md)
- [Monitoring Stack](../monitoring/README.md)
- [Disaster Recovery Plan](../ha-disaster-recovery.md)

## Support

For questions or issues with runbooks:
- Create issue in repository
- Contact infrastructure team
- Page on-call for urgent issues
