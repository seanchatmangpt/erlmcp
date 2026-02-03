# erlmcp Upgrade Runbook

## Emergency Contact

- **On-Call Engineer**: [Contact Info]
- **Engineering Lead**: [Contact Info]
- ** escalation**: [Contact Info]

## Quick Reference

```bash
# Check system status
erlmcpctl status
erlmcpctl health

# Perform upgrade
./scripts/upgrade.sh 3.0.0

# Emergency rollback
./scripts/downgrade.sh 2.1.0 --force

# View logs
tail -f log/erlmcp.log
```

## Upgrade Scenarios

### Scenario 1: Planned Upgrade (Normal Flow)

**Trigger**: Scheduled upgrade to v3.0.0

**Steps**:
1. Pre-flight checks
2. Execute upgrade
3. Verify functionality
4. Monitor for 1 hour

**Time Estimate**: 5-10 minutes

**Rollback Plan**: Downgrade to v2.1.0

---

### Scenario 2: Upgrade Failure Mid-Process

**Symptom**: Upgrade fails at "transform_state" phase

**Actions**:
1. Check error logs: `grep ERROR log/erlmcp.log | tail -50`
2. Identify failure reason
3. Execute rollback: `./scripts/downgrade.sh 2.1.0`
4. Verify system recovery
5. Investigate root cause
6. Reattempt upgrade after fix

**Time Estimate**: 2-5 minutes

**Recovery**: Automatic rollback on critical failures

---

### Scenario 3: Post-Upgrade Performance Degradation

**Symptom**: High latency or CPU usage after upgrade

**Actions**:
1. Monitor metrics for 5 minutes
2. If no improvement, rollback: `./scripts/downgrade.sh 2.1.0`
3. Collect diagnostic data:
   - `erlmcpctl metrics`
   - `erlmcpctl processes`
   - `erlmcpctl memory`
4. Report issue to engineering

**Time Estimate**: 10 minutes

**Decision Point**: Rollback if > 2x performance degradation

---

### Scenario 4: Connection Drops During Upgrade

**Symptom**: Active connections terminated during upgrade

**Actions**:
1. Check transport status: `erlmcpctl transports`
2. Verify supervisor resume completed
3. Restart failed transports
4. If critical issue, rollback

**Time Estimate**: 5 minutes

**Prevention**: Use graceful drain before upgrade

---

### Scenario 5: ETS Table Corruption

**Symptom**: `ets_migration_failed` error

**Actions**:
1. Identify corrupted table from logs
2. Check table integrity: `ets:info(Table)`
3. Restore from backup if available
4. If no backup, rollback immediately
5. Escalate to database team

**Time Estimate**: 5-15 minutes

**Severity**: HIGH - Data loss potential

---

## Diagnostic Commands

### System Health

```bash
# Overall health
erlmcpctl health

# Detailed status
erlmcpctl status

# Version info
erlmcpctl version

# Uptime
erlmcpctl uptime
```

### Process Information

```bash
# All processes
erlmcpctl processes

# Supervisors
erlmcpctl supervisors

# Gen servers
erlmcpctl gen_servers

# Message queue depths
erlmcpctl queues
```

### Data Integrity

```bash
# ETS tables
erlmcpctl ets_tables

# Mnesia status
erlmcpctl mnesia_status

# Registry state
erlmcpctl registry
```

### Metrics

```bash
# Performance metrics
erlmcpctl metrics

# Resource usage
erlmcpctl resources

# Transport statistics
erlmcpctl transport_stats
```

## Log Analysis

### Upgrade Log Locations

```
log/erlmcp.log           # Main application log
log/upgrade.log          # Upgrade-specific log
log/error.log            # Error log
log/console.log          # Console output
```

### Key Log Patterns

**Successful Upgrade**:
```
[INFO] Upgrade to 3.0.0 started
[INFO] Pre-flight checks passed
[INFO] Upgrade completed successfully
[INFO] Verification passed
```

**Failed Upgrade**:
```
[ERROR] Upgrade failed at phase: transform_state
[ERROR] Reason: {transformation_failed, ...}
[WARN] Initiating automatic rollback
```

**Connection Issues**:
```
[WARN] Transport stdio disconnected during upgrade
[INFO] Re-establishing transport connection
```

## Troubleshooting Workflows

### Workflow 1: Upgrade Hangs

1. **Check Process Status**
   ```bash
   erlmcpctl supervisors
   ```

2. **Check Message Queues**
   ```bash
   erlmcpctl queues
   ```

3. **If supervisor suspended but not resuming**:
   - Force resume: `erlmcpctl resume_supervisors`
   - Or rollback: `./scripts/downgrade.sh 2.1.0 --force`

4. **Escalate** if unresolved after 5 minutes

### Workflow 2: Verification Failures

1. **Run Detailed Verification**
   ```erlang
   erlmcp_upgrade_smoke_test:run_post_upgrade_checks().
   ```

2. **Check Specific Failures**
   ```bash
   # Applications not started
   erlmcpctl applications

   # ETS tables missing
   erlmcpctl ets_tables

   # Version mismatch
   erlmcpctl version
   ```

3. **Address Each Failure**
   - Restart missing applications
   - Restore missing ETS tables
   - Rollback if critical issues

### Workflow 3: Performance Issues

1. **Collect Metrics**
   ```bash
   erlmcpctl metrics
   erlmcpctl resources
   ```

2. **Compare to Baseline**
   - CPU usage: Should be < 80%
   - Memory: Should be < 90%
   - Throughput: Should be within 10% of baseline

3. **Decision Matrix**
   ```
   Degradation < 10%  -> Monitor, no action
   Degradation 10-50% -> Investigate, consider rollback
   Degradation > 50%  -> Immediate rollback
   ```

## Escalation Matrix

| Severity | Response Time | Escalation Path |
|----------|---------------|-----------------|
| S1 - Critical | 5 minutes | On-Call → Eng Lead → CTO |
| S2 - High | 15 minutes | On-Call → Eng Lead |
| S3 - Medium | 1 hour | On-Call |
| S4 - Low | Next business day | Standard support |

## Decision Trees

### Upgrade Decision Tree

```
Start Upgrade
│
├─ Pre-flight checks pass?
│  ├─ Yes → Proceed with upgrade
│  └─ No → Fix issues or postpone
│
├─ Upgrade completes?
│  ├─ Yes → Run verification
│  └─ No → Automatic rollback → Investigate
│
├─ Verification passes?
│  ├─ Yes → Monitor for 1 hour
│  └─ No → Rollback → Investigate
│
└─ Post-monitoring OK?
   ├─ Yes → Upgrade complete
   └─ No → Rollback → Investigate
```

### Rollback Decision Tree

```
Upgrade Issue Detected
│
├─ Can fix within 5 minutes?
│  ├─ Yes → Fix and continue
│  └─ No → Rollback
│
├─ Data integrity at risk?
│  ├─ Yes → Immediate rollback
│  └─ No → Graceful rollback
│
└─ Rollback successful?
   ├─ Yes → Investigate and re-attempt
   └─ No → Emergency procedures
```

## Runbook Maintenance

- **Owner**: DevOps Team
- **Review Frequency**: Quarterly
- **Last Updated**: 2026-02-02
- **Next Review**: 2026-05-02

## Change History

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-02-02 | 1.0.0 | Initial runbook for v3.0.0 | erlmcp-team |

## Appendices

### A. Checksum Verification

```bash
# Verify release package integrity
sha256sum erlmcp-3.0.0.tar.gz

# Expected checksum
d41d8cd98f00b204e9800998ecf8427e  erlmcp-3.0.0.tar.gz
```

### B. Manual State Inspection

```erlang
% Connect to node
erl -name inspector@127.0.0.1 -setcookie erlmcp -remsh erlmcp@127.0.0.1

% Check upgrade status
{ok, Status} = erlmcp_upgrade_coordinator:get_upgrade_status().

% Check system health
Health = erlmcp_upgrade_monitor:get_system_health().

% Inspect ETS tables
ets:info(erlmcp_registry).
ets:tab2list(erlmcp_registry).
```

### C. Emergency Contacts (24/7)

- **On-Call (Primary)**: +1-555-0101
- **On-Call (Secondary)**: +1-555-0102
- **Escalation Manager**: +1-555-0103
- **Emergency Chat**: #erlmcp-emergency

### D. System Requirements for Upgrade

- **Minimum Erlang/OTP**: 28.0
- **Minimum Disk Space**: 100MB free
- **Minimum Memory**: 512MB free
- **Network**: Local node connectivity

### E. Post-Upgrade Verification Checklist

```
☐ All applications started
☐ All supervisors running
☐ All gen_servers reachable
☐ ETS tables intact
☐ Mnesia consistent (if used)
☐ Registry functional
☐ Transports operational
☐ Version correct (3.0.0)
☐ No error spikes in logs
☐ Performance within 10% of baseline
```

---

**Document Version**: 1.0.0
**Last Modified**: 2026-02-02
**Maintained By**: erlmcp DevOps Team
