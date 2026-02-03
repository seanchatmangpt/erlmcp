# erlmcp v3 Incident Response Runbooks

**Version**: 3.0.0
**Last Updated**: 2026-02-02

## Overview

This document contains incident response runbooks for erlmcp v3 deployment. Each runbook follows the standard format with symptoms, diagnosis, resolution, and prevention.

**For a complete list of all runbooks, see the [Runbooks Index](README.md).**

## Quick Links to Runbooks

### Database & Data
- [RB-009: Database Failover](DATABASE_FAILOVER.md) - PostgreSQL failover procedures
- [RB-012: Session Failover and State Recovery](SESSION_FAILOVER.md) - Session management during failover

### Network & Infrastructure
- [RB-010: Network Partition Recovery](NETWORK_PARTITION.md) - Split-brain detection and recovery
- [RB-011: Cascading Failures Recovery](CASCADING_FAILURES.md) - System-wide cascade prevention

### Advanced Scenarios
- [RB-008: Minority Partition Handling](RB-008-minority-partition-handling.md) - Minority partition scenarios

## Incident Severity Levels

| Level | Name | Response Time | Escalation |
|-------|------|---------------|------------|
| SEV-1 | Critical | < 15 minutes | CTO |
| SEV-2 | High | < 1 hour | Engineering Manager |
| SEV-3 | Medium | < 4 hours | Tech Lead |
| SEV-4 | Low | < 1 day | Developer |

## Runbook: RB-001 - Pod CrashLoopBackOff

### Symptoms

- Pod status: `CrashLoopBackOff`
- Pod restart count increasing
- Application logs showing startup failures

### Diagnosis

```bash
# Check pod status
kubectl get pods -n erlmcp-prod -l app=erlmcp

# Check pod logs
kubectl logs -n erlmcp-prod <pod-name> --previous

# Check pod events
kubectl describe pod -n erlmcp-prod <pod-name>

# Check resource limits
kubectl get pod -n erlmcp-prod <pod-name> -o jsonpath='{.spec.containers[*].resources}'
```

### Common Causes

1. **Out of Memory**: Container OOMKilled
2. **Missing Configuration**: Required env vars not set
3. **Dependency Failure**: Database/external service unavailable
4. **Startup Timeout**: Application not ready within probe timeout

### Resolution

#### Case 1: Out of Memory

```bash
# Check current limits
kubectl get statefulset erlmcp -n erlmcp-prod -o yaml | grep -A 10 resources

# Increase memory limit
kubectl patch statefulset erlmcp -n erlmcp-prod --type='json' \
  -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/resources/limits/memory", "value": "4Gi"}]'
```

#### Case 2: Missing Configuration

```bash
# Check configmap
kubectl get configmap erlmcp-config -n erlmcp-prod -o yaml

# Check secret
kubectl get secret erlmcp-secrets -n erlmcp-prod -o yaml

# Update missing values
kubectl patch configmap erlmcp-config -n erlmcp-prod --type=json \
  -p='[{"op": "add", "path": "/data/ERLMCP_LOG_LEVEL", "value": "info"}]'
```

#### Case 3: Dependency Failure

```bash
# Check connectivity to dependencies
kubectl exec -n erlmcp-prod <pod-name> -- nc -zv database.default.svc 5432

# If dependency is down, scale up dependency or wait for recovery
kubectl rollout status deployment/database -n default
```

### Prevention

- Set appropriate resource requests and limits
- Add pre-flight checks in init containers
- Use readiness probes with appropriate timeouts
- Implement circuit breakers for external dependencies

## Runbook: RB-002 - High Memory Usage

### Symptoms

- Memory usage > 90%
- OOM kills observed
- Pod evictions due to memory pressure

### Diagnosis

```bash
# Check memory usage across all pods
kubectl top pods -n erlmcp-prod -l app=erlmcp

# Get detailed metrics
kubectl exec -n erlmcp-prod <pod-name> -- erlang:memory()

# Check for memory leaks
kubectl exec -n erlmcp-prod <pod-name> -- recon:bin_leak(100)
```

### Common Causes

1. **Memory Leak**: Unreleased references
2. **Large Messages**: Memory growth in mailbox
3. **ETS Table Growth**: Unbounded table growth
4. **Binary Leak**: Large binaries not released

### Resolution

#### Case 1: Memory Leak

```bash
# Identify leaking processes
kubectl exec -n erlmcp-prod <pod-name> -- recon:proc_count(10)

# Check mailbox sizes
kubectl exec -n erlmcp-prod <pod-name> -- recon:proc_mailbox_lengths('all')

# Restart affected node
kubectl delete pod -n erlmcp-prod <pod-name>
```

#### Case 2: Large Messages

```bash
# Check message queue stats
kubectl exec -n erlmcp-prod <pod-name> -- erlmcp_registry:stat(queue)

# Enable backpressure
kubectl exec -n erlmcp-prod <pod-name> -- erlmcp_transport:set_backpressure(true)
```

### Prevention

- Enable memory monitoring with alerts
- Implement process heap size limits
- Use message queue monitoring
- Regular memory leak detection runs

## Runbook: RB-003 - High CPU Usage

### Symptoms

- CPU usage > 80%
- Slow response times
- Increased latency

### Diagnosis

```bash
# Check CPU usage
kubectl top pods -n erlmcp-prod -l app=erlmcp

# Check scheduler utilization
kubectl exec -n erlmcp-prod <pod-name> -- erlang:statistics(scheduler_wall_time)

# Check run queue
kubectl exec -n erlmcp-prod <pod-name> -- erlang:statistics(run_queue)
```

### Common Causes

1. **Busy Loop**: Process in infinite loop
2. **NIF Function**: Native function consuming CPU
3. **Garbage Collection**: Excessive GC
4. **High Load**: Insufficient capacity

### Resolution

#### Case 1: Busy Loop

```bash
# Find busy process
kubectl exec -n erlmcp-prod <pod-name> -- recon:proc_window(10, reductions)

# Suspend the process (if safe)
kubectl exec -n erlmcp-prod <pod-name> -- erlang:suspend_process(Pid)
```

#### Case 2: Insufficient Capacity

```bash
# Scale horizontally
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=5

# Or increase CPU limits
kubectl patch statefulset erlmcp -n erlmcp-prod --type='json' \
  -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/resources/limits/cpu", "value": "4000m"}]'
```

### Prevention

- Implement CPU usage monitoring
- Set HPA based on CPU
- Profile code before deployment
- Avoid busy loops in Erlang code

## Runbook: RB-004 - Network Partition

### Symptoms

- Pods unable to communicate
- Connection timeouts
- Cluster split-brain

### Diagnosis

```bash
# Check pod connectivity
kubectl exec -n erlmcp-prod <pod-name-1> -- ping -c 3 <pod-ip-2>

# Check Erlang distribution
kubectl exec -n erlmcp-prod <pod-name> -- net_adm:ping(Node)

# Check network policies
kubectl get networkpolicy -n erlmcp-prod
```

### Common Causes

1. **Network Policy**: Too restrictive rules
2. **Firewall**: External firewall blocking
3. **DNS Failure**: Cannot resolve hostnames
4. **CNI Issue**: Network plugin problems

### Resolution

#### Case 1: Network Policy

```bash
# Check current policies
kubectl get networkpolicy -n erlmcp-prod -o yaml

# Update policy to allow traffic
kubectl patch networkpolicy erlmcp -n erlmcp-prod --type='json' \
  -p='[{"op": "add", "path": "/spec/egress/-", "value": {"to": [{"podSelector": {}}]}}]'
```

#### Case 2: DNS Issues

```bash
# Check DNS resolution
kubectl exec -n erlmcp-prod <pod-name> -- nslookup erlmcp-headless.erlmcp-prod

# Check CoreDNS
kubectl get pods -n kube-system -l k8s-app=kube-dns

# Restart CoreDNS if needed
kubectl rollout restart deployment/coredns -n kube-system
```

### Prevention

- Test network policies before applying
- Use headless services for Erlang distribution
- Implement proper health checks
- Monitor network connectivity

## Runbook: RB-005 - Database Connection Issues

### Symptoms

- Database connection errors
- Increased connection pool usage
- Slow queries

### Diagnosis

```bash
# Check database connectivity
kubectl exec -n erlmcp-prod <pod-name> -- nc -zv database.default.svc 5432

# Check connection pool stats
kubectl exec -n erlmcp-prod <pod-name> -- erlmcp_db:pool_stats()

# Check database logs
kubectl logs -n default deployment/database --tail=100
```

### Common Causes

1. **Connection Leak**: Connections not released
2. **Pool Exhaustion**: Max connections reached
3. **Database Down**: Database unavailable
4. **Network Issue**: Cannot reach database

### Resolution

#### Case 1: Connection Leak

```bash
# Check active connections
kubectl exec -n erlmcp-prod <pod-name> -- erlmcp_db:active_connections()

# Restart affected pods to clear connections
kubectl rollout restart statefulset erlmcp -n erlmcp-prod
```

#### Case 2: Pool Exhaustion

```bash
# Increase pool size
kubectl patch configmap erlmcp-config -n erlmcp-prod --type=json \
  -p='[{"op": "add", "path": "/data/DB_POOL_SIZE", "value": "50"}]'

# Rollout restart to apply
kubectl rollout restart statefulset erlmcp -n erlmcp-prod
```

### Prevention

- Monitor connection pool usage
- Set appropriate pool limits
- Implement connection timeout
- Use connection health checks

## Runbook: RB-006 - Deployment Rollback

### Symptoms

- New deployment causing issues
- Canary metrics degraded
- User-reported errors after deployment

### Diagnosis

```bash
# Check deployment status
kubectl rollout status statefulset erlmcp -n erlmcp-prod

# Check canary status
kubectl get canary erlmcp -n erlmcp-prod

# Compare versions
kubectl get statefulset erlmcp -n erlmcp-prod -o jsonpath='{.spec.template.metadata.annotations}'
```

### Resolution

```bash
# Immediate rollback
kubectl rollout undo statefulset erlmcp -n erlmcp-prod

# Wait for rollback completion
kubectl rollout status statefulset erlmcp -n erlmcp-prod

# Verify rollback
kubectl get pods -n erlmcp-prod -l app=erlmcp

# Check health
kubectl exec -n erlmcp-prod <pod-name> -- curl http://localhost:8081/health
```

### If Rollback Fails

```bash
# Rollback to specific revision
kubectl rollout undo statefulset erlmcp -n erlmcp-prod --to-revision=<revision-number>

# Or manually restore from backup
kubectl apply -f /backup/erlmcp-statefulset-backup.yaml

# Scale to previous stable state
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=0
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=3
```

### Prevention

- Use canary deployments
- Implement automated rollback on failure
- Test deployments in staging first
- Monitor deployment metrics closely

## Runbook: RB-007 - Full Cluster Outage

### Symptoms

- All pods down
- No API responses
- Complete service unavailability

### Diagnosis

```bash
# Check cluster health
kubectl cluster-info

# Check node status
kubectl get nodes

# Check all pods
kubectl get pods --all-namespaces

# Check API server
kubectl get componentstatuses
```

### Resolution

#### Step 1: Identify Scope

```bash
# Is it just erlmcp or entire cluster?
kubectl get pods -n erlmcp-prod
kubectl get pods -n kube-system
```

#### Step 2: Check Control Plane

```bash
# Check API server logs
kubectl logs -n kube-system kube-apiserver-<node>

# Check etcd health
kubectl exec -n kube-system etcd-<node> -- etcdctl endpoint health
```

#### Step 3: Restore Services

```bash
# If control plane is down, contact cloud provider

# If application issue, scale to zero then back up
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=0
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=3
```

#### Step 4: Verify Recovery

```bash
# Check all pods are ready
kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=5m

# Run smoke tests
./scripts/smoke-tests.sh production
```

### Prevention

- Implement multi-AZ deployment
- Use cluster backup solutions
- Regular disaster recovery drills
- Monitor cluster health metrics

## Escalation Procedures

### SEV-1 Escalation

1. **0-5 min**: Page on-call engineer
2. **5-15 min**: Page engineering manager
3. **15-30 min**: Page CTO
4. **30+ min**: Executive notification

### War Room

For SEV-1 incidents:

1. Create incident Slack channel: `#incident-<ticket-number>`
2. Set up Zoom bridge for voice communication
3. Assign incident commander
4. Begin hourly status updates

### Post-Incident

1. Complete incident report within 24 hours
2. Schedule postmortem meeting
3. Create action items for prevention
4. Update runbooks based on learnings

## Communication Templates

### Initial Incident Announcement

```
**SEV-1 Incident**: <Brief Description>

**Impact**: <What is affected>
**Started**: <Time>
**Current Status**: <Investigating/Mitigating/Monitoring>
**Next Update**: <Time>

Follow updates in #incident-<ticket-number>
```

### Resolution Announcement

```
**RESOLVED**: <Incident Description>

**Root Cause**: <What happened>
**Resolution**: <What was fixed>
**Impact Duration**: <Time>
**Post-Incident Actions**: <Prevention steps>

Full postmortem to follow.
```

## References

- On-Call: https://pagerduty.example.com
- Metrics: https://grafana.erlmcp.io
- Logs: https://loki.erlmcp.io
- Runbook Repository: https://github.com/erlmcp/runbooks
