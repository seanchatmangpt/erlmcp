# Incident Response - erlmcp

## Executive Summary

This document defines incident response procedures for erlmcp production deployments, including severity classifications, escalation paths, runbooks for common failures, and post-mortem processes. The goal is to minimize Mean Time to Recovery (MTTR) through clear procedures and blameless learning.

**Key Metrics:**
- **P0 (Critical)**: Response time <15 minutes, MTTR <1 hour
- **P1 (High)**: Response time <1 hour, MTTR <4 hours
- **P2 (Medium)**: Response time <4 hours, MTTR <24 hours
- **P3 (Low)**: Response time <24 hours, MTTR <1 week

**Philosophy:** Toyota Andon system - anyone can pull the cord, no blame, focus on systemic improvement.

## Incident Severity Levels

### P0 - Critical (Production Down)

**Criteria:**
- Complete service outage
- Data loss or corruption
- Security breach
- Multiple client failures (>50% affected)

**Examples:**
- erlmcp_registry crash loop (all connections lost)
- Secrets backend compromised
- OOM killer terminating BEAM VM
- Network partition in production cluster

**Response SLA:**
- Detection: Automated alerts (immediate)
- Response: <15 minutes
- Escalation: Immediate to on-call engineer + incident commander
- MTTR Target: <1 hour

**Notification:**
- PagerDuty/OpsGenie high-urgency alert
- #incidents Slack channel
- Email to engineering-oncall@
- SMS to incident commander

### P1 - High (Partial Outage)

**Criteria:**
- Degraded service (10-50% clients affected)
- Critical feature unavailable
- Performance degradation (>3x latency increase)
- Circuit breakers triggered

**Examples:**
- Connection pool exhaustion (new connections rejected)
- Single transport type down (e.g., WebSocket failing)
- Memory approaching limits (>85% heap usage)
- Database connection pool saturated

**Response SLA:**
- Detection: Automated alerts (within 5 minutes)
- Response: <1 hour
- Escalation: On-call engineer, notify team lead
- MTTR Target: <4 hours

**Notification:**
- PagerDuty/OpsGenie medium-urgency alert
- #incidents Slack channel
- Email to engineering-team@

### P2 - Medium (Non-Critical Issue)

**Criteria:**
- Single client affected
- Minor feature degradation
- Performance anomaly (non-critical)
- Elevated error rate (<5%)

**Examples:**
- Single session leak (detectable but not cascading)
- Metrics collection delay
- Dashboard rendering issues
- Non-critical transport warning

**Response SLA:**
- Detection: Monitoring dashboard (manual check)
- Response: <4 hours (during business hours)
- Escalation: Assign to engineer, no page
- MTTR Target: <24 hours

**Notification:**
- JIRA ticket creation
- #engineering Slack mention
- Email to assigned engineer

### P3 - Low (Minor Issue)

**Criteria:**
- Cosmetic issues
- Documentation errors
- Non-impacting warnings
- Feature requests

**Examples:**
- Log message typo
- Dashboard formatting issue
- Unused code warnings
- Performance optimization opportunities

**Response SLA:**
- Detection: Bug report or manual discovery
- Response: <24 hours (acknowledge)
- Escalation: Backlog prioritization
- MTTR Target: <1 week

**Notification:**
- JIRA ticket
- Weekly engineering sync

## Incident Response Roles

### Incident Commander (IC)

**Responsibilities:**
- Declare incident severity
- Coordinate response team
- Make escalation decisions
- Communicate with stakeholders
- Lead post-mortem

**Authority:**
- Override normal procedures
- Request additional resources
- Make rollback decisions
- Pause non-critical work

**Rotation:**
- Weekly rotation among senior engineers
- Backup IC always assigned
- Handoff during shift changes

### On-Call Engineer

**Responsibilities:**
- First responder to alerts
- Execute runbooks
- Gather diagnostic data
- Escalate to IC if needed

**Tools Access:**
- Production SSH/kubectl access
- erlmcp observer access
- Metrics dashboards (Grafana)
- Log aggregation (ELK/Splunk)

**Rotation:**
- 24/7 coverage (1-week shifts)
- Primary + backup on-call
- Geographic distribution (follow-the-sun)

### Subject Matter Expert (SME)

**Responsibilities:**
- Deep technical knowledge of specific component
- Advise on complex failures
- Review proposed fixes

**Modules:**
- Registry SME (gproc, routing)
- Transport SME (stdio, TCP, WebSocket)
- Session SME (persistence, failover)
- Observability SME (metrics, tracing)

## Escalation Procedures

### Escalation Decision Tree

```
┌─────────────────────────────────────────────────┐
│  Alert Triggered                                │
└──────────────────┬──────────────────────────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │ On-Call acknowledges │
        │    within 15 min     │
        └──────────┬───────────┘
                   │
                   ▼
        ┌──────────────────────┐      NO
        │ Can resolve in 30min?├────────┐
        └──────────┬───────────┘        │
                   │ YES                │
                   ▼                    ▼
        ┌──────────────────────┐   ┌────────────────┐
        │ Execute runbook      │   │ Escalate to IC │
        └──────────┬───────────┘   └────────┬───────┘
                   │                        │
                   ▼                        ▼
        ┌──────────────────────┐   ┌────────────────────┐
        │ Verify resolution    │   │ IC declares P0/P1  │
        └──────────┬───────────┘   └────────┬───────────┘
                   │                        │
                   ▼                        ▼
        ┌──────────────────────┐   ┌────────────────────┐
        │ Document in ticket   │   │ Assemble war room  │
        └──────────────────────┘   └────────────────────┘
```

### Escalation Criteria

**Escalate to Incident Commander if:**
- Runbook execution fails
- Issue not resolved in 30 minutes
- Root cause unclear
- Multiple systems affected
- Customer-facing impact confirmed

**Escalate to Engineering Manager if:**
- MTTR exceeds SLA by 50%
- Requires emergency deployment
- External vendor coordination needed
- Customer communication required

**Escalate to CTO if:**
- Data breach suspected
- Legal/compliance implications
- Major customer impact (>1000 users)
- Press/media involvement

## Communication Templates

### Initial Alert (P0/P1)

**Subject:** [P0] erlmcp Production Incident - [Brief Description]

```
SEVERITY: P0 (Critical)
STATUS: Investigating
IMPACT: All WebSocket connections failing (estimated 5,000 users affected)
START TIME: 2026-01-31 14:23:45 UTC
INCIDENT COMMANDER: Alice Chen (alice@example.com)
MONITORING: https://grafana.example.com/incident-2026-01-31-001

CURRENT ACTIONS:
- Analyzing connection pool metrics
- Reviewing recent deployments (last deploy: 2026-01-31 12:00 UTC)
- Rolling back to v2.1.3 (ETA: 5 minutes)

NEXT UPDATE: 15 minutes (14:45 UTC)
```

### Status Update

**Subject:** [P0] UPDATE - erlmcp Production Incident

```
SEVERITY: P0 (Critical)
STATUS: Mitigating
IMPACT: WebSocket connections partially restored (70% recovered)
ELAPSED: 22 minutes

ACTIONS TAKEN:
✅ Rolled back to v2.1.3
✅ Restarted connection pool supervisor
⏳ Monitoring connection recovery rate

ROOT CAUSE (preliminary): Connection pool exhaustion due to missing timeout in v2.2.0

NEXT STEPS:
- Monitor for 10 more minutes
- Declare resolved if error rate <1%
- Schedule post-mortem for 2026-02-01 10:00 UTC

NEXT UPDATE: 10 minutes (14:55 UTC)
```

### Resolution Notice

**Subject:** [P0] RESOLVED - erlmcp Production Incident

```
SEVERITY: P0 (Critical)
STATUS: Resolved
IMPACT: All WebSocket connections restored
DURATION: 37 minutes (14:23 - 15:00 UTC)
INCIDENT COMMANDER: Alice Chen

ROOT CAUSE:
Connection pool exhaustion introduced in v2.2.0 due to missing gen_tcp:recv/3 timeout.
Connections accumulated indefinitely, exceeding max pool size (10,000).

RESOLUTION:
1. Rolled back to v2.1.3
2. Restarted erlmcp_transport_pool supervisor
3. Verified connection recovery (99.8% restored within 10 minutes)

FOLLOW-UP ACTIONS:
- [ ] Fix missing timeout in v2.2.1 (owner: Bob, ETA: 2026-02-01)
- [ ] Add connection pool saturation alert (owner: Carol, ETA: 2026-02-02)
- [ ] Post-mortem scheduled: 2026-02-01 10:00 UTC

MONITORING:
https://grafana.example.com/incident-2026-01-31-001
```

## Runbooks

### 1. Memory Exhaustion

**Symptoms:**
- Erlang VM memory >90% of limit
- `erlmcp_memory_guard` triggered
- Slow response times
- Process mailbox backlogs

**Diagnostic Commands:**
```erlang
% Connect to production node
erl -remsh erlmcp@prod-node-1

% Check memory usage
erlang:memory().
%=> [{total, 8_500_000_000}, {processes, 6_000_000_000}, ...]

% Find top memory consumers
recon:proc_count(memory, 10).
%=> [{<0.1234.0>, 250_000_000, [erlmcp_session_manager, ...]}, ...]

% Check process info
process_info(pid(0,1234,0), [memory, message_queue_len, heap_size]).
%=> [{memory, 250_000_000}, {message_queue_len, 50000}, ...]
```

**Resolution Steps:**

1. **Immediate Mitigation**
   ```bash
   # If memory >95%, restart node (with drain)
   kubectl drain erlmcp-prod-1 --grace-period=60
   kubectl delete pod erlmcp-prod-1
   ```

2. **Identify Memory Leak**
   ```erlang
   % Enable memory profiling
   erlmcp_profiler:start(memory, #{duration => 60000}).

   % Check session count
   erlmcp_session_manager:count().
   %=> 150_000  % Expected: <100,000

   % Find stale sessions
   erlmcp_session_manager:find_expired().
   %=> [<<"session_abc">>, <<"session_def">>, ...]
   ```

3. **Cleanup**
   ```erlang
   % Force cleanup of expired sessions
   erlmcp_session_manager:cleanup_expired().

   % Verify memory decrease
   erlang:memory(total).
   %=> 4_500_000_000  % Down from 8.5GB
   ```

4. **Prevent Recurrence**
   - Review session timeout settings
   - Enable automatic cleanup cron
   - Add session count alerts (threshold: 100,000)

**Post-Incident:**
- Analyze heap dumps: `recon:bin_leak(10).`
- Review ETS table sizes: `ets:i().`
- Check for binary reference leaks

### 2. Circuit Breaker Triggered

**Symptoms:**
- `erlmcp_circuit_breaker` in `:open` state
- Requests rejected with `{error, circuit_open}`
- Elevated error rate in logs

**Diagnostic Commands:**
```erlang
% Check circuit breaker status
erlmcp_circuit_breaker:status(tool_executor).
%=> #{state => open,
%     failures => 15,
%     threshold => 10,
%     last_failure => {{2026,1,31},{14,23,45}}}

% Check recent errors
erlmcp_circuit_breaker:get_errors(tool_executor, 100).
%=> [{timeout, <<"call_tool timeout after 5000ms">>}, ...]
```

**Resolution Steps:**

1. **Identify Root Cause**
   ```erlang
   % Check downstream service health
   erlmcp_health_monitor:check_dependency(llm_backend).
   %=> {error, {connection_refused, "http://llm.example.com:8080"}}
   ```

2. **Verify Downstream**
   ```bash
   # Check backend availability
   curl -f http://llm.example.com:8080/health
   # HTTP 503 Service Unavailable

   # Check network connectivity
   ping llm.example.com
   # Timeout
   ```

3. **Reset Circuit Breaker (if downstream recovered)**
   ```erlang
   % Manual reset (only after verifying downstream health)
   erlmcp_circuit_breaker:reset(tool_executor).
   %=> ok

   % Verify state
   erlmcp_circuit_breaker:status(tool_executor).
   %=> #{state => half_open, ...}
   ```

4. **Monitor Recovery**
   ```erlang
   % Watch for successful requests
   erlmcp_circuit_breaker:stats(tool_executor).
   %=> #{requests => 10, successes => 10, failures => 0}
   %=> State transitions: open -> half_open -> closed
   ```

**Prevention:**
- Implement retry with exponential backoff
- Add health checks for downstream services
- Configure circuit breaker thresholds appropriately

### 3. Database Connection Pool Exhausted

**Symptoms:**
- `{error, pool_timeout}` in logs
- New sessions fail to create
- Connection pool at max capacity

**Diagnostic Commands:**
```erlang
% Check pool status
poolboy:status(erlmcp_db_pool).
%=> {ready, 0, 25, 25}  % {ready, Available, Busy, Max}
%=> All 25 connections busy, 0 available

% Check pool queue
gen_server:call(erlmcp_db_pool, queue_size).
%=> 150  % 150 requests waiting for connections
```

**Resolution Steps:**

1. **Increase Pool Size (Temporary)**
   ```erlang
   % Dynamically increase pool size
   supervisor:terminate_child(erlmcp_sup, erlmcp_db_pool),
   supervisor:restart_child(erlmcp_sup, erlmcp_db_pool,
       [{size, 50}, {max_overflow, 20}]).
   ```

2. **Find Connection Leaks**
   ```erlang
   % Check for long-running transactions
   erlmcp_db:list_active_connections().
   %=> [{pid(0,5678,0), 125000}]  % 125 seconds old!

   % Inspect process
   sys:get_state(pid(0,5678,0)).
   %=> Transaction started but never committed
   ```

3. **Kill Stale Connections**
   ```erlang
   % Force rollback of stuck transactions
   erlmcp_db:kill_connection(pid(0,5678,0)).

   % Verify pool recovery
   poolboy:status(erlmcp_db_pool).
   %=> {ready, 10, 15, 25}  % 10 available again
   ```

4. **Root Cause Analysis**
   - Review code for missing transaction commits
   - Add connection timeout enforcement
   - Implement connection leak detection

**Configuration:**
```erlang
% apps/erlmcp_core/config/sys.config
{erlmcp_db_pool, [
    {size, 25},              % Base pool size
    {max_overflow, 10},      % Additional connections
    {checkout_timeout, 5000} % Max wait time (ms)
]}.
```

### 4. Secrets Backend Failure

**Symptoms:**
- `{error, secrets_unavailable}` in logs
- Authentication failures
- Cannot start new sessions

**Diagnostic Commands:**
```bash
# Check Vault status (if using HashiCorp Vault)
curl -f https://vault.example.com:8200/v1/sys/health
# HTTP 503 - Vault sealed

# Check AWS Secrets Manager (if using AWS)
aws secretsmanager get-secret-value --secret-id erlmcp/prod
# ServiceUnavailable
```

**Resolution Steps:**

1. **Failover to Backup Backend**
   ```erlang
   % Switch to local encrypted backend
   application:set_env(erlmcp_secrets, backend, local_encrypted).
   erlmcp_secrets:reload_backend().
   ```

2. **Verify Failover**
   ```erlang
   erlmcp_secrets:get(<<"api_key">>).
   %=> {ok, <<"backup_encrypted_value">>}
   ```

3. **Unseal Vault (if applicable)**
   ```bash
   # Unseal Vault with threshold key shares
   vault operator unseal $KEY_SHARE_1
   vault operator unseal $KEY_SHARE_2
   vault operator unseal $KEY_SHARE_3

   # Verify unsealed
   vault status
   # Sealed: false
   ```

4. **Restore Primary Backend**
   ```erlang
   application:set_env(erlmcp_secrets, backend, vault).
   erlmcp_secrets:reload_backend().

   % Verify primary backend
   erlmcp_secrets:health_check().
   %=> {ok, #{backend => vault, status => healthy}}
   ```

**Prevention:**
- Implement multi-backend failover
- Cache secrets with TTL
- Add secrets backend health checks

### 5. Network Partition (Split-Brain)

**Symptoms:**
- Node disconnection alerts
- Duplicate registry entries
- Inconsistent session state

**Diagnostic Commands:**
```erlang
% Check node connectivity
nodes().
%=> [erlmcp@prod-2, erlmcp@prod-3]  % prod-1 missing!

% Check split-brain detector
erlmcp_split_brain_detector:status().
%=> {partitioned, [
    {partition_a, [erlmcp@prod-1]},
    {partition_b, [erlmcp@prod-2, erlmcp@prod-3]}
]}
```

**Resolution Steps:**

1. **Identify Winner (Majority Partition)**
   ```erlang
   % Partition B has majority (2/3 nodes)
   % Partition A is isolated (1/3 node)
   ```

2. **Heal Partition**
   ```bash
   # Restart isolated node (prod-1)
   kubectl delete pod erlmcp-prod-1

   # Verify reconnection
   kubectl logs erlmcp-prod-1 | grep "Node connected"
   # Node connected: erlmcp@prod-2
   # Node connected: erlmcp@prod-3
   ```

3. **Resolve State Conflicts**
   ```erlang
   % Run conflict resolution
   erlmcp_split_brain_detector:resolve_conflicts(winner_takes_all).

   % Verify registry consistency
   erlmcp_registry:validate_consistency().
   %=> ok
   ```

4. **Verify Session Replication**
   ```erlang
   % Check session counts match across nodes
   rpc:multicall(nodes(), erlmcp_session_manager, count, []).
   %=> {[5000, 5000, 5000], []}  % All nodes agree on 5000 sessions
   ```

**Prevention:**
- Configure network partition detection
- Use winner_takes_all strategy for small clusters
- Implement session replication with quorum

### 6. High Latency (Performance Degradation)

**Symptoms:**
- Request latency >500ms (p99)
- Dashboard shows red latency alerts
- Circuit breakers approaching thresholds

**Diagnostic Commands:**
```erlang
% Check latency percentiles
erlmcp_metrics:get_histogram(request_latency).
%=> #{p50 => 250, p95 => 850, p99 => 1500}  % ms

% Find slow operations
erlmcp_profiler:start(time, #{duration => 30000}).
% Analyze results after 30s

% Check message queue backlogs
recon:proc_count(message_queue_len, 10).
%=> [{<0.9876.0>, 5000, [erlmcp_server, ...]}, ...]
```

**Resolution Steps:**

1. **Identify Bottleneck**
   ```erlang
   % Check scheduler utilization
   erlang:statistics(scheduler_wall_time).
   %=> [{1, 95_000_000, 100_000_000}, ...]  % 95% busy

   % Check process reductions
   recon:proc_count(reductions, 10).
   %=> [{<0.9876.0>, 10_000_000, [json_encode_heavy, ...]}, ...]
   ```

2. **Apply Backpressure**
   ```erlang
   % Enable backpressure on overloaded processes
   erlmcp_backpressure:enable(erlmcp_server).

   % Throttle incoming requests
   erlmcp_rate_limiter:update_limit(1000).  % Reduce from 5000 to 1000 req/s
   ```

3. **Scale Horizontally**
   ```bash
   # Add more nodes to cluster
   kubectl scale deployment erlmcp --replicas=5  # Up from 3
   ```

4. **Optimize Hot Paths**
   - Profile JSON encoding/decoding
   - Cache frequently accessed data
   - Use binary matching optimizations

**Root Cause Examples:**
- Inefficient JSON encoding (large payloads)
- N+1 queries to session backend
- Missing ETS index on lookup key

## Post-Mortem Process

### Timeline

- **Incident Resolved** → **Post-mortem within 48 hours**
- **Draft Post-mortem** → **Review within 1 week**
- **Action Items** → **Track to completion**

### Blameless Post-Mortem Template

```markdown
# Post-Mortem: [Incident Title]

**Incident ID:** INC-2026-01-31-001
**Date:** 2026-01-31
**Severity:** P0 (Critical)
**Duration:** 37 minutes (14:23 - 15:00 UTC)
**Incident Commander:** Alice Chen
**Responders:** Bob Smith (on-call), Carol Lee (SME)

## Summary

WebSocket connections failed for all 5,000 active users due to connection pool
exhaustion introduced in v2.2.0. Issue was mitigated by rolling back to v2.1.3.

## Impact

- **Users Affected:** 5,000 (100% of active WebSocket users)
- **Duration:** 37 minutes
- **Revenue Impact:** $1,250 (estimated, based on SLA credits)
- **Customer Escalations:** 3 (via support tickets)

## Root Cause

Missing timeout parameter in `gen_tcp:recv/3` call added in v2.2.0 commit abc123.
Without timeout, connections never released back to pool, accumulating until max
pool size (10,000) reached. New connection requests blocked indefinitely.

**Code Diff:**
```erlang
% v2.1.3 (working)
gen_tcp:recv(Socket, 0, 5000)

% v2.2.0 (broken)
gen_tcp:recv(Socket, 0)  % Missing timeout!
```

## Timeline (UTC)

| Time  | Event |
|-------|-------|
| 12:00 | Deployed v2.2.0 to production |
| 14:15 | Connection pool saturation began |
| 14:23 | First alert: WebSocket connection failures |
| 14:25 | On-call acknowledged alert |
| 14:30 | Escalated to Incident Commander |
| 14:35 | Identified pool exhaustion, initiated rollback |
| 14:40 | Rollback to v2.1.3 complete |
| 14:45 | Connections recovering (50%) |
| 15:00 | Incident resolved (99.8% connections restored) |

## What Went Well

- ✅ Automated alerting detected issue quickly (8 minutes after onset)
- ✅ Rollback procedure well-documented and fast (5 minutes)
- ✅ On-call responded within SLA (<15 minutes)
- ✅ Communication timely and clear

## What Went Wrong

- ❌ Missing unit test for timeout parameter
- ❌ Code review didn't catch missing timeout
- ❌ No canary deployment to detect issue before full rollout
- ❌ Connection pool saturation alert missing
- ❌ Integration test didn't simulate sustained load

## Action Items

| Action | Owner | Due Date | Priority | Status |
|--------|-------|----------|----------|--------|
| Add timeout parameter to gen_tcp:recv/3 in v2.2.1 | Bob | 2026-02-01 | P0 | ✅ Done |
| Add unit test for connection timeout | Bob | 2026-02-01 | P0 | ✅ Done |
| Create connection pool saturation alert | Carol | 2026-02-02 | P1 | ⏳ In Progress |
| Implement canary deployment (10% traffic) | DevOps | 2026-02-05 | P1 | 📋 Planned |
| Add integration test for sustained load | QA | 2026-02-07 | P2 | 📋 Planned |
| Review all gen_tcp calls for missing timeouts | Team | 2026-02-10 | P2 | 📋 Planned |

## Lessons Learned

1. **Timeouts are critical** - All blocking calls must have explicit timeouts
2. **Canary deployments prevent cascading failures** - Deploy to 10% first
3. **Connection pool limits need monitoring** - Alert before saturation
4. **Code review checklists** - Add "verify all blocking calls have timeouts"

## Related Incidents

- INC-2025-12-15-003: Similar timeout issue in HTTP client (resolved)
```

### Post-Mortem Review Meeting

**Agenda (1 hour):**
1. Incident timeline walkthrough (10 min)
2. Root cause analysis (15 min)
3. What went well / What went wrong (15 min)
4. Action items discussion (15 min)
5. Process improvements (5 min)

**Attendees:**
- Incident Commander
- Responders
- Engineering Manager
- Affected team members

**Follow-up:**
- Share post-mortem with entire engineering team
- Track action items in JIRA
- Review in monthly reliability meeting

## Contact Information

### On-Call Rotation

| Role | Primary | Backup | Phone | Email |
|------|---------|--------|-------|-------|
| **Incident Commander** | Alice Chen | Bob Smith | +1-555-0101 | alice@ |
| **On-Call Engineer** | Carol Lee | Dave Wong | +1-555-0102 | carol@ |
| **Registry SME** | Eve Martin | Frank Liu | +1-555-0103 | eve@ |
| **Transport SME** | Grace Kim | Henry Zhao | +1-555-0104 | grace@ |

### Escalation Contacts

| Level | Contact | Phone | Email | Slack |
|-------|---------|-------|-------|-------|
| Engineering Manager | Ian Taylor | +1-555-0201 | ian@ | @ian |
| VP Engineering | Jane Doe | +1-555-0301 | jane@ | @jane |
| CTO | John Smith | +1-555-0401 | john@ | @john |

### External Vendors

| Service | Contact | Phone | Support Portal |
|---------|---------|-------|----------------|
| HashiCorp Vault | support@hashicorp.com | 1-800-VAULT-01 | https://support.hashicorp.com |
| AWS Support | Enterprise Support | 1-800-AWS-SUPP | https://console.aws.amazon.com/support |

## Tools and Dashboards

| Tool | URL | Purpose |
|------|-----|---------|
| **Grafana** | https://grafana.example.com/erlmcp | Real-time metrics |
| **PagerDuty** | https://example.pagerduty.com | Alerting |
| **Splunk** | https://splunk.example.com | Log aggregation |
| **Datadog APM** | https://app.datadoghq.com | Distributed tracing |
| **Incident Manager** | https://incident.example.com | Incident tracking |

## Andon System Integration

### Pull the Cord

**Anyone can trigger an incident:**
```bash
# Via CLI
erlmcp-admin incident declare --severity=P0 --description="Description"

# Via API
curl -X POST https://api.example.com/incidents \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"severity": "P0", "description": "..."}'

# Via Slack
/incident declare P0 WebSocket connections failing
```

### Automatic Shutdown

**Quality gates trigger automatic rollback:**
```erlang
% If error rate >5% for 5 minutes
erlmcp_health_monitor:check_error_rate().
%=> {unhealthy, #{error_rate => 8.5, threshold => 5.0}}

% Trigger automatic rollback
erlmcp_deploy:rollback(automatic, <<"v2.1.3">>).
```

---

**References:**
- Toyota Production System (Andon, Jidoka)
- [Google SRE Book - Incident Response](https://sre.google/sre-book/managing-incidents/)
- [PagerDuty Incident Response](https://response.pagerduty.com/)
- erlmcp Runbooks: `/docs/runbooks/`
