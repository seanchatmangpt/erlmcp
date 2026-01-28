# Disaster Recovery Quick Reference Guide
## erlmcp - Emergency Operations

**PRINT THIS DOCUMENT AND KEEP IT ACCESSIBLE**

---

## EMERGENCY CONTACTS

| Role | Contact | Phone | Email |
|------|---------|-------|-------|
| On-Call Ops Lead | [NAME] | [PHONE] | [EMAIL] |
| Database Team | [NAME] | [PHONE] | [EMAIL] |
| Infrastructure Team | [NAME] | [PHONE] | [EMAIL] |
| Project Manager | [NAME] | [PHONE] | [EMAIL] |

---

## INCIDENT RESPONSE FLOWCHART

```
┌─────────────────────┐
│  INCIDENT DETECTED  │
│  (Alert triggered)  │
└──────────┬──────────┘
           │
           ↓
    ┌──────────────┐
    │  Activate    │
    │  Incident    │
    │  Command     │
    └──────┬───────┘
           │
           ↓
    ┌──────────────────────────────────┐
    │  Assess Impact                   │
    │  - Check health dashboard        │
    │  - Review error logs             │
    │  - Determine RTO/RPO needed      │
    └──────┬───────────────────────────┘
           │
       ┌───┴────┬──────────┬─────────┐
       │         │          │         │
       ↓         ↓          ↓         ↓
   [Process] [Network] [Data] [Node
    Crash]  [Issue]   [Loss]  Failure]
       │         │        │      │
       ↓         ↓        ↓      ↓
   [Quick] [Check] [Check  [Promote
    Restart] [Routes] Backups] Replica]
       │      │        │       │
       └──────┴────┬───┴───────┘
                   │
                   ↓
          ┌────────────────┐
          │  Verify Fix    │
          │  - Run tests   │
          │  - Check logs  │
          │  - Smoke test  │
          └────────┬───────┘
                   │
                   ↓
          ┌────────────────┐
          │  Communicate   │
          │  - Update team │
          │  - Notify users│
          │  - Post-IR     │
          └────────────────┘
```

---

## QUICK DIAGNOSIS

### Is erlmcp down?

```bash
# Check if process running
ps aux | grep erlmcp

# Check port listening
netstat -tlnp | grep 8080

# Check logs
tail -100 logs/erlmcp.log

# Check health endpoint
curl http://localhost:9090/metrics
```

### What's the impact?

```
If HTTP responses are:
- Timeout (no reply)       → Process dead or overloaded
- 500 errors              → Application error (check logs)
- Connection refused      → Port not listening
- Intermittent errors     → Rate limiting or queue full
```

---

## RECOVERY PROCEDURES

### Procedure 1: Single Process Restart (FASTEST)

**RTO**: < 1 minute
**Data Loss**: None (stateless)

```bash
# 1. Stop the process
pkill -9 beam

# 2. Start it again
cd /Users/sac/erlmcp
make console

# 3. Verify it's running
curl http://localhost:8080/health

# 4. Check logs for errors
tail -50 logs/erlmcp.log
```

---

### Procedure 2: TCPS Data Restore (MODERATE)

**RTO**: 5-10 minutes
**Data Loss**: Maximum 1 hour (last backup)

```bash
# 1. Identify latest backup
ls -lt /Users/sac/erlmcp/priv/tcps/backups/ | head -5

# 2. STOP the application
pkill -9 beam
sleep 2

# 3. Restore from backup
tar -xzf /Users/sac/erlmcp/priv/tcps/backups/full-TIMESTAMP.tar.gz \
    -C /Users/sac/erlmcp/

# 4. Verify integrity
cd /Users/sac/erlmcp
make console
> tcps_persistence:verify_integrity().
% Should return: {ok, Report}

# 5. Rebuild indexes
> tcps_ontology_index:rebuild_all().
% Wait for completion

# 6. Verify data accessible
> tcps_persistence:list_receipts().
% Should return: {ok, [Receipt1, Receipt2, ...]}

# 7. START the application normally
```

---

### Procedure 3: Multi-Node Failover (ADVANCED)

**RTO**: 10-30 seconds
**Data Loss**: None (replicated)
**Prerequisites**: Replica node must be running and healthy

```bash
# 1. Check primary node status
ssh primary-host
erl -sname debug -setcookie erlmcp
> net_adm:ping('erlmcp@primary').
% Should return: 'pong' if alive, 'pang' if dead

# 2. If primary is dead, promote replica
ssh replica-host
erl -sname debug -setcookie erlmcp
> erlmcp_enterprise_session_replication:trigger_failover().
% Should return: ok

# 3. Update DNS/load balancer to point to replica
# Edit load balancer config to route to replica-host

# 4. Notify clients to reconnect
# Old connections will time out, new ones connect to replica

# 5. Once primary is fixed, configure as replica again
# OR promote primary back (planned failback)
```

---

### Procedure 4: Complete Data Loss Recovery

**RTO**: 30-60 minutes
**Data Loss**: Last 1 hour

**Prerequisites**: Off-site backup available

```bash
# 1. On new/clean system, restore from backup
# (Example: restoring to a fresh EC2 instance)

# 2. Copy backup from S3
aws s3 cp s3://erlmcp-backups/full-TIMESTAMP.tar.gz ./

# 3. Extract to application directory
tar -xzf full-TIMESTAMP.tar.gz -C /opt/erlmcp/

# 4. Start application
cd /opt/erlmcp
make console

# 5. Verify all data present
> tcps_persistence:verify_integrity().

# 6. Update DNS to point to new instance
# OR update in-service traffic to new instance

# 7. Monitor for any data inconsistencies
# Check logs for errors in first 10 minutes
```

---

## HEALTH CHECK COMMANDS

### Check Process Health

```erlang
% In erl console
erlmcp_health_monitor:get_all_recovery_status().

% Returns:
%   ✅ - Green (healthy)
%   ⚠️  - Yellow (degraded)
%   ❌ - Red (failed)
```

### Check Memory Usage

```erlang
erlang:memory().

% Look for:
%   total < 1 GB (normal)
%   total > 2 GB (investigate OOM)
```

### Check Connection Count

```erlang
erlang:system_info(schedulers_online).
erlang:statistics(io).
```

### Check Message Queue Depth

```erlang
process_info(whereis(erlmcp_server), message_queue_len).

% Should be:
%   < 100 (normal)
%   100-1000 (backpressure active)
%   > 1000 (critical - circuit breaker may open)
```

---

## BACKUP & RESTORE QUICK COMMANDS

### Create Manual Backup

```erlang
% In erl console
tcps_persistence:backup(full).
% Returns: {ok, "/path/to/backup-TIMESTAMP.tar.gz"}
```

### List Available Backups

```bash
ls -lh /Users/sac/erlmcp/priv/tcps/backups/
# Output:
# full-2026-01-27T13:45:30Z.tar.gz
# full-2026-01-27T13:45:30Z.tar.gz.sha256
# incremental-2026-01-27T14:00:00Z.tar.gz
```

### Verify Backup Integrity

```erlang
tcps_persistence:verify_integrity().
% Returns: {ok, #{valid => true, files_checked => 150, ...}}
```

### Restore from Backup

```erlang
tcps_persistence:restore("/path/to/backup-TIMESTAMP.tar.gz").
% Returns: {ok, Report}
% Stops services, restores, rebuilds indexes, restarts
```

---

## LOGS & DIAGNOSTICS

### View Application Logs

```bash
# Real-time log tail
tail -f logs/erlmcp.log

# Search for errors
grep "ERROR\|error" logs/erlmcp.log

# Search for specific component
grep "erlmcp_server\|transport" logs/erlmcp.log

# View last N minutes
tail -100 logs/erlmcp.log | grep -A5 "ERROR"
```

### Collect Diagnostic Bundle

```bash
#!/bin/bash
DIAG_DIR="/tmp/erlmcp_diag_$(date +%s)"
mkdir -p $DIAG_DIR

# Collect logs
cp logs/erlmcp.log $DIAG_DIR/

# Collect config
cp config/sys.config $DIAG_DIR/

# Collect crash dump if exists
if [ -f erl_crash.dump ]; then
  cp erl_crash.dump $DIAG_DIR/
fi

# Collect metrics
curl http://localhost:9090/metrics > $DIAG_DIR/metrics.txt

# Compress
tar -czf /tmp/erlmcp_diag_$(date +%s).tar.gz $DIAG_DIR/
echo "Diagnostic bundle: /tmp/erlmcp_diag_*.tar.gz"
```

---

## ALERT INTERPRETATION

| Alert | Meaning | Action |
|-------|---------|--------|
| `erlmcp_process_health = 0` | Process crashed | Restart process |
| `erlmcp_memory_bytes > 2GB` | Memory leak | Check logs, restart |
| `erlmcp_queue_depth > 1000` | System overloaded | Check client load |
| `erlmcp_error_rate > 0.05` | > 5% errors | Check logs, investigate |
| `erlmcp_connections > 10000` | High connection count | May need scaling |

---

## ESCALATION PATH

1. **First Alert** (T+0)
   - On-call engineer investigates
   - Check health dashboard
   - Review recent logs

2. **No Recovery (T+5 min)**
   - Engage secondary on-call
   - Check replica health
   - Consider failover

3. **Still Down (T+15 min)**
   - Declare SEV-1 incident
   - Activate incident commander
   - Begin data restore if needed
   - Notify stakeholders

4. **Escalation (T+30 min)**
   - Contact database team
   - Review backup status
   - Plan recovery strategy
   - Estimate RTO/RPO

---

## METRICS & THRESHOLDS

```
NORMAL (Green):
- Response time: < 100ms p99
- Error rate: < 0.1%
- CPU: < 70%
- Memory: < 500MB
- Connections: < 1000

WARNING (Yellow):
- Response time: 100-500ms p99
- Error rate: 0.1-5%
- CPU: 70-85%
- Memory: 500MB-1GB
- Connections: 1000-5000

CRITICAL (Red):
- Response time: > 500ms p99
- Error rate: > 5%
- CPU: > 85%
- Memory: > 1GB
- Connections: > 5000
```

---

## CONTACT MATRIX

```
PROBLEM                    WHO TO CONTACT
─────────────────────────────────────────────
Application won't start    → DevOps Lead
High error rate (5%+)      → Duty Engineer
Memory leak detected       → Erlang Expert
Connection spike           → Infrastructure
Backup restore fails       → Database Team
Network unreachable        → Network Team
Replica sync issue         → Distributed Systems
Data corruption detected   → Database Team + Manager
```

---

## CRITICAL PROCEDURES (MEMORIZE)

### PROCEDURE A: Quick Restart

```bash
cd /Users/sac/erlmcp
pkill beam
sleep 2
make console
curl http://localhost:8080/health
```

### PROCEDURE B: Failover to Replica

```erlang
% SSH to replica host
erlmcp_enterprise_session_replication:trigger_failover().
% Update load balancer
% Notify team
```

### PROCEDURE C: Restore Data

```bash
pkill beam
cd /Users/sac/erlmcp
tar -xzf priv/tcps/backups/latest.tar.gz -C .
make console
tcps_persistence:verify_integrity().
```

---

## RUNBOOK VERSION

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-27 | Initial version |
| - | - | - |

**NEXT UPDATE**: 2026-04-27

---

**KEEP THIS DOCUMENT UPDATED AS PROCEDURES CHANGE**

**Last Updated**: 2026-01-27 by Agent 15 (DR Specialist)
