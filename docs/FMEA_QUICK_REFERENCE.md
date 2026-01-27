# erlmcp FMEA Quick Reference Guide

**For Operators, SREs, and On-Call Engineers**

Quick reference cards for the top failure modes and how to detect/respond to them.

---

## Table of Contents
1. [Critical Alerts](#critical-alerts)
2. [Failure Mode Cards](#failure-mode-cards)
3. [Diagnostic Checklist](#diagnostic-checklist)
4. [Recovery Procedures](#recovery-procedures)
5. [Monitoring Dashboard Setup](#monitoring-dashboard-setup)

---

## Critical Alerts

Set these up immediately in your monitoring system.

### Alert 1: Memory Usage Exceeds 80%
```
Alert Name: erlmcp_memory_critical
Condition: erlang:memory(total) > 0.80 * max_memory
Duration: 1 minute
Action: Page on-call, trigger logs collection
```

**What It Means:**
- Process memory approaching limit
- Risk of OOM crash in next 10-30 minutes
- GC pauses likely increasing

**Immediate Response:**
1. Check current memory breakdown: `erlang:memory()`
2. Identify top memory consumers: `recon:memory(top, 5)`
3. Kill idle connections if necessary
4. Plan graceful shutdown if memory > 90%

---

### Alert 2: Gen_server Queue Depth > 1000
```
Alert Name: erlmcp_queue_overflow_risk
Condition: gen_server queue depth > 1000
Duration: 30 seconds
Action: Page on-call, trigger logs
```

**What It Means:**
- Requests accumulating faster than processing
- Risk of timeouts and cascading failures
- Backpressure mechanism may have failed

**Immediate Response:**
1. Check load: `recon:info(node, connections)`
2. Kill lowest-priority clients if necessary
3. Check if a handler is stuck: `recon:info(erlmcp_server, status)`
4. Consider graceful overload shedding

---

### Alert 3: GC Pause > 50ms
```
Alert Name: erlmcp_gc_pause_high
Condition: max_gc_pause > 50ms
Duration: Immediate
Action: Alert (non-paging initially)
```

**What It Means:**
- Single GC pause exceeded 50ms
- Service became unresponsive for 50ms+
- May have caused request timeouts

**Immediate Response:**
1. Check GC statistics: `erlang:statistics(garbage_collection)`
2. Collect heap dump if pause > 100ms
3. Plan heap tuning if sustained

---

### Alert 4: File Descriptors > 80% of Ulimit
```
Alert Name: erlmcp_fd_exhaustion_warning
Condition: open_fds > 0.80 * ulimit
Duration: 5 minutes
Action: Alert, may need to reduce connections
```

**What It Means:**
- System approaching FD limit
- New connections will be rejected soon
- May affect other services on system

**Immediate Response:**
1. Check FD usage: `cat /proc/self/fd | wc -l`
2. Check ulimit: `ulimit -n`
3. Kill idle connections or graceful shutdown if > 90%
4. Alert ops to scale up or increase ulimit

---

### Alert 5: Connection Timeout Rate > 5%
```
Alert Name: erlmcp_timeout_spike
Condition: timeout_errors / total_requests > 0.05
Duration: 2 minutes
Action: Page on-call
```

**What It Means:**
- More than 5% of requests are timing out
- Network issues OR service is overloaded
- Likely cascading failures starting

**Immediate Response:**
1. Check network: `ping -c 10 <server>`
2. Check latency: `mtr --report <server>`
3. Check CPU: `top -b -n 1 | head -20`
4. Check memory/queues (see above)
5. If network issue: wait for recovery
6. If service issue: scale up or restart

---

## Failure Mode Cards

One-page reference for each critical failure mode.

### Card: Memory Exhaustion
**RPN: 288 | Category: Resource | Status: HIGH RISK**

**Symptoms:**
- [ ] Memory usage growing steadily
- [ ] GC pauses increasing (> 20ms)
- [ ] Service latency increasing
- [ ] OOM crash (last resort)

**Detection:**
```erlang
% In Erlang shell
erlang:memory(total).              % Total memory
erlang:statistics(garbage_collection).  % GC stats
recon:memory(top, 10).             % Top 10 memory consumers
```

**Root Cause Analysis:**
| Cause | Check | Fix |
|-------|-------|-----|
| Unbounded queue | `process_info(Pid, messages_queue_len)` | Circuit breaker |
| ETS table leak | `ets:info(TableId, size)` | Add size limits |
| Crash dump accumulation | `ls -lh erl_crash.dump*` | Cleanup logs |
| Connection leak | `process_info(erlmcp_server, message_queue_len)` | Drain connections |

**Recovery Steps:**
1. Alert: Page on-call
2. Collect: Heap dump, memory stats, process list
3. Identify: Top 3 memory consumers
4. Mitigate: Kill idle clients or drain connections
5. If > 90%: Graceful shutdown, restart
6. Post-mortem: Analyze heap dump

**Prevention:**
- [ ] Monitor memory continuously
- [ ] Set alerts at 70%, 85%, 95%
- [ ] Implement memory limits per component
- [ ] Regular memory profiling

---

### Card: Message Queue Overflow
**RPN: 224 | Category: Resource | Status: HIGH RISK**

**Symptoms:**
- [ ] Queue depth growing (> 500)
- [ ] Requests timing out
- [ ] Memory usage spiking
- [ ] Latency increases dramatically

**Detection:**
```erlang
% Check queue depth
Process = erlang:whereis(erlmcp_server),
{message_queue_len, Len} = process_info(Process, message_queue_len).

% Alternative: metrics
gen_server_metrics:get_queue_depth(Server).
```

**Root Cause Analysis:**
| Cause | Check | Fix |
|-------|-------|-----|
| Handler slow | `recon:info(handler, time_in_handler)` | Optimize handler |
| Transport full | `inet:getstat(Socket)` | Check network |
| Too many clients | `process_count()` | Rate limit clients |
| Message bomb | `inet:peername(Socket)` | Block attacker |

**Recovery Steps:**
1. Alert: Page on-call
2. Identify: Which process has full queue?
3. Analyze: What messages are queued?
4. Mitigate: Drop lowest-priority requests
5. Rebalance: Scale up or redirect traffic
6. Post-mortem: Implement circuit breaker

**Prevention:**
- [ ] Monitor queue depth per process
- [ ] Implement circuit breaker at threshold
- [ ] Rate limit incoming traffic
- [ ] Implement request prioritization

---

### Card: Connection Timeout
**RPN: 245 | Category: Network | Status: HIGH RISK**

**Symptoms:**
- [ ] Clients timing out (5s delays)
- [ ] Retries visible in logs
- [ ] Network latency visible
- [ ] Slow recovery after transient issue

**Detection:**
```erlang
% Check timeout rate
RequestCount = get_request_count(),
TimeoutCount = get_timeout_count(),
TimeoutRate = TimeoutCount / RequestCount.
% Alert if > 0.05 (5%)

% Check network latency
rpc:call(Node, inet, getstat, []).
```

**Root Cause Analysis:**
| Cause | Check | Fix |
|-------|-------|-----|
| Network latency | `ping` / `mtr` | Wait or reroute |
| DNS slow | `nslookup` / `dig` | Check DNS server |
| Service slow | Response time histogram | Scale up service |
| GC pause | `erlang:statistics(garbage_collection)` | Tune GC |

**Recovery Steps:**
1. Alert: Monitor for 2 minutes before paging
2. Diagnose: Is it network or service?
3. If network: Monitor for recovery
4. If service: Check CPU, memory, queues
5. Mitigate: Implement exponential backoff
6. Post-mortem: Add SLO tracking

**Prevention:**
- [ ] Implement exponential backoff with jitter
- [ ] Add timeout SLO tracking
- [ ] Monitor per-client timeout rate
- [ ] Add circuit breaker for persistent timeouts

---

### Card: GC Pause Exceeds 100ms
**RPN: 252 | Category: Resource | Status: HIGH RISK**

**Symptoms:**
- [ ] Sudden latency spike (100ms+)
- [ ] Multiple requests timeout simultaneously
- [ ] Memory was just low, now lower
- [ ] Pattern visible in latency histogram

**Detection:**
```erlang
% Monitor GC pauses
{_, GCTime} = erlang:statistics(garbage_collection),
MaxPause = get_max_gc_pause().
% Alert if > 100ms

% Check GC statistics
erlang:statistics(garbage_collection).
```

**Root Cause Analysis:**
| Cause | Check | Fix |
|-------|-------|-----|
| Too many objects | `erlang:memory(processes)` | Reduce memory |
| Heap size wrong | `erlang:system_info(heap_type)` | Tune heap |
| GC priority low | `erlang:system_info(gc_behavior)` | Check vm.args |
| Fragmentation | `erlang:garbage_collect()` | Force GC |

**Recovery Steps:**
1. Alert: Non-critical (log and monitor)
2. Collect: GC stats, heap dump, memory breakdown
3. Analyze: Is pause growing or one-time?
4. If one-time: Monitor and document
5. If growing: Collect heap dump and analyze
6. Tune: GC parameters in vm.args

**Prevention:**
- [ ] Monitor GC pause times continuously
- [ ] Export metrics to observability
- [ ] Tune vm.args for low-latency
- [ ] Regular memory profiling and optimization

---

## Diagnostic Checklist

When erlmcp is having problems, follow this systematic approach.

### Step 1: Determine Service Status (30 seconds)
```bash
# Is the service responding?
curl http://localhost:8080/health

# Check process
ps aux | grep erlmcp

# Check logs for errors
tail -50 /var/log/erlmcp/erlmcp.log | grep -i error
```

**Next Step:**
- Service responding → Go to Step 2 (performance issue)
- Service not responding → Go to Step 3 (service down)

---

### Step 2: Performance Issue Diagnosis (2 minutes)

```erlang
% SSH to server, start Erlang shell
erl -remsh erlmcp@localhost

% Check top resource consumers
recon:top(keys, 3).

% Memory breakdown
erlang:memory().

% Process info
recon:proc_count(memory, 3).

% Queue depths
[{Pid, Len} || {Pid, {message_queue_len, Len}} <-
    [{P, process_info(P, message_queue_len)} || P <- processes()],
 Len > 100].
```

**Likely Causes:**
| Symptom | Check | Action |
|---------|-------|--------|
| Memory high | `erlang:memory()` | Drain connections, analyze heap dump |
| Specific process memory high | `recon:proc_count(memory,3)` | Kill stuck process or handler |
| Queue lengths high | Queue depth check | Identify slow handler |
| Latency high | `get_histogram(latency)` | Check network or handler time |
| CPU high | `top` | Check for infinite loop or lock contention |

---

### Step 3: Service Down Diagnosis (5 minutes)

```bash
# Check if process exists
ps aux | grep erl

# Check logs
tail -100 /var/log/erlmcp/erlmcp.log

# Check for crash dump
ls -la erl_crash.dump*

# Check port
netstat -an | grep 8080

# Try to start
/opt/erlmcp/bin/erlmcp start
```

**Common Issues:**
| Symptom | Cause | Fix |
|---------|-------|-----|
| No process | Crashed | Check erl_crash.dump, restart |
| Port in use | Not shut down cleanly | `lsof -i :8080`, kill process |
| Error in logs | Config issue | Check sys.config, fix issue |
| Out of memory | Memory leak | Analyze heap dump, patch |

---

## Recovery Procedures

Step-by-step procedures for common failure scenarios.

### Procedure: Graceful Shutdown (No Data Loss)

**When to Use:** Memory/FD exhaustion, planned shutdown

```bash
# Step 1: Drain connections gracefully
# (This will take 30-60 seconds)
erlcmd='erlmcp_sup:drain_connections().'
erl -remsh erlmcp@localhost -noshell -eval "$erlcmd" -s erlang halt

# Step 2: Stop service
/opt/erlmcp/bin/erlmcp stop

# Step 3: Restart
/opt/erlmcp/bin/erlmcp start

# Step 4: Verify health
curl http://localhost:8080/health
```

**Verification:**
- [ ] Service starts successfully
- [ ] Health endpoint responds
- [ ] Clients reconnect successfully
- [ ] No data loss observed

---

### Procedure: Force Shutdown (Emergency)

**When to Use:** Service hung, unresponsive

```bash
# Step 1: Kill immediately
pkill -9 beam.smp

# Step 2: Clean up
rm -f /var/run/erlmcp.pid

# Step 3: Restart
/opt/erlmcp/bin/erlmcp start

# Step 4: Monitor recovery
tail -f /var/log/erlmcp/erlmcp.log
```

**Risks:**
- In-flight messages lost
- Connections aborted
- Potential data loss

**Recovery:**
- Clients auto-reconnect
- Messages need to be retried
- Check logs for errors

---

### Procedure: Load Shedding (High Load)

**When to Use:** Queue overflow, too many requests

```bash
# Option 1: Reduce connection limit temporarily
erl -remsh erlmcp@localhost -noshell \
  -eval 'erlmcp_connection_limits:set_max_connections(500).' \
  -s erlang halt

# Option 2: Kill idle connections
erl -remsh erlmcp@localhost -noshell \
  -eval 'erlmcp_client:kill_idle(idle_timeout_ms(60000)).' \
  -s erlang halt

# Option 3: Enable circuit breaker
erl -remsh erlmcp@localhost -noshell \
  -eval 'erlmcp_circuit_breaker:enable(auto).' \
  -s erlang halt
```

**Expected Behavior:**
- New connections rejected with "service busy"
- Existing connections continue
- Service recovers as load decreases
- Clients retry with backoff

---

### Procedure: Memory Leak Investigation

**When to Use:** Memory growing despite stable load

```bash
# Step 1: Collect baseline
Time1=$(date +%s)
Mem1=$(erl -noshell -eval 'io:format("~p~n", [erlang:memory(total)]).' | grep -oE '[0-9]+')

# Step 2: Wait and collect again
sleep 300

Time2=$(date +%s)
Mem2=$(erl -noshell -eval 'io:format("~p~n", [erlang:memory(total)]).' | grep -oE '[0-9]+')

# Step 3: Calculate leak rate
MemGrowth=$((Mem2 - Mem1))
TimeElapsed=$((Time2 - Time1))
LeakRate=$((MemGrowth / TimeElapsed))

echo "Memory growth: $MemGrowth bytes in $TimeElapsed seconds"
echo "Leak rate: $LeakRate bytes/second"

# Step 4: If leak confirmed, collect heap dump
erl -remsh erlmcp@localhost -noshell \
  -eval 'erlang:dump_to_file("/tmp/erlmcp_heapdump.bin"), halt().' &

# Step 5: Analyze with recon or other tools
recon:memory(allocated, 10).
```

**Analysis Tools:**
- `recon:memory()` - High-level overview
- `recon:proc_count(memory, 10)` - Top memory processes
- Heap dump analysis - Deep inspection

---

## Monitoring Dashboard Setup

### Essential Metrics to Collect

1. **System Metrics**
   - Memory usage (total, processes, binaries, atoms)
   - CPU usage
   - File descriptors
   - Network throughput (bytes in/out)

2. **erlmcp Metrics**
   - Active connections
   - Requests per second
   - Request latency (p50, p95, p99)
   - Error rate
   - Queue depths per process
   - GC pause times
   - Timeout rate

3. **Resource Metrics**
   - Connection pool utilization
   - ETS table sizes
   - Message queue lengths
   - Supervisor restart count

### Prometheus Metrics (Recommended)

```
# Memory
erlmcp_memory_total bytes
erlmcp_memory_processes bytes
erlmcp_memory_binaries bytes

# Connections
erlmcp_active_connections count
erlmcp_connection_limit count

# Requests
erlmcp_requests_total counter
erlmcp_requests_errors_total counter
erlmcp_request_duration_seconds histogram
erlmcp_request_timeout_total counter

# Resources
erlmcp_queue_depth_mean gauge
erlmcp_queue_depth_max gauge
erlmcp_gc_pause_max_ms gauge

# System
erlmcp_file_descriptors_used gauge
erlmcp_file_descriptors_max gauge
```

### Alert Rules (Prometheus)

```yaml
groups:
- name: erlmcp_alerts
  interval: 30s
  rules:
  - alert: MemoryHighCritical
    expr: erlmcp_memory_total > 0.9 * max_memory
    for: 1m
    action: page

  - alert: QueueOverflow
    expr: erlmcp_queue_depth_max > 1000
    for: 30s
    action: page

  - alert: TimeoutSpike
    expr: rate(erlmcp_request_timeout_total[5m]) > 0.05
    for: 2m
    action: page

  - alert: FileDescriptorsHigh
    expr: erlmcp_file_descriptors_used > 0.8 * erlmcp_file_descriptors_max
    for: 5m
    action: alert

  - alert: GCPauseHigh
    expr: erlmcp_gc_pause_max_ms > 50
    for: 1m
    action: alert
```

### Grafana Dashboard

**Key Panels:**
1. Memory overview (stacked area: total, processes, binaries, atoms)
2. Connection count (time series)
3. Request latency (p50, p95, p99)
4. Error rate (time series)
5. Queue depth (time series)
6. GC pauses (time series)
7. File descriptors (gauge)
8. Timeout rate (time series)

---

## Frequently Asked Questions

### Q: What does "Circuit breaker open" mean?
**A:** A subsystem (like a handler or external service) has failed repeatedly. Further requests are immediately rejected to prevent cascading failures. The circuit will "half-open" after a timeout (typically 30 seconds) to test recovery.

### Q: How long until the service recovers after a crash?
**A:** Typical recovery time is 5-10 seconds:
- Service starts: 2-3 seconds
- Clients detect and reconnect: 2-3 seconds
- Full operational capacity: 10-15 seconds

### Q: Is my data safe if erlmcp crashes?
**A:** Session data is stored in ETS and survives process crashes. However, in-flight messages (being processed when crash occurs) may be lost. Clients must retry. Configure session replication for additional safety.

### Q: What's the maximum number of concurrent connections?
**A:** Depends on system resources:
- Default: Limited to ~1024 connections (ulimit -n)
- Production: Scale to 15K+ with proper tuning
- See `/Users/sac/erlmcp/docs/PRODUCTION_LAUNCH_CHECKLIST.md`

### Q: How do I scale erlmcp to handle more load?
**A:** Options:
1. **Vertical:** Increase VM resources (memory, CPU, FDs)
2. **Horizontal:** Run multiple erlmcp instances behind load balancer
3. **Tuning:** Optimize GC, connection pooling, handler performance

### Q: Where are the logs?
**A:** Default locations:
- Error log: `/var/log/erlmcp/erlmcp.log`
- Crash dump: `./erl_crash.dump`
- Sasl log: `/var/log/erlmcp/sasl/`

---

## Emergency Contacts

**For Critical Incidents:**
- Page on-call engineer (PagerDuty)
- Alert Slack #erlmcp-incidents
- Contact platform team lead

**For Non-Critical Issues:**
- File GitHub issue
- Post to #erlmcp-help Slack
- Email erlmcp-team@company.com

---

**Last Updated:** 2026-01-27
**FMEA Version:** 1.0
**For questions, see:** `/Users/sac/erlmcp/docs/FMEA_FAILURE_MODE_ANALYSIS.md`
