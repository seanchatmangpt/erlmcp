# TCPS Advanced Troubleshooting Course

**Duration:** 1 day (8 hours)
**Prerequisites:** TCPS Certified Operator
**Target:** Operators with 1+ months experience

---

## Table of Contents

1. [Advanced Diagnostic Techniques](#1-advanced-diagnostic-techniques)
2. [Complex Failure Scenarios](#2-complex-failure-scenarios)
3. [Performance Optimization](#3-performance-optimization)
4. [Disaster Recovery Drills](#4-disaster-recovery-drills)

---

## 1. Advanced Diagnostic Techniques

### 1.1 Log Analysis Mastery

**Learning Objective:** Master log analysis for rapid problem diagnosis

#### Erlang Crash Dumps

```bash
# Find recent crash dumps
ls -lt _build/default/rel/erlmcp/log/*.dump | head -5

# Analyze crash dump
erl -noshell -eval 'crashdump_viewer:start().' -s init stop

# Or use command-line
grep -A20 "=proc:" _build/default/rel/erlmcp/log/erl_crash.dump

# Key sections to check:
# - =proc: - Process information
# - =atoms: - Atom table (memory leak indicator)
# - =memory: - Memory usage
# - =ets: - ETS table info
```

#### Structured Log Analysis

```bash
# Find Andon trigger patterns
grep "ANDON TRIGGERED" priv/tcps/logs/andon_events.log \
  | awk -F'type=' '{print $2}' \
  | awk '{print $1}' \
  | sort | uniq -c | sort -nr

# Output:
#  45 test_failure
#  28 non_determinism
#  18 shacl_violation
#   9 compilation_failure

# Find slowest quality gate stages
grep "stage_duration" priv/tcps/logs/quality_gates.log \
  | awk '{print $3, $5}' \
  | sort -k2 -nr | head -10

# Find work orders stuck >24h
grep "status=in_progress" priv/tcps/logs/work_orders.log \
  | awk '{print $2, $4}' \
  | while read wo_id timestamp; do
      age=$(($(date +%s) - $(date -d "$timestamp" +%s)))
      if [ $age -gt 86400 ]; then
        echo "$wo_id: $((age / 3600))h"
      fi
    done
```

#### Real-Time Log Monitoring

```bash
# Multi-tail all TCPS logs
multitail \
  _build/default/rel/erlmcp/log/erlang.log.1 \
  priv/tcps/logs/andon_events.log \
  priv/tcps/logs/quality_gates.log \
  priv/tcps/logs/work_orders.log

# Watch for error patterns
tail -f _build/default/rel/erlmcp/log/erlang.log.1 \
  | grep --line-buffered -E "(error|ERROR|crash|CRASH)" \
  | while read line; do
      # Send to alerting system
      echo "$line" | curl -X POST -d @- https://alerts.company.com/tcps
    done
```

---

### 1.2 ETS Table Debugging

**Learning Objective:** Diagnose ETS table issues (memory leaks, corruption)

```erlang
% In Erlang shell
1> % List all ETS tables
1> ets:all().
[tcps_andon_events, tcps_work_orders, tcps_kanban, ...].

2> % Check table info
2> ets:info(tcps_andon_events).
[{type,set},
 {size,127},  % Number of entries
 {memory,15642},  % Words of memory
 {owner,<0.234.0>},
 ...].

3> % Find large tables (potential memory leaks)
3> lists:sort([{ets:info(T, memory), T} || T <- ets:all()]).
[{15642, tcps_andon_events},
 {45123, tcps_work_orders},
 {89456, receipts_cache}].  % ← Large!

4> % Check receipts_cache contents
4> ets:tab2list(receipts_cache).
% If this shows thousands of old entries, it's a memory leak

5> % Fix: Clear old entries
5> Now = erlang:system_time(second).
5> OldEntries = [K || {K, _, Timestamp} <- ets:tab2list(receipts_cache),
                     Now - Timestamp > 3600].
5> [ets:delete(receipts_cache, K) || K <- OldEntries].
```

**Automated ETS Health Check:**

```bash
# Create ETS monitoring script
cat > ~/tcps-ets-monitor.sh << 'EOF'
#!/bin/bash
# Monitor ETS table sizes

erl -noshell -eval '
Lists = [
  {T, ets:info(T, memory), ets:info(T, size)}
  || T <- ets:all()
],
Sorted = lists:reverse(lists:sort(Lists)),
io:format("~nETS Table Memory Usage:~n"),
io:format("~-30s ~10s ~10s~n", ["Table", "Memory (KB)", "Entries"]),
io:format("~s~n", [lists:duplicate(52, $-)]),
[io:format("~-30s ~10w ~10w~n", [T, M div 1024, S]) || {T, M, S} <- Sorted],
halt().
' -noinput

EOF

chmod +x ~/tcps-ets-monitor.sh
~/tcps-ets-monitor.sh
```

---

### 1.3 Distributed Erlang Debugging

**Learning Objective:** Debug multi-node TCPS deployments

```bash
# Connect to remote node
erl -name debug@localhost -setcookie tcps_production -remsh tcps@production

# On remote node:
1> % Check node connections
1> nodes().
[tcps_worker1@prod, tcps_worker2@prod].

2> % Check process registry
2> gproc:lookup_pids({n, l, tcps_work_order}).

3> % Check message queue lengths (slow processing indicator)
3> [{P, erlang:process_info(P, message_queue_len)}
   || P <- erlang:processes(),
      erlang:process_info(P, message_queue_len) > 100].

4> % Trace function calls
4> dbg:tracer().
4> dbg:p(all, c).
4> dbg:tpl(tcps_work_order, process_pull_signal, x).
4> % Now all calls to process_pull_signal will be traced

5> % Stop tracing
5> dbg:stop_clear().
```

---

## 2. Complex Failure Scenarios

### 2.1 Scenario: Cascading Failures

**Setup:**
```
Event: Database connection pool exhausted
↓
Effect 1: SPARQL queries timeout
↓
Effect 2: Receipt verification fails
↓
Effect 3: Multiple Andons triggered
↓
Effect 4: Pipeline completely blocked
```

**Diagnosis Process:**

```bash
# Step 1: Identify symptom (many Andons)
./tools/tcps andon list --status=open
# Output: 45 open Andons (all type=missing_receipt)

# Step 2: Pattern analysis
./tools/tcps root-cause analyze-patterns
# Output: 100% are "receipt verification timeout"

# Step 3: Check SPARQL query performance
./tools/tcps tpm profile-sparql
# Output: Average query time: 45s (should be <1s)

# Step 4: Check database connections
erl -noshell -eval '
  PoolStatus = poolboy:status(http_pool),
  io:format("Pool: ~p~n", [PoolStatus]),
  halt().
' -noinput

# Output:
# {status, full, waiting: 127}  ← All connections in use, 127 waiting!

# Step 5: Identify root cause
# - Connection pool too small for load
# - OR database responding slowly
# OR connection leak

# Check for connection leaks:
lsof -i TCP | grep <db-port> | wc -l
# If this shows hundreds of connections, it's a leak

# Step 6: Emergency fix
# Restart application to clear leaked connections
./scripts/deploy.sh restart

# Step 7: Temporary mitigation
# Increase pool size (until permanent fix)
# Edit config/sys.config:
{poolboy, [{http_pool, [{size, 100}, {max_overflow, 50}]}]}.

# Step 8: Permanent fix
# - Investigate code for connection leaks
# - Add connection timeout guards
# - Implement connection recycling

# Step 9: Create master Andon resolution
./tools/tcps root-cause start EMERGENCY-CASCADE-001
# (Perform 5 Whys)
./tools/tcps root-cause finalize ... \
  --root-cause="Database connection pool exhausted due to leak" \
  --prevention="Add connection leak detection + increase pool size"

# Step 10: Resolve all Andons with master receipt
for andon in $(./tools/tcps andon list --status=open --format=ids); do
  ./tools/tcps andon resolve $andon \
    --receipt-path=receipt-EMERGENCY-CASCADE-001.json
done
```

---

### 2.2 Scenario: Split-Brain (Network Partition)

**Setup:**
```
Distributed TCPS cluster: [node1, node2, node3]
↓
Network partition: node1 | node2, node3
↓
Both sides think they are primary
↓
Work orders processed twice
↓
Receipt conflicts
```

**Diagnosis:**

```bash
# Step 1: Detect split-brain
erl -name debug@localhost -setcookie tcps_prod -remsh tcps@node1

1> % Check visible nodes
1> nodes().
[].  % ← Should see node2, node3 but doesn't!

2> % Check if node2 sees node1
2> net_adm:ping('tcps@node2').
pang.  % ← Network partition!

# Step 2: Verify conflict
./tools/tcps work-order list --status=completed \
  | sort | uniq -d
# Shows duplicate completions

# Step 3: Resolution
# Designate node1 as authoritative (has most receipts)

# On node2 and node3: Stop processing
./scripts/deploy.sh stop

# Restore network connectivity
# (Fix firewall, routing, etc.)

# Verify connectivity
ping node1.prod.company.com
ping node2.prod.company.com

# Restart node2 and node3
./scripts/deploy.sh start

# Step 4: Reconcile conflicts
./tools/tcps tpm reconcile-receipts \
  --primary=node1 \
  --secondaries=node2,node3

# This:
# - Identifies conflicting receipts
# - Keeps primary (node1) receipts
# - Invalidates secondary receipts
# - Generates conflict resolution receipt

# Step 5: Prevention
# - Implement consensus (Raft/Paxos)
# - Add network partition detection
# - Implement automatic failover with quorum
```

---

### 2.3 Scenario: Ontology Corruption

**Setup:**
```
Bad receipt written to ontology
↓
RDF syntax error
↓
All SPARQL queries fail
↓
Receipt verification broken
↓
Pipeline halted
```

**Diagnosis & Recovery:**

```bash
# Step 1: Detect corruption
./tools/tcps receipt verify-all
# Output: Error parsing ontology at line 4567

# Step 2: Identify corrupt receipt
sed -n '4560,4570p' ontology/receipts.ttl
# Shows malformed RDF triple

# Step 3: Find problematic receipt
grep -B5 -A5 "line 4567" ontology/receipts.ttl \
  | grep "tcps:receipt"
# Shows receipt ID: RCPT-1234567890-123456

# Step 4: Backup ontology
cp ontology/receipts.ttl ontology/receipts.ttl.backup

# Step 5: Remove corrupt entry
# Extract all receipts except corrupt one
./tools/tcps tpm extract-receipts \
  --ontology=ontology/receipts.ttl \
  --exclude=RCPT-1234567890-123456 \
  --output=ontology/receipts.ttl.clean

# Step 6: Validate cleaned ontology
./tools/tcps receipt validate ontology/receipts.ttl.clean

# Step 7: Replace ontology
mv ontology/receipts.ttl.clean ontology/receipts.ttl

# Step 8: Rebuild from JSON (alternative method)
# JSON receipts are source of truth, rebuild RDF
./tools/tcps tpm rebuild-ontology \
  --from=priv/receipts/*.json \
  --output=ontology/receipts.ttl

# Step 9: Verify rebuild
./tools/tcps receipt verify-all

# Step 10: Identify root cause of corruption
./tools/tcps root-cause start ONTOLOGY-CORRUPTION-001
# Why was malformed RDF written?
# - Bug in receipt generation code
# - Special characters not escaped
# - Unicode handling issue
# etc.
```

---

## 3. Performance Optimization

### 3.1 Profiling TCPS Performance

**Using fprof (Function Profiler):**

```erlang
% In Erlang shell
1> % Start profiling
1> fprof:trace([start, {procs, all}]).

2> % Run workload (process 100 work orders)
2> [tcps_work_order:process_next() || _ <- lists:seq(1, 100)].

3> % Stop profiling
3> fprof:trace(stop).

4> % Analyze results
4> fprof:profile().
4> fprof:analyse([{dest, "fprof_analysis.txt"}]).

5> % View hot spots
5> fprof:analyse([{sort, acc}, {totals, true}]).

% Output shows:
% tcps_receipt_verifier:verify_chain: 45% of time
% tcps_kanban:check_wip_limit: 23% of time
% tcps_work_order:calculate_priority: 15% of time
```

**Optimizations Based on Profiling:**

```erlang
% BEFORE (45% of time)
verify_chain(SkuId) ->
    % Slow: Queries ontology for each receipt
    Receipts = query_all_receipts(SkuId),
    [verify_receipt(R) || R <- Receipts].

% AFTER (10% of time)
verify_chain(SkuId) ->
    % Fast: Single query with all receipts, cached
    case get_cached_receipts(SkuId) of
        {ok, Receipts} ->
            [verify_receipt(R) || R <- Receipts];
        undefined ->
            Receipts = query_all_receipts(SkuId),
            cache_receipts(SkuId, Receipts),
            [verify_receipt(R) || R <- Receipts]
    end.
```

---

### 3.2 Memory Optimization

**Detecting Memory Leaks:**

```bash
# Monitor memory usage over time
while true; do
  erl -noshell -eval '
    Memory = erlang:memory(),
    Total = proplists:get_value(total, Memory),
    Processes = proplists:get_value(processes, Memory),
    ETS = proplists:get_value(ets, Memory),
    io:format("~s Total: ~wMB, Procs: ~wMB, ETS: ~wMB~n",
              [calendar:system_time_to_rfc3339(erlang:system_time(second)),
               Total div 1048576,
               Processes div 1048576,
               ETS div 1048576]),
    halt().
  ' -noinput
  sleep 60
done > memory_usage.log

# Analyze trend
grep "Total:" memory_usage.log \
  | awk '{print $2, $4}' \
  | gnuplot -e 'set terminal dumb; plot "-" using 0:2 with lines'

# If memory continuously increasing: MEMORY LEAK
```

**Finding Memory Leak Source:**

```erlang
% In Erlang shell
1> % Take heap snapshot
1> instrument:allocations().

2> % Run workload
2> [tcps_work_order:create(...) || _ <- lists:seq(1, 1000)].

3> % Take another snapshot
3> instrument:allocations().

4> % Compare snapshots
4> % Look for processes with growing heaps

5> % Inspect suspicious process
5> ProcessInfo = erlang:process_info(Pid, [heap_size, message_queue_len, memory]).

6> % Check for message queue buildup
6> {message_queue_len, QueueLen} = lists:keyfind(message_queue_len, 1, ProcessInfo).
6> % If QueueLen > 1000, messages not being processed fast enough

7> % Check for leaked ETS entries
7> [ets:info(T, size) || T <- ets:all()].
7> % If sizes continuously grow, ETS leak
```

---

## 4. Disaster Recovery Drills

### 4.1 Full System Failure Drill

**Objective:** Restore TCPS from complete failure in <30 minutes

**Scenario:**
- All servers down
- No running processes
- Must restore from last backup

**Drill Procedure:**

```bash
# Timer starts NOW

# Step 1: Verify backup availability (2 min)
ls -lh /backups/tcps/
# Identify latest: tcps-20260125.tar.gz

# Step 2: Deploy TCPS from scratch (5 min)
cd /opt/erlmcp
git pull origin main
make workspace-build
make workspace-release

# Step 3: Restore from backup (10 min)
./tools/tcps tpm restore --from=/backups/tcps/tcps-20260125.tar.gz

# Step 4: Verify integrity (5 min)
./tools/tcps receipt verify-all
./tools/tcps tpm health

# Step 5: Start services (3 min)
./scripts/deploy.sh start
./scripts/start_dashboard.sh

# Step 6: Verify operational (2 min)
./tools/tcps andon list
./tools/tcps work-order list
curl http://localhost:8080/api/health

# Step 7: Resume operations (3 min)
# Re-sync work orders since backup
./tools/tcps work-order sync-github \
  --since=$(date -d '1 day ago' +%Y-%m-%d)

# Timer stops

# Success criteria: <30 minutes, all checks pass
```

---

### 4.2 Data Corruption Drill

**Objective:** Recover from partial data corruption

**Scenario:**
- 500 receipts corrupted (random byte flips)
- Must identify and recover

**Drill Procedure:**

```bash
# Step 1: Detect corruption (automated)
./tools/tcps receipt verify-all 2>&1 | tee verify-results.log

# Output shows:
# ✗ receipt-xxx.json: Invalid JSON
# ✗ receipt-yyy.json: Hash mismatch
# ... (500 errors)

# Step 2: Extract corrupt receipt IDs
grep "✗" verify-results.log | awk '{print $2}' > corrupt-receipts.txt

# Step 3: Check if JSON receipts are backed up
ls /backups/tcps/*.tar.gz | tail -1
# Latest: tcps-20260125.tar.gz

# Step 4: Extract receipts from backup
mkdir /tmp/tcps-recovery
tar -xzf /backups/tcps/tcps-20260125.tar.gz \
  -C /tmp/tcps-recovery \
  receipts/

# Step 5: Restore corrupt receipts
while read receipt_id; do
  cp /tmp/tcps-recovery/receipts/${receipt_id}.json \
     priv/receipts/${receipt_id}.json
done < corrupt-receipts.txt

# Step 6: Rebuild ontology from restored receipts
./tools/tcps tpm rebuild-ontology

# Step 7: Verify recovery
./tools/tcps receipt verify-all

# Should show: ✓ All receipts valid

# Step 8: Document incident
./tools/tcps andon trigger \
  --type=missing_receipt \
  --sku=system \
  --stage=validation \
  --message="Recovered from 500 corrupted receipts"

./tools/tcps root-cause start ...
# Perform 5 Whys to identify corruption cause
```

---

## Advanced Troubleshooting Certification

### Practical Exam (2 hours)

**You will be given:**
1. A broken TCPS system with multiple issues
2. Limited information about the problems
3. Access to all tools and logs

**You must:**
1. Diagnose all issues using logs and tools
2. Prioritize fixes by impact
3. Implement fixes with minimal downtime
4. Document root causes and prevention measures
5. Create receipts for all actions taken

**Pass Criteria:**
- All critical issues resolved
- System fully operational
- Proper documentation provided
- Time: <2 hours

---

## Resources

### Tools Developed in This Course

```bash
# ETS monitor
~/tcps-ets-monitor.sh

# Memory trend analyzer
~/tcps-memory-trend.sh

# Log pattern analyzer
~/tcps-log-analyzer.sh

# Backup verification
~/tcps-verify-backup.sh
```

### Further Reading

- [Erlang Crash Dump Analysis](https://erlang.org/doc/apps/erts/crash_dump.html)
- [Performance Profiling with fprof](https://erlang.org/doc/man/fprof.html)
- [Distributed Erlang](https://erlang.org/doc/reference_manual/distributed.html)
- [ETS Table Tuning](https://erlang.org/doc/man/ets.html)

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-26
**Maintained By:** TCPS Advanced Training Team
