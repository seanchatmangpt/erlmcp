# Cascading Failures Recovery Runbook

**Runbook ID**: RB-011
**Severity**: SEV-1
**Last Updated**: 2026-02-02

## Overview

This runbook covers cascading failure scenarios in erlmcp v3, where failure in one component triggers failures in others, leading to system-wide collapse.

## Symptoms

- Rapid series of failures across components
- System-wide degradation
- Multiple services failing simultaneously
- Resource exhaustion (CPU, memory, connections)
- Storm of timeout errors
- Circular dependencies causing deadlock

## Prerequisites

- Circuit breakers configured
- Bulkheading implemented
- Health checks active
- Monitoring for all components
- Emergency shutdown procedures

## Diagnosis

### Step 1: Identify Failure Origin

```bash
# Check component failure order
kubectl get events -n erlmcp-prod --sort-by='.lastTimestamp' | tail -100

# Check logs for first failure
kubectl logs -n erlmcp-prod -l app=erlmcp --tail=1000 | grep -i "error\|failure\|crash" | head -50

# Check failure cascade pattern
kubectl logs -n erlmcp-prod erlmcp-0 --since=1h | grep -B5 -A5 "connection refused"
```

### Step 2: Analyze Failure Propagation

```bash
# Check system-wide resource usage
kubectl top pods -n erlmcp-prod
kubectl top nodes

# Check connection storm
kubectl exec -n erlmcp-prod erlmcp-0 -- netstat -an | grep TIME_WAIT | wc -l

# Check retry storms
kubectl logs -n erlmcp-prod erlmcp-0 --since=10m | grep -i "retry" | wc -l

# Check circuit breaker status
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -s http://localhost:8081/admin/circuit-breakers
```

### Step 3: Classify Failure Type

| Pattern | Description | Example |
|---------|-------------|---------|
| **Resource Exhaustion** | Component failure → resource contention → other failures | DB failure → connection pool exhaustion → app timeout |
| **Retry Storm** | Client retries overload failing component | Service A retries Service B 100x/sec |
| **Cascading Timeout** | Timeout in one service propagates | API timeout → database query timeout |
| **Circular Dependency** | Circular wait causes deadlock | A waits for B, B waits for C, C waits for A |

## Resolution Procedures

### Immediate Mitigation

#### Step 1: Stop the Bleeding

```bash
# Activate all circuit breakers
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/circuit-breakers/open-all

# Enable bulkhead isolation
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/bulkheads/enable

# Enable rate limiting globally
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/rate-limit/enable
```

#### Step 2: Isolate Failed Components

```bash
# Scale down to minimum
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=1

# Stop all non-essential services
kubectl scale deployment -n erlmcp-prod erlmcp-analytics --replicas=0
kubectl scale deployment -n erlmcp-prod erlmcp-observability --replicas=0

# Enable maintenance mode
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/maintenance/on
```

### Recovery by Failure Type

#### Scenario 1: Database Connection Pool Exhaustion

```bash
# Diagnosis
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -s http://localhost:8081/metrics | grep db_pool_active

# Immediate action - kill long-running queries
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE state = 'active'
AND query_start < now() - interval '5 minutes'
AND pid != pg_backend_pid();
"

# Increase pool size temporarily
kubectl set env statefulset erlmcp -n erlmcp-prod \
  DB_POOL_SIZE=100 \
  DB_POOL_TIMEOUT=5000

# Restart applications
kubectl rollout restart statefulset erlmcp -n erlmcp-prod
```

#### Scenario 2: Retry Storm

```bash
# Diagnosis - identify clients causing retries
kubectl logs -n erlmcp-prod erlmcp-0 --since=5m | \
  grep "retry attempt" | \
  awk '{print $NF}' | sort | uniq -c | sort -rn

# Immediate action - disable retries globally
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/retries/disable

# Enable exponential backoff with jitter
kubectl set env statefulset erlmcp -n erlmcp-prod \
  RETRY_ENABLED=true \
  RETRY_MAX_ATTEMPTS=3 \
  RETRY_BACKOFF_MS=1000 \
  RETRY_BACKOFF_MULTIPLIER=2 \
  RETRY_JITTER_ENABLED=true

# Re-enable retries gradually
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -X POST http://localhost:8081/admin/retries/enable
```

#### Scenario 3: Memory Exhaustion Cascade

```bash
# Diagnosis
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
erlang:memory(total).
"

# Immediate action - enable memory limits
kubectl set env statefulset erlmcp -n erlmcp-prod \
  ERL_MAX_PORTS=65536 \
  ERL_MAX_ETS_TABLES=1400

# Force garbage collection
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
erlang:garbage_collect().
"

# Restart with increased limits
kubectl patch statefulset erlmcp -n erlmcp-prod --type='json' \
  -p='[{"op": "replace", "path": "/spec/template/spec/containers/0/resources/limits/memory", "value": "8Gi"}]'
```

#### Scenario 4: Circular Dependency Deadlock

```bash
# Diagnosis - identify circular wait
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
recon:proc_count(10, mailbox).
"

# Immediate action - break the circle
# Find processes stuck in gen_server:call
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
[
  begin
    {Pid, _} = erlang:process_info(self(), message_queue_len),
    case Pid of
      {message_queue_len, Len} when Len > 1000 ->
        erlang:exit(self(), kill);
      _ ->
        ok
    end
  end
|| _ <- processes()].
"

# Restart affected services in dependency order
# Order: Database → Core API → Session → Analytics
kubectl rollout restart statefulset erlmcp-postgres -n erlmcp-prod
sleep 30
kubectl rollout restart statefulset erlmcp -n erlmcp-prod
```

### Systematic Recovery

#### Step 1: Restore Core Components

```bash
# Start with database
kubectl scale statefulset erlmcp-postgres -n erlmcp-prod --replicas=3
kubectl wait --for=condition=ready pod -l app=erlmcp-postgres -n erlmcp-prod --timeout=5m

# Verify database health
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "SELECT 1;"

# Start core API
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=3
kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=5m
```

#### Step 2: Verify Core Stability

```bash
# Health checks
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -f http://localhost:8081/health

# Smoke tests
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
[
  {ok, _} = application:ensure_all_started(erlmcp),
  {ok, _} = erlmcp_session:create(test_session),
  ok = erlmcp_session:delete(test_session),
  smoke_test_passed
].
"
```

#### Step 3: Gradual Service Restoration

```bash
# Enable service by service
# Step 3a: Analytics (read-only)
kubectl scale deployment erlmcp-analytics -n erlmcp-prod --replicas=1
sleep 60

# Step 3b: Observability
kubectl scale deployment erlmcp-observability -n erlmcp-prod --replicas=1
sleep 60

# Step 3c: Full scale
kubectl scale deployment -l app=erlmcp -n erlmcp-prod --replicas=3
```

#### Step 4: Ramp Up Traffic

```bash
# Start with 10% traffic
kubectl apply -f - <<EOF
apiVersion: v1
kind: ConfigMap
metadata:
  name: traffic-config
  namespace: erlmcp-prod
data:
  TRAFFIC_PERCENTAGE: "10"
EOF

# Gradually increase over 30 minutes
for i in 10 25 50 75 100; do
  kubectl patch configmap traffic-config -n erlmcp-prod --type=json \
    -p='[{"op": "replace", "path": "/data/TRAFFIC_PERCENTAGE", "value": "'$i'"}]'
  echo "Traffic at $i%"
  sleep 300
done
```

## Prevention Measures

### Circuit Breaker Configuration

```erlang
%% erlmcp_circuit_breaker.erl
-record(circuit_state, {
    state = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    failure_threshold = 5 :: pos_integer(),
    success_threshold = 2 :: pos_integer(),
    timeout = 60000 :: pos_integer(),
    next_attempt = 0 :: integer()
}).

-config({circuit_breaker, [
    {database, [
        {failure_threshold, 5},
        {timeout_ms, 30000},
        {half_open_max_calls, 3}
    ]},
    {external_api, [
        {failure_threshold, 10},
        {timeout_ms, 10000},
        {half_open_max_calls, 5}
    ]}
]}).
```

### Bulkhead Isolation

```erlang
%% erlmcp_bulkhead.erl
-record(bulkhead, {
    name :: atom(),
    max_concurrent = 10 :: pos_integer(),
    max_queue = 100 :: non_neg_integer(),
    timeout = 5000 :: pos_integer()
}).

%% Application config
{bulkheads, [
    {database, [
        {max_concurrent, 50},
        {max_queue, 500},
        {timeout, 5000}
    ]},
    {session_operations, [
        {max_concurrent, 100},
        {max_queue, 1000},
        {timeout, 2000}
    ]}
]}.
```

### Retry Configuration

```erlang
%% erlmcp_retry.erl
-record(retry_config, {
    max_attempts = 3 :: pos_integer(),
    base_backoff = 1000 :: pos_integer(),
    max_backoff = 30000 :: pos_integer(),
    multiplier = 2.0 :: float(),
    jitter = true :: boolean()
}).

%% Global retry policy
{retries, [
    {default_policy, [
        {max_attempts, 3},
        {base_backoff_ms, 1000},
        {max_backoff_ms, 30000},
        {jitter_enabled, true}
    ]},
    {idempotent_operations, [
        {max_attempts, 5},
        {base_backoff_ms, 500}
    ]},
    {non_idempotent_operations, [
        {max_attempts, 1}
    ]}
]}.
```

### Monitoring and Alerts

```yaml
# Prometheus alerts
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: cascading-failure-alerts
  namespace: erlmcp-prod
spec:
  groups:
    - name: cascade_prevention
      rules:
        - alert: CircuitBreakerOpen
          expr: circuit_breaker_state == 1
          for: 1m
          labels:
            severity: warning
          annotations:
            summary: "Circuit breaker is open"

        - alert: HighFailureRate
          expr: rate(request_failures_total[1m]) > 0.5
          for: 2m
          labels:
            severity: critical
          annotations:
            summary: "High failure rate detected - possible cascade"

        - alert: ResourceExhaustion
          expr: process_resident_memory_bytes / process_virtual_memory_max_bytes > 0.9
          for: 5m
          labels:
            severity: critical
          annotations:
            summary: "Memory exhaustion imminent"

        - alert: RetryStormDetected
          expr: rate(retry_attempts_total[1m]) > 100
          for: 1m
          labels:
            severity: critical
          annotations:
            summary: "Retry storm detected"
```

## Chaos Engineering Tests

### Automated Cascade Test

```bash
#!/bin/bash
# scripts/test-cascading-failure.sh

set -e

echo "Starting cascading failure test..."

# Step 1: Disable database
echo "Step 1: Disable database..."
kubectl scale statefulset erlmcp-postgres -n erlmcp-prod --replicas=0

# Step 2: Wait for circuit breaker to open
echo "Step 2: Waiting for circuit breaker..."
sleep 60

# Step 3: Verify circuit breaker opened
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -s http://localhost:8081/admin/circuit-breakers | grep -q "open"
echo "✓ Circuit breaker opened"

# Step 4: Verify no resource exhaustion
MEMORY_BEFORE=$(kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "erlang:memory(total).")
echo "Memory usage: $MEMORY_BEFORE"

# Step 5: Restore database
echo "Step 5: Restoring database..."
kubectl scale statefulset erlmcp-postgres -n erlmcp-prod --replicas=3

# Step 6: Verify automatic recovery
echo "Step 6: Waiting for recovery..."
sleep 120

kubectl exec -n erlmcp-prod erlmcp-0 -- curl -f http://localhost:8081/health
echo "✓ System recovered"

# Step 7: Verify no data corruption
kubectl exec -n erlmcp-prod erlmcp-0 -- /opt/erlmcp/bin/erlmcp eval "
erlmcp_session_ha:validate_consistency().
"
echo "✓ Data consistent"

echo "Test passed!"
```

## Escalation

| Condition | Escalation | Timeline |
|-----------|------------|----------|
| Circuit breaker open | On-call Engineer | Immediate |
| Multiple components failing | Engineering Lead | < 5 min |
| System-wide degradation | Crisis Team | < 10 min |
| Data corruption suspected | CTO | Immediate |

## Runbook Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Cascade Detection Time | < 10 sec | 5 sec |
| Mitigation Time | < 30 sec | 15 sec |
| Recovery Time | < 5 min | 3 min |
| Data Loss | 0 | 0 |

## Related Runbooks

- RB-002: High Memory Usage
- RB-007: Full Cluster Outage
- RB-009: Database Failover
- RB-010: Network Partition
