# Interpreting Benchmark Results

## Overview

This guide explains how to understand and analyze the metrics collected from Docker Swarm benchmarks.

## Key Metrics

### Latency Metrics

**Definition**: Time taken for a message to travel from client to server and receive a response.

**Percentiles**:
- **p50 (Median)**: 50% of requests faster than this value
- **p95 (95th percentile)**: 95% of requests faster than this value
- **p99 (99th percentile)**: 99% of requests faster than this value
- **Max**: Slowest request observed

**Interpretation**:

```
Ideal Range:
  p50:  10-50ms    (good for interactive use)
  p95:  50-200ms   (acceptable for most applications)
  p99:  100-500ms  (tail latency acceptable)
  max:  <5000ms    (no catastrophic slowdowns)

Warning Thresholds:
  p95 > 500ms      (SLA breach likely)
  p99 > 2000ms     (tail latency becoming problematic)
  max > 10000ms    (investigate outliers)
```

### Throughput Metrics

**Definition**: Number of messages processed per second.

**Types**:
- **Request Rate (req/sec)**: Messages processed by servers
- **Message Rate (msg/sec)**: Total messages sent by all clients
- **Tool Call Rate**: Specifically tool execution requests

**Interpretation**:

```
Expected by Load Profile:
  Baseline:        2,000-3,000 msg/sec
  Connection Flood: 10,000-50,000 msg/sec
  Message Bomb:    150,000+ msg/sec

Degradation Indicators:
  <50% of expected = performance issue
  >20% latency increase = saturation point
  Errors > 0.1% = critical threshold
```

### Connection Metrics

**Definition**: Number of active TCP/WebSocket connections.

**Key Indicators**:
- **Active Connections**: Current open connections
- **Connection Rate**: New connections per second
- **Connection Errors**: Failed connection attempts
- **Connection Duration**: Time connections remain open

**Interpretation**:

```
Healthy Behavior:
  - Stable active connection count
  - Low error rate (<1% of attempts)
  - Smooth ramp-up/ramp-down
  - Quick recovery after disruptions

Red Flags:
  - Sudden connection drops (network issues)
  - High error rate (server capacity exceeded)
  - Slow recovery (poor connection management)
  - Memory leak (connections never closed)
```

### Error Metrics

**Types**:
- **Connection Errors**: Failed to establish connection
- **Write Errors**: Failed to send message
- **Timeout Errors**: Request exceeded timeout
- **Protocol Errors**: Invalid message format
- **Server Errors**: Server returned error response

**Interpretation**:

```
Acceptable Error Rates:
  Normal load:      <0.01% (1 per 10,000)
  Stressed load:    <1%    (1 per 100)
  Failure recovery: <5%    (during disruption)
  After recovery:   <0.1%  (quickly stabilizes)

Error Rate = Errors / (Errors + Successes)

Critical Thresholds:
  >5% sustained     = investigate immediately
  >10% = system failure
```

### Resource Metrics

**Types**:
- **CPU Usage**: Percentage of CPU cores utilized
- **Memory Usage**: RAM consumed by containers
- **Network I/O**: Bytes transmitted/received
- **Disk I/O**: Read/write operations

**Interpretation**:

```
Healthy Resource Usage:
  CPU:    20-60% (headroom for spikes)
  Memory: 40-70% (stable, no leaks)
  Network: <80% of capacity

Warning Signs:
  CPU steadily increasing = memory leak or busy loop
  Memory never decreasing = memory leak
  Disk I/O high = logging or metrics issue
  Network saturation = bottleneck found
```

## Baseline Performance Analysis

### Baseline Test Results

Expected results when running 25 normal clients at 100 msg/sec each:

```
Metric                  | Expected | Acceptable Range
───────────────────────────────────────────────────────
Total Throughput        | 2,500    | 2,000-3,500
p50 Latency (ms)        | 25       | 10-50
p95 Latency (ms)        | 100      | 50-200
p99 Latency (ms)        | 250      | 100-500
Max Latency (ms)        | 1,000    | <5,000
Connection Count        | 25       | 23-27
Error Rate (%)          | 0.001    | <0.01
Server CPU (%)          | 30       | <60
Server Memory (MB)      | 256      | <400
───────────────────────────────────────────────────────
```

### How to Calculate Baseline

1. **Extract metrics**:
   ```bash
   curl 'http://localhost:9091/api/v1/query_range' \
     -d 'query=histogram_quantile(0.95, rate(mcp_client_request_duration_ms_bucket[1m]))' \
     -d 'start=<test_start>' \
     -d 'end=<test_end>' \
     -d 'step=1m' | jq .
   ```

2. **Calculate statistics**:
   ```python
   import statistics

   latencies = [100, 95, 102, 98, ...]  # values from metrics

   print(f"Mean: {statistics.mean(latencies):.2f}")
   print(f"Median: {statistics.median(latencies):.2f}")
   print(f"Stdev: {statistics.stdev(latencies):.2f}")
   print(f"Max: {max(latencies):.2f}")
   ```

3. **Save baseline**:
   ```bash
   cat > baseline.json << 'EOF'
   {
     "timestamp": "2024-01-27T12:00:00Z",
     "configuration": {
       "clients": 25,
       "message_rate": 100,
       "duration": 300
     },
     "results": {
       "p50_latency_ms": 25,
       "p95_latency_ms": 100,
       "p99_latency_ms": 250,
       "throughput": 2500
     }
   }
   EOF
   ```

## Stress Test Analysis

### Connection Flood Test

Phase breakdown:

**Ramp-up Phase** (0-5 minutes):
- Expected: Linear increase in connections (0 → 500)
- Healthy: Latency stays constant, errors <1%
- Problem: Exponential latency increase (degradation)

**Peak Phase** (5-8 minutes):
- Expected: 500 sustained connections, stable throughput
- Healthy: p95 latency ≤ 300ms, error rate <0.5%
- Problem: Latency spikes, errors >5%

**Cool-down Phase** (8-10 minutes):
- Expected: Gradual connection drop, latency normalizes
- Healthy: Recovery time <30 seconds
- Problem: Connections linger, memory not released

### Message Bombing Test

Analysis approach:

1. **Find peak throughput**:
   ```bash
   tail -n 100 test-results/message_bombing/metrics.jsonl | \
     jq '.req_rate' | sort -rn | head -1
   ```

2. **Identify saturation point**:
   - Where latency starts increasing (usually 80%+ capacity)
   - Where error rate becomes significant (>0.1%)

3. **Calculate headroom**:
   ```
   Headroom = (Max Throughput - Baseline) / Baseline * 100%

   Example: 150,000 req/sec ÷ 2,500 req/sec = 60x headroom
   ```

## Comparison Analysis

### Comparing Test Results

Use this template to compare runs:

```markdown
# Comparison: Baseline vs Stress Test

## Configuration Changes
- Clients: 25 → 100 (+300%)
- Message Rate: 100 → 500 msg/sec (+400%)

## Results Comparison

| Metric | Baseline | Stress | Change |
|--------|----------|--------|--------|
| Throughput | 2,500 | 40,000 | +1,500% |
| p50 Latency | 25ms | 35ms | +40% |
| p95 Latency | 100ms | 200ms | +100% |
| p99 Latency | 250ms | 1,000ms | +300% |
| Error Rate | 0.01% | 2% | +200x |
| CPU Usage | 30% | 75% | +250% |

## Analysis
- **Positive**: System handled 40x load with acceptable degradation
- **Concern**: p99 latency increased 3x (tail latency problem)
- **Action**: Investigate message queuing or connection pooling
```

## Identifying Bottlenecks

### Method 1: Metric Correlation

```python
import json

# Load test data
baseline = json.load(open('baseline_test/metrics.json'))
stress = json.load(open('stress_connection_flood/metrics.json'))

# Find correlations
print("When connections increased:")
print(f"  Latency changed by: {(stress_p95 - baseline_p95) / baseline_p95:.1%}")
print(f"  Error rate changed by: {(stress_errors - baseline_errors) / baseline_errors:.1%}")
print(f"  CPU changed by: {(stress_cpu - baseline_cpu) / baseline_cpu:.1%}")

if (stress_p95 / baseline_p95) > 5:
    print("→ Bottleneck: Connection handling (latency spike)")
elif (stress_errors / baseline_errors) > 100:
    print("→ Bottleneck: Request queue saturation")
elif (stress_cpu / baseline_cpu) < 1.5:
    print("→ Bottleneck: Network I/O bound")
```

### Method 2: Resource Analysis

```bash
# Check if CPU-bound
if [ $(grep CPU_PERCENT test-results/*/metrics.jsonl | tail -1) -gt 90 ]; then
    echo "Bottleneck: CPU-bound (add more cores or optimize code)"
fi

# Check if memory-bound
if [ $(grep MEMORY_PERCENT test-results/*/metrics.jsonl | tail -1) -gt 90 ]; then
    echo "Bottleneck: Memory-bound (add more RAM or reduce pool sizes)"
fi

# Check if network-bound
if [ $(grep NETWORK_UTILIZATION test-results/*/metrics.jsonl | tail -1) -gt 80 ]; then
    echo "Bottleneck: Network-bound (check bandwidth or connection pooling)"
fi
```

### Method 3: Log Analysis

```bash
# Find error spikes
grep -r "error\|ERROR\|panic\|PANIC" docker-logs/ | wc -l

# Check for timeouts
grep -r "timeout\|timed out\|deadline" docker-logs/ | wc -l

# Look for queue depth
grep -r "queue\|pending\|backlog" docker-logs/ | tail -20
```

## Performance Tuning Recommendations

### If Latency Increases

```
p95 increased from 100ms to 300ms:
1. Check if it correlates with connection count
   → Add connection pooling
2. Check if it correlates with message rate
   → Add message batching
3. Check if it correlates with CPU usage
   → Optimize hot paths or add more servers
4. Check if it correlates with memory
   → Fix memory leaks
```

### If Throughput Decreases

```
Throughput decreased from 10K to 8K msg/sec:
1. Check error rate increase
   → Fix underlying issue causing errors
2. Check latency increase
   → Investigate bottleneck
3. Check resource usage
   → Scale up resources
4. Check network saturation
   → Optimize message size or add bandwidth
```

### If Errors Increase

```
Error rate jumped from 0.01% to 2%:
1. Check error type (connection vs protocol vs timeout)
2. Check correlated metrics (CPU, memory, connections)
3. Check server logs for crash/panic messages
4. Verify network connectivity between nodes
5. Check if error rate stabilizes after burst
```

## Creating Performance Reports

### Automated Report Generation

```bash
#!/bin/bash
# Generate comparison report

python3 << 'EOF'
import json
from datetime import datetime

# Load results
baseline = json.load(open('test-results/baseline_test/metrics.json'))
stress = json.load(open('test-results/connection_flood/metrics.json'))

report = {
    "generated": datetime.utcnow().isoformat() + "Z",
    "comparison": {
        "throughput_multiplier": stress['throughput'] / baseline['throughput'],
        "latency_increase": (stress['p95_latency'] - baseline['p95_latency']) / baseline['p95_latency'],
        "error_increase": (stress['error_rate'] - baseline['error_rate']) / baseline['error_rate']
    }
}

with open('report.json', 'w') as f:
    json.dump(report, f, indent=2)

print("Report generated: report.json")
EOF
```

## SLA Verification

Example SLA definition:

```yaml
SLA:
  latency:
    p50: 50ms
    p95: 200ms
    p99: 1000ms
  throughput: 1000 msg/sec minimum
  availability: 99.9%
  error_rate: <0.1%

Check:
  if p95_latency > 200ms: SLA_BREACH
  if throughput < 1000: SLA_BREACH
  if error_rate > 0.001: SLA_BREACH
  if downtime > 0.1%: SLA_BREACH
```
