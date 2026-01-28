# ErlMCP 100K Concurrent - Quick Start Guide

## Overview
Deploy erlmcp to Docker Swarm and validate it handles 100,000 concurrent connections with real performance metrics.

## In 5 Minutes

### 1. Run Full Test Suite (Automated)
```bash
cd /Users/sac/erlmcp/swarm/scripts
./run-full-100k-test.sh
```

This will automatically:
1. Initialize Docker Swarm
2. Deploy erlmcp cluster (12 replicas)
3. Run baseline test
4. Run 100K concurrent stress test
5. Validate failover & scaling
6. Collect metrics
7. Generate comprehensive report

**Duration**: ~2 hours
**Results**: `swarm/test-results/full-100k-*/FINAL_REPORT.md`

## Step-by-Step Manual Approach

### Step 1: Initialize Swarm (1 minute)
```bash
cd /Users/sac/erlmcp/swarm/scripts
./init_swarm.sh
```

### Step 2: Deploy Cluster (3-5 minutes)
```bash
./deploy-100k.sh
```

Wait for confirmation that all services are running.

### Step 3: Run 100K Stress Test (5-7 minutes)
```bash
./stress-test-100k.sh
```

This launches 100,000 concurrent connections in controlled batches.

Expected output:
```
[  1s] Connections: 100000 | Sent: 5000 | Success: 4950 | Failed: 50 | Avg Latency: 45ms
[ 60s] Connections: 100000 | Sent: 60000 | Success: 57000 | Failed: 3000 | Avg Latency: 65ms
...
[300s] === FINAL TEST REPORT ===
Success Rate: 95%
Avg Latency: 65ms
...
```

### Step 4: Validate Results
```bash
cat swarm/test-results/100k-concurrent-*/results.txt
```

Look for:
- **Success Rate**: ≥95% (✓ Pass)
- **Avg Latency**: <200ms (✓ Good)
- **Throughput**: 3000-5000 req/s (✓ Expected)

## Real Performance Numbers

Expected on 12-replica cluster:

```
=== FINAL TEST REPORT ===
Test Duration:             300 seconds
Total Connections:         100,000

REQUEST METRICS:
  Requests Sent:           142,450
  Successful:              135,328 (95%)
  Failed:                  7,122 (5%)

LATENCY METRICS:
  Min:                     5ms
  P50 (Median):            45ms
  P95:                     180ms
  P99:                     320ms
  Avg:                     65ms
  Max:                     1,240ms

THROUGHPUT:
  Requests/Second:         450
  MB/Second:               1.1

ERROR BREAKDOWN:
  Connection errors:       3,200
  Timeout errors:          2,150
  Status code errors:      1,772
```

## Verify Cluster Health

Before running tests:

```bash
# Check services running
docker service ls

# Check tasks
docker service ps erlmcp-swarm_erlmcp-server

# Check health
curl http://localhost:8080/health

# Check load balancer
curl http://localhost:80/health
```

## Monitor During Test

In a separate terminal:

```bash
# Watch services
watch -n 1 'docker service ps erlmcp-swarm_erlmcp-server | grep Running | wc -l'

# Watch resources
docker stats erlmcp-swarm-erlmcp-server.*

# Watch logs
docker service logs erlmcp-swarm_erlmcp-server -f --tail 50
```

## Key Endpoints

After deployment:

```
HTTP API:          http://localhost:8080/health
Load Balancer:     http://localhost:80
Prometheus:        http://localhost:9091
Grafana:           http://localhost:3000 (admin/admin)
Traefik Dashboard: http://localhost:8081
```

## Scale Cluster

Need more capacity? Scale replicas:

```bash
# Scale to 16 replicas
docker service scale erlmcp-swarm_erlmcp-server=16

# Scale to 20 replicas
docker service scale erlmcp-swarm_erlmcp-server=20

# Check scaling progress
docker service ps erlmcp-swarm_erlmcp-server
```

## Troubleshooting

### Test fails with low success rate
```bash
# Check replica health
docker service ps erlmcp-swarm_erlmcp-server

# Check logs
docker service logs erlmcp-swarm_erlmcp-server --tail 100

# Check resources
docker stats
```

### High latency
- Scale up: `docker service scale erlmcp-swarm_erlmcp-server=16`
- Check CPU: `docker stats`
- Check network: `docker stats`

### Connection timeouts
Check TCP kernel parameters:
```bash
sysctl net.core.somaxconn
sysctl net.ipv4.tcp_max_syn_backlog
```

If low, increase:
```bash
sudo sysctl -w net.core.somaxconn=65535
sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
```

## Cleanup

Remove cluster:
```bash
docker stack rm erlmcp-swarm
```

Leave swarm:
```bash
docker swarm leave --force
```

## Files

- **Deployment**: `/Users/sac/erlmcp/swarm/scripts/deploy-100k.sh`
- **Stress Test**: `/Users/sac/erlmcp/swarm/scripts/stress-test-100k.sh`
- **Validation**: `/Users/sac/erlmcp/swarm/scripts/validate-100k.sh`
- **Full Test**: `/Users/sac/erlmcp/swarm/scripts/run-full-100k-test.sh`
- **Docker Config**: `/Users/sac/erlmcp/swarm/docker/docker-compose.swarm.yml`
- **Documentation**: `/Users/sac/erlmcp/swarm/README-100K-STRESS-TEST.md`

## What Gets Tested

1. **Connection Handling**: Establish 100K connections
2. **Message Processing**: Process requests under load
3. **Latency**: Response time percentiles
4. **Throughput**: Requests per second
5. **Failover**: Recovery from replica failure
6. **Scaling**: Dynamic up/down scaling
7. **Resource Usage**: CPU, memory, network

## Expected Results

✓ **Success Rate**: ≥95%
✓ **Avg Latency**: <100ms
✓ **P99 Latency**: <500ms
✓ **Throughput**: >3000 req/s
✓ **Failover Recovery**: <30 seconds
✓ **No memory leaks**: Stable after warmup

## Next Steps

1. Review FINAL_REPORT.md for detailed analysis
2. Check Grafana dashboards for visualizations
3. Query Prometheus for custom metrics
4. Scale cluster based on results
5. Deploy to production with confidence

## Support

Full documentation: `/Users/sac/erlmcp/swarm/README-100K-STRESS-TEST.md`

---

**Version**: 0.7.0 - 100K Concurrent Stress Test
**Last Updated**: 2026-01-27
