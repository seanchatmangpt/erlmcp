# RB-008: Minority Partition Handling Runbook

## Overview

This runbook describes the procedure for detecting and handling minority network partitions in an erlmcp cluster to prevent split-brain scenarios.

**Severity**: P0 - Critical
**Component**: Cluster Coordination
**Reference**: P0-007

---

## Background

### What is a Split-Brain?

A split-brain occurs when a network partition divides a cluster into two or more groups that cannot communicate, but each group believes it is the legitimate cluster. This can lead to:

- Data divergence (inconsistent state)
- Conflicting writes
- Data corruption
- Difficulty in reconciliation

### Minority Partition Detection

The cluster uses a **majority-based quorum** strategy:

- **Quorum Size**: Configurable (default: 3 nodes)
- **Majority Threshold**: `floor(quorum_size / 2) + 1` nodes
- **Minority Partition**: Fewer than majority threshold nodes
- **Majority Partition**: At least majority threshold nodes

### Strategy: Shutdown Minority Partitions

When a network partition is detected:

1. **Minority partitions** automatically shut down to prevent split-brain
2. **Majority partitions** continue normal operations
3. When network heals, minority nodes rejoin and sync state

---

## Detection

### Automatic Detection

The cluster automatically detects partitions via:

1. **EPMD Connectivity**: Checks for reachable cluster nodes
2. **Health Check Scripts**: `check-partition.sh` monitors connectivity
3. **Netstat Connections**: Counts established distribution connections

### Manual Detection

Check partition status manually:

```bash
# Check reachable nodes
docker exec erlmcp epmd -names

# Check network connections
docker exec erlmcp netstat -an | grep ESTABLISHED

# Check cluster membership
docker exec erlmcp erl -noshell -name detector@127.0.0.1 -setcookie "$ERLANG_COOKIE" \
  -eval "net_adm:ping('erlmcp@127.0.0.1'), halt()."
```

---

## Procedure

### Step 1: Verify Partition Status

```bash
# Run the detection script
./scripts/runbooks/detect-and-shutdown-minority.sh

# Expected output:
# - MAJORITY PARTITION: Continue operations
# - MINORITY PARTITION DETECTED: Will shutdown
```

### Step 2: Minority Partition Shutdown (Automatic)

If in minority partition, the container will:

1. Log minority partition detection
2. Wait `SHUTDOWN_DELAY` seconds (default: 10)
3. Stop the container

```bash
# Automatic shutdown logs:
# ⚠️  MINORITY PARTITION DETECTED
# Reachable nodes: 1 (need 2 for majority)
# Shutting down minority partition...
```

### Step 3: Majority Partition Operations (Continue)

If in majority partition, operations continue normally:

```bash
# Expected output:
# ✅ MAJORITY PARTITION: Continue operations
# This node is part of the majority partition (2 >= 2)
```

### Step 4: Network Healing and Rejoin

After network partition heals:

```bash
# 1. Start stopped nodes
docker compose up -d

# 2. Verify cluster reconnection
docker exec erlmcp epmd -names

# 3. Check state sync
docker logs erlmcp | grep "Node.*connected"

# 4. Verify data consistency
docker exec erlmcp erlmcp-admin cluster status
```

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ERLMCP_QUORUM_SIZE` | `3` | Total nodes in cluster |
| `SHUTDOWN_DELAY` | `10` | Seconds to wait before shutdown |
| `FORCE_SHUTDOWN` | `false` | Set to `true` to actually shutdown |

### Docker Compose Health Check

```yaml
healthcheck:
  test: ["CMD", "/opt/erlmcp/scripts/check-partition.sh"]
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 60s
```

---

## Troubleshooting

### False Positive: Minority Detection

**Symptoms**: Healthy cluster incorrectly shuts down

**Diagnosis**:
```bash
# Check actual connectivity
docker exec erlmcp netstat -an | grep -E "(4369|ESTABLISHED)"

# Verify QUORUM_SIZE matches actual cluster size
echo $ERLMCP_QUORUM_SIZE
docker ps | grep erlmcp | wc -l
```

**Resolution**:
1. Update `ERLMCP_QUORUM_SIZE` to match actual cluster size
2. Restart healthcheck with correct configuration

### Network Flapping

**Symptoms**: Container repeatedly starts and stops

**Diagnosis**:
```bash
# Check network stability
ping -c 10 <other-node-host>

# Check Docker network
docker network inspect erlmcp-network
```

**Resolution**:
1. Fix underlying network issues
2. Increase `SHUTDOWN_DELAY` to tolerate temporary partitions
3. Implement network redundancy

### Stuck in Shutdown State

**Symptoms**: Container won't start after partition heals

**Diagnosis**:
```bash
# Check container logs
docker logs erlmcp

# Check for manual stop
docker ps -a | grep erlmcp
```

**Resolution**:
```bash
# Manually start container
docker compose up -d

# Verify it stays running
sleep 30
docker ps | grep erlmcp
```

---

## Testing

### Simulate Network Partition

```bash
# 1. Isolate a node (block network)
docker network disconnect erlmcp-network erlmcp-node2

# 2. Verify minority shutdown (on isolated node)
docker logs erlmcp-node2 | grep "MINORITY PARTITION"

# 3. Verify majority continues (on other nodes)
docker ps | grep erlmcp

# 4. Heal partition
docker network connect erlmcp-network erlmcp-node2

# 5. Verify rejoin
docker logs erlmcp-node2 | grep "Node.*connected"
```

### Test Detection Script (Dry Run)

```bash
# Test without actual shutdown
ERLMCP_QUORUM_SIZE=3 \
SHUTDOWN_DELAY=1 \
FORCE_SHUTDOWN=false \
./scripts/runbooks/detect-and-shutdown-minority.sh
```

---

## Related Runbooks

- [RB-001: Incident Response](./INCIDENT_RESPONSE.md)
- [RB-002: Deployment](./DEPLOYMENT_RUNBOOK.md)
- [RAFT Consensus](../../docs/RAFT_CONSENSUS.md)

---

## Change History

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 1.0 | Initial runbook for P0-007 |
