# Network Partition Recovery Runbook

**Runbook ID**: RB-010
**Severity**: SEV-1
**Last Updated**: 2026-02-02

## Overview

This runbook covers network partition scenarios in erlmcp v3, including split-brain detection, automatic recovery, and manual intervention procedures.

## Symptoms

- Nodes unable to communicate
- Inconsistent cluster state
- Split-brain conditions
- Increased timeouts
- Connection refused errors
- Duplicate session IDs

## Prerequisites

- Erlang distribution enabled
- EPMD monitoring active
- Network policies configured
- Split-brain detection enabled
- Health checks configured

## Diagnosis

### Step 1: Check Network Connectivity

```bash
# Check pod-to-pod connectivity
kubectl exec -n erlmcp-prod erlmcp-0 -- ping -c 3 erlmcp-1.erlmcp-prod.erlmcp-prod.svc.cluster.local
kubectl exec -n erlmcp-prod erlmcp-0 -- ping -c 3 erlmcp-2.erlmcp-prod.erlmcp-prod.svc.cluster.local

# Check Erlang distribution
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "net_adm:ping('erlmcp@erlmcp_node2')."

# Check EPMD
docker exec erlmcp_node1 epmd -names
```

### Step 2: Identify Partition Type

```bash
# Check cluster membership from each node
for node in erlmcp_node1 erlmcp_node2 erlmcp_node3; do
  echo "=== $node ==="
  docker exec $node /opt/erlmcp/bin/erlmcp eval "erlmcp_cluster_membership:get_members()."
done

# Compare results - different views indicate split-brain
```

### Step 3: Check Network Policies

```bash
# Check if network policies exist
kubectl get networkpolicies -n erlmcp-prod

# Check pod connectivity rules
kubectl describe pod erlmcp-0 -n erlmcp-prod | grep -A 20 "Network:"
```

## Resolution Procedures

### Automated Recovery

#### Scenario 1: Minor Partition (Automatic Healing)

```bash
# Monitor automatic recovery
watch -n 5 'docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "erlmcp_cluster_membership:get_members()."'

# Check connection restoration
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
[net_adm:ping(N) || N <- nodes()].
"
```

**Expected Behavior**:
- Erlang auto-reconnects when network restores
- Cluster membership automatically synchronizes
- State transfer occurs via Mnesia/RAFT
- No manual intervention needed

**Validation**:
```bash
# Verify all nodes see same cluster view
for node in erlmcp_node1 erlmcp_node2 erlmcp_node3; do
  docker exec $node /opt/erlmcp/bin/erlmcp eval "length(nodes())."
done
# All should return same count (typically 2 for 3-node cluster)
```

### Split-Brain Recovery

#### Scenario 2: Split-Brain Detected (Manual Intervention)

**CRITICAL**: Do NOT allow both partitions to continue processing. One partition MUST be stopped.

```bash
# Step 1: Identify majority partition
for node in erlmcp_node1 erlmcp_node2 erlmcp_node3; do
  echo "=== $node ==="
  docker exec $node /opt/erlmcp/bin/erlmcp eval "length(nodes())."
done

# Partition with > N/2 nodes continues
# Other partition MUST be stopped

# Step 2: Stop minority partition
docker exec erlmcp_node2 /opt/erlmcp/bin/erlmcp eval "init:stop()."
docker exec erlmcp_node3 /opt/erlmcp/bin/erlmcp eval "init:stop()."

# Step 3: Reset cluster state on majority
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
mnesia:clear_table(cluster_membership).
erlmcp_cluster_coordinator:reset_cluster_state().
"

# Step 4: Restart minority nodes
docker start erlmcp_node2
docker start erlmcp_node3

# Step 5: Verify convergence
for node in erlmcp_node1 erlmcp_node2 erlmcp_node3; do
  docker exec $node /opt/erlmcp/bin/erlmcp eval "length(nodes())."
done
```

### Kubernetes Network Partition

#### Scenario 3: K8s Network Issues

```bash
# Step 1: Check network plugins
kubectl get pods -n kube-system | grep -E 'calico|weave|flannel'

# Step 2: Restart CNI plugins if needed
kubectl delete pod -n kube-system -l k8s-app=calico-node

# Step 3: Check network policies
kubectl get networkpolicies -n erlmcp-prod -o yaml

# Step 4: Temporarily allow all traffic (emergency only)
kubectl apply -f - <<EOF
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-allow-all
  namespace: erlmcp-prod
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  ingress:
  - {}
  egress:
  - {}
  policyTypes:
  - Ingress
  - Egress
EOF

# Step 5: Restore proper policies after recovery
kubectl delete networkpolicy erlmcp-allow-all -n erlmcp-prod
```

## Recovery Validation

### Health Checks

```bash
# Comprehensive health check
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
[
  {ok, _} = application:ensure_all_started(erlmcp),
  {ok, Members} = erlmcp_cluster_membership:get_members(),
  true = length(Members) >= 2,
  health_check_passed
].
"

# Check data consistency
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
erlmcp_session_ha:validate_consistency().
"
```

### State Synchronization

```bash
# Trigger full sync after recovery
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
erlmcp_cluster_sync:trigger_full_sync().
"

# Monitor sync progress
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "
erlmcp_cluster_sync:get_sync_status().
"
```

## Prevention Measures

### Network Configuration

```yaml
# docker-compose.yml - network isolation
networks:
  erlmcp-net:
    driver: bridge
    ipam:
      config:
        - subnet: 172.28.0.0/16
    driver_opts:
      com.docker.network.bridge.name: br-erlmcp
      com.docker.network.bridge.enable_icc: "true"
      com.docker.network.bridge.enable_ip_masquerade: "true"

# K8s - network policies
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-cluster-traffic
  namespace: erlmcp-prod
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: erlmcp
    ports:
    - protocol: TCP
      port: 4369  # EPMD
    - protocol: TCP
      port: 9100  # Erlang distribution
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: erlmcp
    ports:
    - protocol: TCP
      port: 4369
    - protocol: TCP
      port: 9100
```

### Erlang Distribution Tuning

```erlang
# vm.args - distribution settings
## Heartbeat
-kernel net_ticktime 60

## Connection setup
-kernel inet_dist_use_interface {172,28,0,1}
-kernel inet_dist_listen_min 9100
-kernel inet_dist_listen_max 9105

## Monitoring
-kernel net_setup intensely
```

### Monitoring and Alerts

```yaml
# Prometheus alerts
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: network-partition-alerts
  namespace: erlmcp-prod
spec:
  groups:
    - name: network
      rules:
        - alert: ErlangNodeDown
          expr: erlang_up == 0
          for: 1m
          labels:
            severity: critical
          annotations:
            summary: "Erlang node is down"

        - alert: SplitBrainDetected
          expr: erlang_cluster_members != erlang_expected_members
          for: 30s
          labels:
            severity: critical
          annotations:
            summary: "Split-brain condition detected"

        - alert: NetworkPartitionDetected
          expr: rate(node_network_receive_errors_total[5m]) > 0.1
          for: 2m
          labels:
            severity: warning
          annotations:
            summary: "Network partition suspected"
```

## Testing Procedures

### Chaos Engineering

```bash
# Simulate network partition using tc (traffic control)
# On node1:
docker exec erlmcp_node1 tc qdisc add dev eth0 root netem loss 100%

# Wait 30 seconds
sleep 30

# Restore network
docker exec erlmcp_node1 tc qdisc del dev eth0 root

# Verify recovery
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "nodes()."
```

### Automated Testing Script

```bash
#!/bin/bash
# scripts/test-network-partition.sh

set -e

echo "Starting network partition test..."

# Get current cluster state
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "length(nodes())." > /tmp/before.txt

# Introduce partition
echo "Introducing partition..."
docker network disconnect erlmcp-net erlmcp_node2

# Wait for detection
sleep 60

# Check partition detected
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "length(nodes())." > /tmp/during.txt

# Restore network
echo "Restoring network..."
docker network connect erlmcp-net erlmcp_node2

# Wait for recovery
sleep 30

# Verify recovery
docker exec erlmcp_node1 /opt/erlmcp/bin/erlmcp eval "length(nodes())." > /tmp/after.txt

echo "Test complete"
echo "Before partition: $(cat /tmp/before.txt)"
echo "During partition: $(cat /tmp/during.txt)"
echo "After partition: $(cat /tmp/after.txt)"
```

## Escalation

| Condition | Escalation | Timeline |
|-----------|------------|----------|
| Auto-recovery in progress | Network Engineer | Monitor |
| Split-brain detected | Engineering Lead | < 2 min |
| Manual recovery required | Crisis Team | < 5 min |
| Data inconsistency | CTO | Immediate |

## Runbook Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Partition Detection Time | < 30 sec | 15 sec |
| Auto-Recovery Success | > 95% | 98% |
| Manual Recovery Time | < 5 min | 3 min |

## Related Runbooks

- RB-004: Network Partition (Basic)
- RB-007: Full Cluster Outage
- RB-009: Database Failover
