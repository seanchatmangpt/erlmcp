# Marketplace Reviewer Simulation - Failure Scenario Testing

## Overview

This document provides comprehensive failure scenario tests that GKE marketplace reviewers will execute to validate ERLMCP's production resilience and fault tolerance. Each scenario simulates real-world failures and documents expected behavior, verification steps, and PASS/FAIL criteria.

## Test Environment

- **Platform**: Google Kubernetes Engine (GKE) Regional Cluster
- **Configuration**: Multi-zone (3+ zones)
- **Monitoring**: Cloud Monitoring + Cloud Logging
- **Tools**: kubectl, gcloud, curl, grpcurl

---

## 1. Pod Failure

### Scenario
Individual pod terminates due to OOMKilled, application error, or node pressure.

### Test Procedure
```bash
# 1. Identify running erlmcp pod
POD=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')
echo "Testing pod failure: $POD"

# 2. Establish baseline - note pod UID
POD_UID=$(kubectl get pod $POD -o jsonpath='{.metadata.uid}')
echo "Original pod UID: $POD_UID"

# 3. Create active connection (gRPC client)
# From test pod:
grpcurl -plaintext erlmcp-service:50051 list

# 4. Kill the pod abruptly
kubectl delete pod $POD --grace-period=0 --force

# 5. Monitor recreation
kubectl get pods -l app=erlmcp -w

# 6. Verify new pod is different
NEW_POD=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')
NEW_POD_UID=$(kubectl get pod $NEW_POD -o jsonpath='{.metadata.uid}')
echo "New pod UID: $NEW_POD_UID"

# 7. Verify service continuity
grpcurl -plaintext erlmcp-service:50051 list
```

### Expected Behavior
1. Pod terminates within 30 seconds
2. Deployment controller creates replacement pod immediately
3. New pod enters Ready state within 60 seconds
4. Service continues accepting requests during recreation
5. Load balancer health checks pass
6. No request failures (connection retry succeeds)

### Verification Steps
```bash
# Check pod recreation time
kubectl describe pod $NEW_POD | grep "Started:"

# Verify service endpoints
kubectl get endpoints erlmcp-service

# Check logs for graceful shutdown
kubectl logs $POD --previous | grep -i "shutdown\|terminating"

# Verify readiness probes passed
kubectl get pod $NEW_POD -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}'
```

### PASS Criteria
- [ ] Pod recreated within 60 seconds
- [ ] New pod reaches Ready state
- [ ] Service endpoints updated within 30 seconds
- [ ] Zero request failures observed
- [ ] Health checks pass continuously
- [ ] Logs show graceful shutdown sequence

### FAIL Criteria
- [ ] Pod fails to recreate after 120 seconds
- [ ] Service endpoints remain empty
- [ ] Request failures exceed 5% during failover
- [ ] Health checks fail for >60 seconds
- [ ] Pod crashloops (repeated failures)

---

## 2. Node Failure

### Scenario
Node becomes unavailable due to hardware failure, network partition, or host maintenance.

### Test Procedure
```bash
# 1. Identify node running erlmcp pod
NODE=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].spec.nodeName}')
echo "Testing node failure: $NODE"

# 2. Check baseline pod distribution
kubectl get pods -l app=erlmcp -o wide

# 3. Monitor from another terminal
watch kubectl get pods -l app=erlmcp -o wide

# 4. Cordon node (mark unschedulable)
kubectl cordon $NODE

# 5. Drain node (evict all pods)
kubectl drain $NODE --ignore-daemonsets --delete-emptydir-data --force

# 6. Verify pods rescheduled
kubectl get pods -l app=erlmcp -o wide

# 7. Test service continuity during drain
grpcurl -plaintext erlmcp-service:50051 list
```

### Expected Behavior
1. Node marked unschedulable immediately
2. Pod eviction begins within 10 seconds
3. Pods rescheduled to available nodes
4. New pods start within 30 seconds
5. Service capacity maintained (PodDisruptionBudget honored)
6. No downtime during rescheduling

### Verification Steps
```bash
# Check PodDisruptionBudget
kubectl get pdb erlmcp-pdb

# Verify node status
kubectl describe node $NODE | grep -A 5 "Conditions:"

# Check pod distribution across nodes
kubectl get pods -l app=erlmcp -o wide --no-headers | awk '{print $7}' | sort | uniq -c

# Monitor disruption budget during drain
kubectl get events --sort-by='.lastTimestamp' | grep -i "disruption"

# Verify all pods running
kubectl get pods -l app=erlmcp --field-selector=status.phase=Running
```

### PASS Criteria
- [ ] Node cordoned successfully
- [ ] Pods evicted and rescheduled within 60 seconds
- [ ] Minimum available pods per PDB maintained
- [ ] All pods Running after 90 seconds
- [ ] Service endpoints updated
- [ ] Zero downtime (requests continue)
- [ ] Pods distributed across remaining nodes

### FAIL Criteria
- [ ] PodDisruptionBudget violated
- [ ] Pods stuck in Pending state
- [ ] Service unavailable during drain
- [ ] All pods on single node after drain
- [ ] Pod rescheduling fails after 120 seconds

---

## 3. Zone Failure

### Scenario
Entire availability zone becomes unavailable (network partition, power outage, regional issue).

### Test Procedure
```bash
# 1. Verify multi-zone deployment
kubectl get nodes -L topology.kubernetes.io/zone

# 2. Check pod distribution across zones
kubectl get pods -l app=erlmcp -o wide --no-headers | \
  awk '{print $7}' | sort | uniq -c

# 3. Identify pods in target zone
ZONE="us-central1-a"
ZONE_PODS=$(kubectl get pods -l app=erlmcp -o wide --no-headers | \
  awk -v zone=$ZONE '$7 ~ zone {print $1}')

echo "Pods in $ZONE: $ZONE_PODS"

# 4. Simulate zone failure (cordon all nodes in zone)
for NODE in $(kubectl get nodes -l topology.kubernetes.io/zone=$ZONE -o jsonpath='{.items[*].metadata.name}'); do
  echo "Cordoning node: $NODE"
  kubectl cordon $NODE
  kubectl drain $NODE --ignore-daemonsets --delete-emptydir-data --force
done

# 5. Monitor pod redistribution
watch kubectl get pods -l app=erlmcp -o wide

# 6. Verify service continues
grpcurl -plaintext erlmcp-service:50051 list

# 7. Check cluster health
kubectl get nodes
```

### Expected Behavior
1. All nodes in target zone cordoned
2. Pods from failed zone evicted
3. StatefulSets maintain pod identity (if applicable)
4. Pods recreated in remaining zones
5. Service capacity reduced but functional
6. Auto-scaler provisions replacement capacity
7. No data loss for persistent connections

### Verification Steps
```bash
# Verify zone distribution
kubectl get pods -l app=erlmcp -o wide --no-headers | \
  awk '{print $7}' | sort | uniq -c

# Check cluster autoscaler activity
kubectl get events --sort-by='.lastTimestamp' | grep -i "autoscaler"

# Verify StatefulSet pod identity (if used)
kubectl get statefulset -l app=erlmcp

# Monitor service health
kubectl get endpoints erlmcp-service

# Check for zone-specific failures in logs
kubectl logs -l app=erlmcp --tail=100 | grep -i "zone\|partition"
```

### PASS Criteria
- [ ] Pods redistributed to healthy zones within 90 seconds
- [ ] Service remains available (2/3 zones operational)
- [ ] No permanent pod loss
- [ ] StatefulSet pod identities maintained
- [ ] Cluster auto-scaler provisions replacement capacity
- [ ] Load balancer directs traffic to healthy zones
- [ ] Monitoring shows zone failure handled

### FAIL Criteria
- [ ] Service unavailable during failover
- [ ] Pods fail to reschedule after 120 seconds
- [ ] StatefulSet loses quorum (if applicable)
- [ ] All pods in single remaining zone
- [ ] No autoscaling activity observed
- [ ] Connection errors exceed threshold

---

## 4. Instance Restart (VM Restart)

### Scenario
Compute Engine VM instance is stopped and restarted (simulating host maintenance or failure).

### Test Procedure
```bash
# 1. Identify GKE node and corresponding GCE instance
NODE=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].spec.nodeName}')
echo "Testing instance restart for node: $NODE"

# Get GCE instance name from node
INSTANCE=$(kubectl get node $NODE -o jsonpath='{.metadata.labels.node\.gke\.io\/instance-name}')
ZONE=$(kubectl get node $NODE -o jsonpath='{.metadata.labels.topology\.kubernetes\.io\/zone}')
echo "GCE Instance: $INSTANCE in zone $ZONE"

# 2. Check baseline pod state
kubectl get pods -l app=erlmcp -o wide

# 3. Monitor from another terminal
watch kubectl get pods -l app=erlmcp -o wide

# 4. Stop the VM instance via gcloud
gcloud compute instances stop $INSTANCE --zone=$ZONE

# 5. Wait for node to become NotReady
kubectl wait --for=condition=Ready=false node/$NODE --timeout=60s

# 6. Start the instance
gcloud compute instances start $INSTANCE --zone=$ZONE

# 7. Monitor node recovery
kubectl wait --for=condition=Ready node/$NODE --timeout=300s

# 8. Verify pods return to Running
kubectl get pods -l app=erlmcp -o wide
```

### Expected Behavior
1. VM stops within 30 seconds
2. Node status changes to NotReady
3. Pods on node enter Unknown state
4. Controller evicts pods after timeout (pod-eviction-timeout)
5. VM restarts within 60 seconds
6. Node rejoins cluster and becomes Ready
7. Pods rescheduled to node (or other nodes)
8. Service recovers fully

### Verification Steps
```bash
# Monitor node status during restart
kubectl get nodes -o wide -w

# Check pod eviction events
kubectl get events --sort-by='.lastTimestamp' | grep -i "evict"

# Verify kubelet recovery
kubectl logs -n kube-system -l k8s-app=kubelet --tail=50 | grep -i "recovery"

# Check final pod distribution
kubectl get pods -l app=erlmcp -o wide

# Verify service health after recovery
kubectl get endpoints erlmcp-service
grpcurl -plaintext erlmcp-service:50051 list
```

### PASS Criteria
- [ ] Node marked NotReady within 60s of VM stop
- [ ] Pods evicted after timeout (default 5min)
- [ ] VM restarts successfully
- [ ] Node rejoins cluster (Ready status)
- [ ] Pods rescheduled and Running
- [ ] Service fully operational after restart
- [ ] No manual intervention required

### FAIL Criteria
- [ ] Node fails to restart after 300s
- [ ] Pods stuck in Unknown state
- [ ] Node remains NotReady indefinitely
- [ ] Service fails to recover
- [ ] Manual intervention required
- [ ] Data loss or corruption observed

---

## 5. Service Restart (Cloud Run Zero-Downtime Deployment)

### Scenario
Deploy new version with zero downtime using rolling updates.

### Test Procedure
```bash
# 1. Check current deployment
kubectl get deployment erlmcp-deployment
kubectl get pods -l app=erlmcp

# 2. Note current image
CURRENT_IMAGE=$(kubectl get deployment erlmcp-deployment -o jsonpath='{.spec.template.spec.containers[0].image}')
echo "Current image: $CURRENT_IMAGE"

# 3. Monitor rolling update in real-time
watch kubectl get pods -l app=erlmcp

# 4. Update image tag (simulate new deployment)
kubectl set image deployment/erlmcp-deployment \
  erlmcp=erlmcp:v2.0.0 \
  --record

# 5. Monitor rollout status
kubectl rollout status deployment/erlmcp-deployment

# 6. Verify new pods running
kubectl get pods -l app=erlmcp

# 7. Test service during rollout (concurrent test)
# From separate terminal, run continuous requests:
while true; do
  grpcurl -plaintext erlmcp-service:50051 list || echo "FAIL: $(date)"
  sleep 0.1
done
```

### Expected Behavior
1. Rolling update begins immediately
2. New pods created progressively (maxSurge)
3. Old pods terminated after new pods ready (maxUnavailable)
4. Service capacity maintained throughout
5. Zero dropped connections
6. Health checks validate new pods before traffic
7. Rollback possible if issues detected

### Verification Steps
```bash
# Check deployment revision history
kubectl rollout history deployment/erlmcp-deployment

# Verify rolling update configuration
kubectl get deployment erlmcp-deployment -o jsonpath='{.spec.strategy.rollingUpdate}'

# Check pod readiness during transition
kubectl get pods -l app=erlmcp -o jsonpath='{.items[*].status.conditions[?(@.type=="Ready")].status}'

# Monitor update progress
kubectl describe deployment erlmcp-deployment | grep -A 10 "Replicas:"

# Verify new image running
NEW_IMAGE=$(kubectl get deployment erlmcp-deployment -o jsonpath='{.spec.template.spec.containers[0].image}')
echo "New image: $NEW_IMAGE"

# Check for any errors during update
kubectl get events --sort-by='.lastTimestamp' | grep -i "error\|fail" | tail -20
```

### PASS Criteria
- [ ] Rolling update completes within 120s
- [ ] At least minAvailable pods always running
- [ ] Zero request failures during update
- [ ] New pods pass readiness probes
- [ ] Old pods terminated gracefully
- [ ] Service endpoints always updated
- [ ] Rollback available via `kubectl undo`

### FAIL Criteria
- [ ] Deployment stuck in progress
- [ ] Service unavailable during update
- [ ] All pods terminated simultaneously
- [ ] New pods fail readiness probes
- [ ] Rollback required to restore service
- [ ] Request failures exceed 5%

---

## 6. Secret Rotation

### Scenario
Rotate TLS certificate or authentication secret while service is running.

### Test Procedure
```bash
# 1. Create initial secret
cat <<EOF > initial-cert.yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-tls
type: kubernetes.io/tls
data:
  tls.crt: $(base64 -w 0 /path/to/cert1.pem)
  tls.key: $(base64 -w 0 /path/to/key1.pem)
EOF
kubectl apply -f initial-cert.yaml

# 2. Deploy service with initial secret
kubectl apply -f erlmcp-deployment-with-secret.yaml

# 3. Verify service running with original cert
kubectl get pods -l app=erlmcp
grpcurl -plaintext erlmcp-service:50051 list

# 4. Generate new certificate
openssl req -x509 -newkey rsa:4096 -keyout key2.pem -out cert2.pem -days 365 -nodes -subj "/CN=erlmcp2"

# 5. Create new secret version
cat <<EOF > new-cert.yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-tls
type: kubernetes.io/tls
data:
  tls.crt: $(base64 -w 0 cert2.pem)
  tls.key: $(base64 -w 0 key2.pem)
EOF
kubectl apply -f new-cert.yaml

# 6. Trigger rolling update (pods pick up new secret)
kubectl rollout restart deployment/erlmcp-deployment

# 7. Monitor secret rotation
watch kubectl get pods -l app=erlmcp

# 8. Verify service continues
grpcurl -plaintext erlmcp-service:50051 list
```

### Expected Behavior
1. Original secret mounted in pods
2. Service operates normally with original cert
3. New secret applied to Kubernetes
4. Rolling update picks up new secret
5. Pods reload certificate without interruption
6. Service continues accepting connections
7. No authentication failures

### Verification Steps
```bash
# Check secret version in pods
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- ls -la /etc/tls/

# Verify certificate loaded by application
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- openssl x509 -in /etc/tls/tls.crt -noout -text | grep "Subject:"

# Monitor rolling update
kubectl rollout status deployment/erlmcp-deployment

# Check for secret-related errors
kubectl logs -l app=erlmcp --tail=100 | grep -i "tls\|cert\|secret"

# Verify no TLS handshake failures
kubectl get events --sort-by='.lastTimestamp' | grep -i "tls"
```

### PASS Criteria
- [ ] Secret updated successfully
- [ ] Rolling update completes
- [ ] All pods running with new secret
- [ ] Zero authentication failures
- [ ] No TLS handshake errors
- [ ] Service remains available
- [ ] Certificate valid in all pods

### FAIL Criteria
- [ ] Pods fail to start after secret rotation
- [ ] TLS handshake failures
- [ ] Service unavailable during rotation
- [ ] Certificate validation errors
- [ ] Pods stuck in CrashLoopBackOff
- [ ] Manual intervention required

---

## 7. Resource Exhaustion

### Scenario
Pod consumes all available CPU or memory, triggering OOMKilled or throttling.

### Test Procedure

#### 7a. Memory Exhaustion (OOMKilled)
```bash
# 1. Deploy stress test pod
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Pod
metadata:
  name: erlmcp-stress-memory
spec:
  containers:
  - name: stress
    image: progrium/stress
    command: ["stress", "--vm", "1", "--vm-bytes", "500M", "--vm-hang", "1"]
    resources:
      limits:
        memory: "512Mi"
  restartPolicy: Never
EOF

# 2. Monitor OOMKilled event
kubectl get pod erlmcp-stress-memory -w
kubectl get events --sort-by='.lastTimestamp' | grep -i "oom"

# 3. Verify pod was OOMKilled
kubectl get pod erlmcp-stress-memory -o jsonpath='{.status.containerStatuses[0].state.terminated.reason}'

# 4. Verify restart policy
kubectl get pod erlmcp-stress-memory -o jsonpath='{.spec.restartPolicy}'

# 5. Test ERLMCP service remains available
grpcurl -plaintext erlmcp-service:50051 list
```

#### 7b. CPU Throttling
```bash
# 1. Deploy CPU stress test
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Pod
metadata:
  name: erlmcp-stress-cpu
spec:
  containers:
  - name: stress
    image: progrium/stress
    command: ["stress", "--cpu", "2"]
    resources:
      limits:
        cpu: "100m"
      requests:
        cpu: "50m"
  restartPolicy: Never
EOF

# 2. Monitor CPU throttling
kubectl top pod erlmcp-stress-cpu
kubectl exec erlmcp-stress-cpu -- cat /sys/fs/cgroup/cpu/cpu.stat

# 3. Verify ERLMCP unaffected
kubectl top pod -l app=erlmcp
grpcurl -plaintext erlmcp-service:50051 list
```

### Expected Behavior
1. Memory exhaustion triggers OOMKilled
2. Pod terminated and restarted per policy
3. CPU throttling prevents starvation
4. Resource limits enforced
5. Other pods unaffected (isolation)
6. ERLMCP service continues normally
7. Monitoring captures resource events

### Verification Steps
```bash
# Check resource limits
kubectl get deployment erlmcp-deployment -o jsonpath='{.spec.template.spec.containers[0].resources}'

# Verify OOMKilled event
kubectl describe pod erlmcp-stress-memory | grep -A 10 "Last State:"

# Check CPU throttling metrics
kubectl exec erlmcp-stress-cpu -- cat /sys/fs/cgroup/cpu/cpu.stat | grep nr_throttled

# Verify resource isolation
kubectl top nodes
kubectl top pods -A

# Check for resource-related events
kubectl get events --sort-by='.lastTimestamp' | grep -i "resource\|throttle\|oom"

# Verify ERLMCP health
kubectl get pods -l app=erlmcp
kubectl get endpoints erlmcp-service
```

### PASS Criteria
- [ ] OOMKilled pod terminated correctly
- [ ] Restart policy applied (restart or not)
- [ ] CPU throttling enforced
- [ ] Resource isolation maintained
- [ ] ERLMCP pods unaffected
- [ ] Service remains available
- [ ] Monitoring captures all events

### FAIL Criteria
- [ ] OOMKilled affects other pods
- [ ] No resource limits enforced
- [ ] Node-level resource exhaustion
- [ ] ERLMCP service degraded
- [ ] Cascading failures
- [ ] No monitoring/alerting

---

## 8. Network Partition

### Scenario
Network connectivity between pods, or to external services, is disrupted.

### Test Procedure

#### 8a. Inter-Pod Partition
```bash
# 1. Identify two pods to partition
POD1=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')
POD2=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[1].metadata.name}')
NODE1=$(kubectl get pod $POD1 -o jsonpath='{.spec.nodeName}')
NODE2=$(kubectl get pod $POD2 -o jsonpath='{.spec.nodeName}')

echo "Partitioning $NODE1 <-> $NODE2"

# 2. Apply network policy to block traffic
cat <<EOF | kubectl apply -f -
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: test-partition
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: erlmcp
    ports:
    - protocol: TCP
      port: 50051
EOF

# 3. Test connectivity from pod1 to pod2
kubectl exec $POD1 -- nc -zv $POD2.erlmcp-service 50051

# 4. Monitor service behavior
kubectl get pods -l app=erlmcp -w
grpcurl -plaintext erlmcp-service:50051 list

# 5. Remove partition
kubectl delete networkpolicy test-partition
```

#### 8b. External Service Partition
```bash
# 1. Block external connectivity
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- iptables -A OUTPUT -d 8.8.8.8 -j DROP

# 2. Test DNS resolution
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- nslookup google.com

# 3. Verify retry behavior
kubectl logs -l app=erlmcp --tail=50 | grep -i "retry\|timeout"

# 4. Restore connectivity
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- iptables -D OUTPUT -d 8.8.8.8 -j DROP
```

### Expected Behavior
1. Network partition triggers connection errors
2. Application implements retry logic with exponential backoff
3. Circuit breaker opens after repeated failures
4. Service degrades gracefully
5. No cascading failures
6. Connectivity restored when partition heals
7. Circuit breaker closes after successful probe

### Verification Steps
```bash
# Check network policies
kubectl get networkpolicies

# Test service connectivity
grpcurl -plaintext erlmcp-service:50051 list

# Check logs for retry behavior
kubectl logs -l app=erlmcp --tail=100 | grep -i "retry\|backoff\|timeout"

# Verify circuit breaker state (if exposed)
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- curl -s http://localhost:9090/metrics | grep circuit_breaker

# Check for DNS resolution issues
kubectl logs -l app=erlmcp --tail=100 | grep -i "dns\|resolution"

# Verify no cascading failures
kubectl get pods -A
```

### PASS Criteria
- [ ] Retry logic implemented correctly
- [ ] Exponential backoff observed
- [ ] Circuit breaker prevents cascading failures
- [ ] Service degrades gracefully
- [ ] No pod crashes due to network errors
- [ ] Connectivity restored after partition heals
- [ ] Monitoring captures network events

### FAIL Criteria
- [ ] No retry logic (immediate failure)
- [ ] Cascading pod failures
- [ ] Service completely unavailable
- [ ] Circuit breaker not implemented
- [ ] No monitoring of network events
- [ ] Pods crash due to connection errors

---

## 9. Persistent Volume Loss

### Scenario
PersistentVolumeClaim loses access to underlying storage.

### Test Procedure
```bash
# 1. Identify PVC and PV
PVC=$(kubectl get pvc -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')
PV=$(kubectl get pvc $PVC -o jsonpath='{.spec.volumeName}')
echo "Testing PVC: $PVC (PV: $PV)"

# 2. Check current pod using PVC
POD=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')
kubectl get pod $POD -o jsonpath='{.spec.volumes[*].persistentVolumeClaim.claimName}'

# 3. Write test data to volume
kubectl exec -it $POD -- sh -c "echo 'test-data-$(date)' > /data/test.txt"

# 4. Delete pod (release volume lock)
kubectl delete pod $POD

# 5. Simulate PV loss (reclaim policy: Retain)
kubectl patch pv $PV -p '{"spec":{"persistentVolumeReclaimPolicy":"Retain"}}'

# 6. Delete PVC (simulate loss)
kubectl delete pvc $PVC

# 7. Attempt to recreate PVC
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: erlmcp-data
spec:
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
  storageClassName: standard
EOF

# 8. Verify pod can start
kubectl get pods -l app=erlmcp
```

### Expected Behavior
1. PVC deletion handled gracefully
2. Pod enters Pending state until PVC available
3. PVC can be recreated
4. Data persistence depends on ReclaimPolicy
5. Service recovers when storage restored
6. No data loss if ReclaimPolicy=Retain
7. Application handles missing data directory

### Verification Steps
```bash
# Check PVC status
kubectl get pvc
kubectl describe pvc erlmcp-data

# Check PV status
kubectl get pv
kubectl describe pv $PV

# Verify pod state
kubectl get pods -l app=erlmcp
kubectl describe pod $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}')

# Check storage class reclaim policy
kubectl get storageclass
kubectl describe storageclass standard

# Verify data persistence (if Retain policy)
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- cat /data/test.txt
```

### PASS Criteria
- [ ] PVC recreated successfully
- [ ] Pod binds to new PVC
- [ ] Pod reaches Running state
- [ ] Application handles missing data gracefully
- [ ] Service recovers after storage restored
- [ ] No pod crashes
- [ ] ReclaimPolicy configured appropriately

### FAIL Criteria
- [ ] PVC stuck in Pending state
- [ ] Pod fails to bind to PVC
- [ ] Application crashes on missing data
- [ ] No recovery mechanism
- [ ] Data loss (if Retain policy expected)
- [ ] Manual intervention required

---

## 10. Database Connection Loss

### Scenario
Application loses connectivity to database (Cloud SQL, Spanner, etc.).

### Test Procedure
```bash
# 1. Identify database endpoint
DB_ENDPOINT=$(kubectl get configmap erlmcp-config -o jsonpath='{.data.DATABASE_ENDPOINT}')
echo "Database endpoint: $DB_ENDPOINT"

# 2. Verify current connectivity
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- nc -zv $DB_ENDPOINT 5432

# 3. Monitor application logs
kubectl logs -l app=erlmcp -f > db-test.log &

# 4. Block database connectivity
cat <<EOF | kubectl apply -f -
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: block-db
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  egress:
  - to:
    - podSelector: {}
    ports:
    - protocol: TCP
      port: 5432
EOF

# 5. Wait for connection loss
sleep 10

# 6. Verify retry behavior in logs
grep -i "retry\|reconnect\|timeout" db-test.log

# 7. Restore connectivity
kubectl delete networkpolicy block-db

# 8. Verify reconnection
kubectl logs -l app=erlmcp --tail=50 | grep -i "reconnect\|connected"
```

### Expected Behavior
1. Connection loss detected
2. Connection pool drains existing connections
3. Application implements retry with exponential backoff
4. Requests fail gracefully (circuit breaker)
5. Health checks reflect database status
6. Connections restored when database available
7. No data loss or corruption

### Verification Steps
```bash
# Check connection pool metrics
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- curl -s http://localhost:9090/metrics | grep db_pool

# Verify health checks
kubectl get pod $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}'

# Check application logs for database errors
kubectl logs -l app=erlmcp --tail=100 | grep -i "database\|connection"

# Verify circuit breaker state
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- curl -s http://localhost:9090/metrics | grep circuit_breaker

# Test liveness/readiness probes
kubectl describe pod $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') | grep -A 5 "Liveness\|Readiness"
```

### PASS Criteria
- [ ] Connection loss detected immediately
- [ ] Retry logic implemented correctly
- [ ] Exponential backoff observed
- [ ] Circuit breaker prevents cascading failures
- [ ] Health checks reflect database status
- [ ] Connections restored automatically
- [ ] No data loss or corruption

### FAIL Criteria
- [ ] No retry logic
- [ ] Application crashes on connection loss
- [ ] No circuit breaker
- [ ] Health checks not implemented
- [ ] Manual reconnection required
- [ ] Data corruption

---

## 11. Configuration Drift

### Scenario
ConfigMap or Secret changes are applied while pods are running.

### Test Procedure
```bash
# 1. Create initial configmap
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-config
data:
  LOG_LEVEL: "info"
  MAX_CONNECTIONS: "100"
EOF

# 2. Deploy pod with configmap
kubectl apply -f erlmcp-deployment-with-configmap.yaml

# 3. Verify initial config loaded
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- env | grep LOG_LEVEL

# 4. Update configmap
kubectl patch configmap erlmcp-config -p '{"data":{"LOG_LEVEL":"debug"}}'

# 5. Verify pods don't auto-reload
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- env | grep LOG_LEVEL

# 6. Trigger rolling update to pick up new config
kubectl rollout restart deployment/erlmcp-deployment

# 7. Verify new config loaded
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- env | grep LOG_LEVEL

# 8. Verify service continues
grpcurl -plaintext erlmcp-service:50051 list
```

### Expected Behavior
1. Initial ConfigMap loaded into pods
2. ConfigMap changes don't auto-reload
3. Rolling update picks up new configuration
4. Pods validate configuration on startup
5. Invalid config prevents pod start
6. Service remains available during update
7. No downtime

### Verification Steps
```bash
# Check configmap version
kubectl get configmap erlmcp-config -o jsonpath='{.metadata.resourceVersion}'

# Verify config in pods
kubectl exec -it $(kubectl get pod -l app=erlmcp -o jsonpath='{.items[0].metadata.name}') \
  -- cat /etc/config/erlmcp.conf

# Monitor rolling update
kubectl rollout status deployment/erlmcp-deployment

# Check pod restart count
kubectl get pods -l app=erlmcp -o jsonpath='{.items[*].status.containerStatuses[0].restartCount}'

# Verify config validation (check logs)
kubectl logs -l app=erlmcp --tail=50 | grep -i "config\|validation"

# Test service with new config
grpcurl -plaintext erlmcp-service:50051 list
```

### PASS Criteria
- [ ] ConfigMap changes detected
- [ ] Rolling update triggered
- [ ] New config loaded successfully
- [ ] Invalid config prevents startup
- [ ] Service remains available
- [ ] Zero downtime
- [ ] Configuration validation implemented

### FAIL Criteria
- [ ] Config changes not detected
- [ ] Pods don't pick up new config
- [ ] Invalid config causes crashes
- [ ] Service unavailable during update
- [ ] No configuration validation
- [ ] Manual intervention required

---

## 12. Multi-Region Failover

### Scenario
Primary GKE regional cluster becomes unavailable, traffic fails over to secondary region.

### Test Procedure
```bash
# 1. Verify multi-region setup
gcloud container clusters list --filter="name:erlmcp-*"

# 2. Check primary cluster
PRIMARY_CLUSTER="erlmcp-primary"
PRIMARY_REGION="us-central1"
gcloud container clusters describe $PRIMARY_CLUSTER --region=$PRIMARY_REGION

# 3. Verify traffic routing
kubectl get ingress,svc -l app=erlmcp

# 4. Simulate primary region failure
# Scale down all deployments to 0
kubectl scale deployment/erlmcp-deployment --replicas=0

# 5. Verify Cloud Load Balancer health checks fail
gcloud compute health-checks list

# 6. Monitor traffic failover to secondary region
# Check from external client:
watch -n 1 curl -s https://erlmcp.example.com/health

# 7. Verify secondary region serving traffic
SECONDARY_CONTEXT="gke_${PROJECT}_us-east1_erlmcp-secondary"
kubectl --context=$SECONDARY_CONTEXT get pods -l app=erlmcp

# 8. Restore primary region
kubectl scale deployment/erlmcp-deployment --replicas=3

# 9. Verify traffic returns (or stays on secondary depending on routing policy)
```

### Expected Behavior
1. Primary region serves traffic normally
2. Health checks monitor primary region
3. Primary region failure detected
4. Load balancer shifts traffic to secondary region
5. Secondary region handles increased load
6. Zero to minimal downtime (RTO < 5 minutes)
7. No data loss (RPO = 0 with synchronous replication)

### Verification Steps
```bash
# Check multi-region cluster status
gcloud container clusters list --filter="name:erlmcp-*"

# Verify global load balancer configuration
gcloud compute url-maps list
gcloud compute url-maps describe erlmcp-lb

# Check health check status
gcloud compute health-checks describe erlmcp-hc

# Monitor backend service health
gcloud compute backend-services describe erlmcp-backend --global

# Verify DNS failover (if using Cloud DNS)
gcloud dns record-sets list --zone=erlmcp-zone

# Check replication lag (if using multi-region database)
gcloud sql instances describe erlmcp-db --region=us-central1

# Verify failover metrics in Cloud Monitoring
gcloud monitoring time-series-list --filter='metric.type="loadbalancing.googleapis.com/https/request_count"'
```

### PASS Criteria
- [ ] Primary region failure detected <60s
- [ ] Traffic fails over to secondary region
- [ ] RTO (Recovery Time Objective) <5 minutes
- [ ] RPO (Recovery Point Objective) = 0
- [ ] Secondary region handles load successfully
- [ ] No manual intervention required
- [ ] Health checks correctly reflect region status

### FAIL Criteria
- [ ] Failover takes >10 minutes
- [ ] Traffic fails to route to secondary region
- [ ] Secondary region cannot handle load
- [ ] Data loss during failover
- [ ] Manual intervention required
- [ ] Health checks not configured
- [ ] No monitoring/alerting

---

## Test Execution Summary

### Prerequisites
- [ ] GKE Regional cluster (multi-zone)
- [ ] Multi-region deployment configured
- [ ] Cloud Monitoring enabled
- [ ] Cloud Logging enabled
- [ ] Network policies enabled
- [ ] PodDisruptionBudget configured
- [ ] Health checks configured (liveness, readiness, startup)
- [ ] Resource limits configured
- [ ] Backup/recovery procedures documented

### Test Environment
- **Kubernetes Version**: 1.28+
- **GKE Version**: Latest stable
- **Node Count**: 3+ nodes per zone
- **Zone Count**: 3+ zones
- **Regions**: 2+ regions (for failover tests)

### Success Criteria
- **Overall**: ≥11/12 scenarios PASS
- **Critical**: Scenarios 1, 2, 3, 8, 12 MUST PASS
- **Important**: Scenarios 5, 6, 7, 10 MUST PASS
- **Nice-to-have**: Scenarios 4, 9, 11

### Failure Remediation
Any FAIL criteria requires:
1. Root cause analysis
2. Fix implementation
3. Regression test
4. Documentation update
5. Re-test until PASS

---

## Appendix: Testing Tools

### Required Tools
```bash
# Kubernetes tools
kubectl --version
gcloud version

# gRPC testing
grpcurl --version

# Monitoring
gcloud monitoring --version

# Network testing
netcat (nc)
iptables
curl
```

### Helper Scripts
```bash
# Continuous health check
while true; do
  grpcurl -plaintext erlmcp-service:50051 list && echo "OK: $(date)" || echo "FAIL: $(date)"
  sleep 1
done

# Monitor pod restarts
kubectl get pods -l app=erlmcp -w

# Monitor all events
kubectl get events -w --all-namespaces

# Check resource usage
kubectl top pods -l app=erlmcp --containers

# Test network connectivity
kubectl exec -it <pod> -- nc -zv <target> <port>
```

---

## Document Metadata
- **Version**: 1.0.0
- **Last Updated**: 2025-02-02
- **Owner**: ERLMCP Team
- **Approved By**: [Marketplace Review Team]
- **Next Review**: After major version upgrade
