# StatefulSet vs Deployment for erlmcp v3

## Table of Contents

- [Overview](#overview)
- [Deployment Pattern](#deployment-pattern)
- [StatefulSet Pattern](#statefulset-pattern)
- [Hybrid Architecture](#hybrid-architecture)
- [Decision Matrix](#decision-matrix)
- [Configuration Examples](#configuration-examples)
- [Migration Strategies](#migration-strategies)
- [Operational Considerations](#operational-considerations)

## Overview

erlmcp v3 is an Erlang/OTP MCP SDK that requires careful consideration when choosing between Kubernetes Deployment and StatefulSet resources. The choice depends on your clustering requirements, data persistence needs, and operational constraints.

### Key Differences

| Feature | Deployment | StatefulSet |
|---------|-----------|-------------|
| **Pod Identity** | Random names | Predictable, stable names |
| **Network Identity** | No stable network ID | Stable DNS per pod |
| **Ordering** | Parallel creation/deletion | Ordered creation/deletion |
| **Storage** | Shared PVC or ephemeral | Per-pod PVC |
| **Erlang Clustering** | Challenging | Natural fit |
| **Scaling** | Fast, parallel | Slower, sequential |
| **Use Case** | Stateless nodes | Clustered nodes |

## Deployment Pattern

### When to Use Deployment

Use Kubernetes **Deployment** for:

1. **Stateless API Gateways**: Nodes handling HTTP/WebSocket requests
2. **Load-Balanced Workers**: Process-per-connection model
3. **Horizontally Scalable Services**: Rapid scaling requirements
4. **Ephemeral Workloads**: No persistent state required
5. **Cost Optimization**: Shared storage or no storage

### Architecture

```
┌─────────────────────────────────────────┐
│        External Load Balancer           │
└─────────────────┬───────────────────────┘
                  │
         ┌────────▼─────────┐
         │   Ingress/Service│
         └────────┬─────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
┌───▼───┐    ┌───▼───┐    ┌───▼───┐
│ erlmcp│    │erlmcp │    │erlmcp │
│  pod1 │    │  pod2 │    │  pod3 │
│(random│    │(random│    │(random│
│ name) │    │ name) │    │ name) │
└───┬───┘    └───┬───┘    └───┬───┘
    │            │            │
    └────────────┼────────────┘
                 │
         ┌───────▼────────┐
         │  Shared Storage│
         │   (Optional)   │
         └────────────────┘
```

### Deployment Configuration

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-gateway
  namespace: erlmcp
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 0          # Zero-downtime updates
      maxSurge: 1                # One extra pod during updates

  selector:
    matchLabels:
      app: erlmcp
      component: gateway

  template:
    metadata:
      labels:
        app: erlmcp
        component: gateway
    spec:
      # Anti-affinity for high availability
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
            - weight: 100
              podAffinityTerm:
                labelSelector:
                  matchLabels:
                    app: erlmcp
                    component: gateway
                topologyKey: kubernetes.io/hostname

      containers:
        - name: erlmcp
          image: ghcr.io/seanchatmangpt/erlmcp:3.0.0
          ports:
            - name: http
              containerPort: 8080
            - name: metrics
              containerPort: 9090

          env:
            - name: ERLMCP_NODE_TYPE
              value: "gateway"
            - name: ERLMCP_CLUSTERING
              value: "false"              # No Erlang clustering
            - name: ERLMCP_SESSION_BACKEND
              value: "redis"              # External session storage

          resources:
            requests:
              cpu: "500m"
              memory: "1Gi"
            limits:
              cpu: "2000m"
              memory: "4Gi"

          # Fast startup and health checks
          livenessProbe:
            httpGet:
              path: /health
              port: 8080
            initialDelaySeconds: 15
            periodSeconds: 10

          readinessProbe:
            httpGet:
              path: /ready
              port: 8080
            initialDelaySeconds: 5
            periodSeconds: 5

          # Graceful shutdown
          lifecycle:
            preStop:
              exec:
                command: ["/bin/sh", "-c", "sleep 5"]

      terminationGracePeriodSeconds: 30
```

### Deployment Advantages

1. **Fast Scaling**: Parallel pod creation/deletion
2. **Simple Operations**: Standard rolling updates
3. **Lower Overhead**: No persistent volume management
4. **Cloud-Native**: Natural fit for cloud load balancers
5. **Cost-Effective**: Shared resources

### Deployment Limitations

1. **No Stable Network Identity**: Pods get random DNS names
2. **Challenging Clustering**: Erlang distributed requires stable node names
3. **State Management**: Requires external state storage (Redis, PostgreSQL)
4. **Session Affinity**: May need sticky sessions

## StatefulSet Pattern

### When to Use StatefulSet

Use Kubernetes **StatefulSet** for:

1. **Erlang Distributed Clusters**: Nodes forming an Erlang cluster
2. **Persistent State**: Mnesia database per node
3. **Stable Network Identity**: DNS-based discovery
4. **Ordered Operations**: Sequential startup/shutdown
5. **Data Locality**: Each node owns specific data shards

### Architecture

```
┌──────────────────────────────────────────┐
│     Headless Service (erlmcp-headless)   │
│  erlmcp-0.erlmcp-headless.erlmcp.svc... │
│  erlmcp-1.erlmcp-headless.erlmcp.svc... │
│  erlmcp-2.erlmcp-headless.erlmcp.svc... │
└──────────────────────────────────────────┘
                    │
    ┌───────────────┼───────────────┐
    │               │               │
┌───▼─────┐    ┌───▼─────┐    ┌───▼─────┐
│erlmcp-0 │◄──►│erlmcp-1 │◄──►│erlmcp-2 │
│         │    │         │    │         │
│ Erlang  │    │ Erlang  │    │ Erlang  │
│Cluster  │    │Cluster  │    │Cluster  │
└───┬─────┘    └───┬─────┘    └───┬─────┘
    │              │              │
┌───▼─────┐    ┌───▼─────┐    ┌───▼─────┐
│ PVC-0   │    │ PVC-1   │    │ PVC-2   │
│ 10Gi    │    │ 10Gi    │    │ 10Gi    │
│(Mnesia) │    │(Mnesia) │    │(Mnesia) │
└─────────┘    └─────────┘    └─────────┘
```

### StatefulSet Configuration

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-headless
  namespace: erlmcp
spec:
  clusterIP: None                    # Headless service
  publishNotReadyAddresses: true     # DNS for all pods
  selector:
    app: erlmcp
    component: cluster
  ports:
    - name: tcp
      port: 8080
    - name: epmd
      port: 4369
    - name: distribution
      port: 9100

---
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  serviceName: erlmcp-headless
  replicas: 3

  # Pod management policy
  podManagementPolicy: OrderedReady  # Sequential startup

  # Update strategy
  updateStrategy:
    type: RollingUpdate
    rollingUpdate:
      partition: 0                   # Canary updates possible

  selector:
    matchLabels:
      app: erlmcp
      component: cluster

  template:
    metadata:
      labels:
        app: erlmcp
        component: cluster
    spec:
      # Required: Pod anti-affinity for HA
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
            - labelSelector:
                matchLabels:
                  app: erlmcp
                  component: cluster
              topologyKey: kubernetes.io/hostname

      containers:
        - name: erlmcp
          image: ghcr.io/seanchatmangpt/erlmcp:3.0.0

          ports:
            - name: tcp
              containerPort: 8080
            - name: epmd
              containerPort: 4369        # Erlang Port Mapper Daemon
            - name: distribution
              containerPort: 9100        # Distributed Erlang
            - name: metrics
              containerPort: 9090

          env:
            # Stable node name using StatefulSet pod name
            - name: POD_NAME
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name
            - name: POD_NAMESPACE
              valueFrom:
                fieldRef:
                  fieldPath: metadata.namespace

            # Erlang node configuration
            - name: ERLANG_NAME
              value: "erlmcp@$(POD_NAME).erlmcp-headless.$(POD_NAMESPACE).svc.cluster.local"
            - name: ERLMCP_NODE_TYPE
              value: "cluster"
            - name: ERLMCP_CLUSTERING
              value: "true"
            - name: ERLMCP_CLUSTER_DISCOVERY
              value: "kubernetes"
            - name: ERLMCP_K8S_SERVICE
              value: "erlmcp-headless"

            # Erlang cookie from secret
            - name: ERLANG_COOKIE
              valueFrom:
                secretKeyRef:
                  name: erlmcp-secrets
                  key: erlang-cookie

            # Mnesia configuration
            - name: ERLMCP_MNESIA_DIR
              value: "/data/mnesia"
            - name: ERLMCP_SESSION_BACKEND
              value: "mnesia"            # Use Mnesia for sessions

          volumeMounts:
            - name: data
              mountPath: /data

          resources:
            requests:
              cpu: "1000m"
              memory: "2Gi"
            limits:
              cpu: "4000m"
              memory: "8Gi"

          # Slower probes for clustering
          livenessProbe:
            httpGet:
              path: /health
              port: 8080
            initialDelaySeconds: 30
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 3

          readinessProbe:
            httpGet:
              path: /ready
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 5
            timeoutSeconds: 3
            failureThreshold: 3

          # Extended graceful shutdown for cluster leave
          lifecycle:
            preStop:
              exec:
                command:
                  - /bin/sh
                  - -c
                  - |
                    # Leave cluster gracefully
                    erl_call -s -n ${ERLANG_NAME} -c ${ERLANG_COOKIE} \
                      -a 'application stop erlmcp'
                    sleep 10

      terminationGracePeriodSeconds: 120  # Extended for cluster leave

  # Volume claim templates - one PVC per pod
  volumeClaimTemplates:
    - metadata:
        name: data
      spec:
        accessModes: ["ReadWriteOnce"]
        storageClassName: fast-ssd
        resources:
          requests:
            storage: 10Gi
```

### StatefulSet Advantages

1. **Stable Network Identity**: Predictable DNS names
2. **Erlang Clustering**: Natural fit for distributed Erlang
3. **Persistent Storage**: Per-pod PVC for Mnesia
4. **Data Locality**: Sharding with stable placement
5. **Ordered Operations**: Sequential startup for dependencies

### StatefulSet Limitations

1. **Slower Scaling**: Sequential pod creation
2. **Complex Operations**: Manual PVC management
3. **Higher Costs**: Per-pod storage
4. **Upgrade Complexity**: Careful cluster coordination
5. **Recovery Time**: Slower pod replacement

## Hybrid Architecture

### Recommended Pattern for Production

Use both Deployment and StatefulSet for optimal performance:

```
┌────────────────────────────────────────────┐
│          External Load Balancer            │
└────────────────┬───────────────────────────┘
                 │
        ┌────────▼─────────┐
        │  Ingress/Service │
        └────────┬─────────┘
                 │
    ┌────────────┼────────────┐
    │            │            │
┌───▼──────┐ ┌──▼──────┐ ┌──▼──────┐
│ Gateway  │ │ Gateway │ │ Gateway │  ← Deployment
│ Pod      │ │ Pod     │ │ Pod     │    (Stateless)
│ (random) │ │ (random)│ │ (random)│
└────┬─────┘ └────┬────┘ └────┬────┘
     │            │            │
     └────────────┼────────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
┌───▼──────┐ ┌───▼──────┐ ┌───▼──────┐
│erlmcp-0  │ │erlmcp-1  │ │erlmcp-2  │  ← StatefulSet
│          │◄┼──────────┼►│          │    (Clustered)
│ Erlang   │ │ Erlang   │ │ Erlang   │
│ Cluster  │ │ Cluster  │ │ Cluster  │
└────┬─────┘ └────┬─────┘ └────┬─────┘
     │            │            │
┌────▼─────┐ ┌────▼─────┐ ┌────▼─────┐
│  PVC-0   │ │  PVC-1   │ │  PVC-2   │
└──────────┘ └──────────┘ └──────────┘
```

### Hybrid Configuration

#### Gateway Deployment (Stateless)

```yaml
# Gateway handles external requests
deployment:
  enabled: true
  replicas: 3

  env:
    ERLMCP_NODE_TYPE: "gateway"
    ERLMCP_CLUSTERING: "false"
    ERLMCP_BACKEND_NODES: "erlmcp-0.erlmcp-headless,erlmcp-1.erlmcp-headless,erlmcp-2.erlmcp-headless"
```

#### Cluster StatefulSet (Stateful)

```yaml
# StatefulSet handles business logic and state
statefulset:
  enabled: true
  replicas: 3

  env:
    ERLMCP_NODE_TYPE: "cluster"
    ERLMCP_CLUSTERING: "true"
    ERLMCP_SESSION_BACKEND: "mnesia"
```

### Hybrid Benefits

1. **Scalability**: Scale gateways independently from cluster
2. **Cost Optimization**: Only cluster nodes need persistent storage
3. **Performance**: Fast gateway scaling for traffic spikes
4. **Reliability**: Stable cluster with dynamic frontend
5. **Operational Flexibility**: Update gateways without cluster impact

## Decision Matrix

### Choose Deployment When

- ✅ Stateless request processing
- ✅ No Erlang clustering required
- ✅ External state storage (Redis/PostgreSQL)
- ✅ Rapid scaling requirements
- ✅ Cost optimization priority
- ✅ Simple operations

### Choose StatefulSet When

- ✅ Erlang distributed clustering required
- ✅ Mnesia or persistent state per node
- ✅ Stable network identity needed
- ✅ Data locality requirements
- ✅ Ordered operations important
- ✅ Sharding/partitioning strategy

### Choose Hybrid When

- ✅ Enterprise production deployment
- ✅ High traffic with state requirements
- ✅ Cost and performance optimization
- ✅ Independent scaling needs
- ✅ Complex operational requirements

## Configuration Examples

### Example 1: Pure Deployment (Microservice Pattern)

```yaml
# values.yaml
deployment:
  enabled: true
  replicas: 5

statefulset:
  enabled: false

app:
  erlang:
    env:
      ERLMCP_NODE_TYPE: "standalone"
      ERLMCP_CLUSTERING: "false"
      ERLMCP_SESSION_BACKEND: "redis"

  cache:
    enabled: true
    type: "redis"
    redis:
      host: "redis-service"

  database:
    enabled: true
    type: "postgresql"
    host: "postgres-service"
```

### Example 2: Pure StatefulSet (Cluster Pattern)

```yaml
# values.yaml
deployment:
  enabled: false

statefulset:
  enabled: true
  replicas: 3
  podManagementPolicy: "OrderedReady"

app:
  erlang:
    env:
      ERLMCP_NODE_TYPE: "cluster"
      ERLMCP_CLUSTERING: "true"
      ERLMCP_SESSION_BACKEND: "mnesia"
      ERLMCP_CLUSTER_DISCOVERY: "kubernetes"

storage:
  persistentVolumeClaim:
    enabled: true
    size: "100Gi"
```

### Example 3: Hybrid Pattern (Enterprise Pattern)

```yaml
# values.yaml
deployment:
  enabled: true
  replicas: 3
  annotations:
    role: "gateway"

statefulset:
  enabled: true
  replicas: 3
  annotations:
    role: "cluster"

# Gateway configuration
gateway:
  env:
    ERLMCP_NODE_TYPE: "gateway"
    ERLMCP_CLUSTERING: "false"
    ERLMCP_BACKEND_CLUSTER: "erlmcp-headless"

# Cluster configuration
cluster:
  env:
    ERLMCP_NODE_TYPE: "cluster"
    ERLMCP_CLUSTERING: "true"
    ERLMCP_SESSION_BACKEND: "mnesia"
```

## Migration Strategies

### Deployment to StatefulSet Migration

```bash
# Step 1: Scale down Deployment
kubectl scale deployment erlmcp --replicas=0 -n erlmcp

# Step 2: Backup data (if any)
kubectl exec erlmcp-<pod> -n erlmcp -- /backup.sh

# Step 3: Deploy StatefulSet
helm upgrade erlmcp ./helm/erlmcp-enterprise \
  --set deployment.enabled=false \
  --set statefulset.enabled=true \
  --wait

# Step 4: Restore data
kubectl exec erlmcp-0 -n erlmcp -- /restore.sh

# Step 5: Verify cluster formation
kubectl exec erlmcp-0 -n erlmcp -- erl_call -s -n erlmcp@erlmcp-0... -c cookie -a 'nodes'

# Step 6: Delete old Deployment
kubectl delete deployment erlmcp -n erlmcp
```

### StatefulSet to Deployment Migration

Only possible if moving to external state storage.

```bash
# Step 1: Export Mnesia data
kubectl exec erlmcp-0 -n erlmcp -- mnesia:backup("/tmp/backup")

# Step 2: Import to external storage (e.g., PostgreSQL)
# ... migration script ...

# Step 3: Deploy new Deployment
helm upgrade erlmcp ./helm/erlmcp-enterprise \
  --set statefulset.enabled=false \
  --set deployment.enabled=true \
  --set app.sessions.backend=redis \
  --wait

# Step 4: Verify functionality

# Step 5: Scale down StatefulSet
kubectl scale statefulset erlmcp --replicas=0 -n erlmcp

# Step 6: Delete StatefulSet and PVCs
kubectl delete statefulset erlmcp -n erlmcp
kubectl delete pvc -l app=erlmcp -n erlmcp
```

## Operational Considerations

### Scaling

#### Deployment Scaling

```bash
# Fast, parallel scaling
kubectl scale deployment erlmcp --replicas=10 -n erlmcp
```

#### StatefulSet Scaling

```bash
# Slower, sequential scaling
kubectl scale statefulset erlmcp --replicas=5 -n erlmcp

# Monitor scaling progress
kubectl rollout status statefulset erlmcp -n erlmcp
```

### Updates

#### Deployment Rolling Update

```bash
# Zero-downtime update
kubectl set image deployment/erlmcp erlmcp=erlmcp:3.0.1 -n erlmcp
kubectl rollout status deployment erlmcp -n erlmcp
```

#### StatefulSet Rolling Update

```bash
# Update with partition for canary
kubectl patch statefulset erlmcp -n erlmcp -p '{"spec":{"updateStrategy":{"type":"RollingUpdate","rollingUpdate":{"partition":2}}}}'
kubectl set image statefulset erlmcp erlmcp=erlmcp:3.0.1 -n erlmcp

# Verify canary (only erlmcp-2 updates)
kubectl get pods -l app=erlmcp -n erlmcp

# Complete rollout
kubectl patch statefulset erlmcp -n erlmcp -p '{"spec":{"updateStrategy":{"rollingUpdate":{"partition":0}}}}'
```

### Backup and Recovery

#### Deployment Backup

No persistent state - backup external storage only.

#### StatefulSet Backup

```bash
# Backup all PVCs
for i in 0 1 2; do
  kubectl exec erlmcp-$i -n erlmcp -- /backup.sh
done

# Or use Velero
velero backup create erlmcp-statefulset \
  --include-namespaces erlmcp \
  --wait
```

## Docker-Only Validation

Per CLAUDE.md requirements, all validation must be done via Docker:

```bash
# Validate Deployment configuration
docker compose run erlmcp-build kubectl apply --dry-run=client -f /workspace/gitops/base/deployment.yaml

# Validate StatefulSet configuration
docker compose run erlmcp-build kubectl apply --dry-run=client -f /workspace/gitops/base/statefulset.yaml

# Test Helm chart rendering
docker compose run erlmcp-build helm template erlmcp /workspace/helm/erlmcp-enterprise --values /workspace/helm/erlmcp-enterprise/values-production.yaml
```

## Conclusion

- **Deployment**: Best for stateless, scalable services
- **StatefulSet**: Best for clustered Erlang nodes with state
- **Hybrid**: Best for enterprise production deployments

Choose based on your specific requirements for clustering, state management, and operational complexity.

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3/kubernetes/statefulset-vs-deployment
