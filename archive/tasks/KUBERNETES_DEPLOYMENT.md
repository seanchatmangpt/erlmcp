# erlmcp Kubernetes Deployment Guide

**Version**: 2.1.0
**Status**: Production Ready
**Target**: Auto-scaling erlmcp cluster on Kubernetes

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Prerequisites](#prerequisites)
4. [Deployment Manifests](#deployment-manifests)
5. [Service Mesh](#service-mesh)
6. [Auto-Scaling](#auto-scaling)
7. [Monitoring](#monitoring)
8. [Failure Scenarios](#failure-scenarios)
9. [Maintenance](#maintenance)

---

## Overview

Kubernetes deployment provides cloud-native orchestration for erlmcp with automatic scaling, self-healing, and rolling updates. This guide covers deploying erlmcp on Kubernetes using best practices for stateful distributed systems.

**Key Features**:
- **Horizontal Pod Autoscaling**: Scale based on connections/CPU/memory
- **Self-Healing**: Automatic pod restart and node failover
- **Rolling Updates**: Zero-downtime deployments
- **StatefulSets**: Stable network identities for cluster nodes
- **Service Discovery**: Automatic DNS-based discovery
- **ConfigMaps/Secrets**: Configuration and secrets management

---

## Architecture

### Kubernetes Architecture

```mermaid
graph TB
    subgraph External["External Access"]
        Ingress[Ingress Controller<br/>NGINX/Traefik]
        LB[Load Balancer<br/>Service]
    end

    subgraph Namespace["erlmcp Namespace"]
        subgraph Apps["Applications"]
            Stateful[StatefulSet<br/>erlmcp nodes]
            Deployment[Deployment<br/>Dashboard/Monitoring]
        end

        subgraph Services["Services"]
            Headless[Headless SVC<br/>Cluster DNS]
            Cluster[ClusterIP<br/>Internal access]
            NodePort[NodePort<br/>External access]
        end

        subgraph Config["Configuration"]
            ConfigMap[ConfigMap<br/>sys.config]
            Secret[Secret<br/>TLS certs, cookies]
        end

        subgraph Storage["Storage"]
            PVC[Persistent Volume Claim<br/>Logs/Mnesia]
        end
    end

    subgraph Infrastructure["Infrastructure"]
        K8sAPI[Kubernetes API]
        Kubelet[Kubelet<br/>Node agent]
        Etcd[etcd<br/>Cluster state]
    end

    External --> Ingress
    Ingress --> Services
    LB --> Services

    Services --> Stateful
    Services --> Deployment

    Stateful --> Config
    Stateful --> Storage

    Stateful --> K8sAPI
    Deployment --> K8sAPI

    K8sAPI --> Etcd
    Kubelet --> Stateful
    Kubelet --> Deployment

    style External fill:#FFD700
    style Namespace fill:#90EE90
    style Infrastructure fill:#87CEEB
    style Services fill:#FFE4B5
    style Config fill:#FFE4B5
    style Storage fill:#FFE4B5
```

### Pod Architecture

```mermaid
graph TB
    subgraph Pod["erlmcp Pod"]
        Container1[erlmcp Container<br/>Erlang Runtime]

        subgraph Sidecars["Sidecar Containers"]
            OTEL[OpenTelemetry<br/>Collector]
            Logs[Log Agent<br/>Fluentd]
            Metrics[Metrics Exporter<br/>Prometheus]
        end
    end

    subgraph Volumes["Volumes"]
        Config[ConfigMap Volume<br/>sys.config]
        Certs[Secret Volume<br/>TLS certs]
        Data[Persistent Volume<br/>Mnesia data]
        Logs[EmptyDir<br/>Runtime logs]
    end

    Container1 --> Config
    Container1 --> Certs
    Container1 --> Data
    Container1 --> Logs

    OTEL --> Logs
    Logs --> Logs

    Metrics --> Container1

    style Container1 fill:#FFD700
    style Sidecars fill:#90EE90
    style Volumes fill:#87CEEB
```

---

## Prerequisites

### Cluster Requirements

**Minimum Cluster**:
- Kubernetes version: 1.27+
- Nodes: 3 (for high availability)
- Node specs: 4 vCPU, 8GB RAM
- Total capacity: 12 vCPU, 24GB RAM

**Recommended Production**:
- Kubernetes version: 1.28+
- Nodes: 5+ (for scaling)
- Node specs: 8 vCPU, 16GB RAM
- Total capacity: 40+ vCPU, 80+ GB RAM

### Software Requirements

```bash
# kubectl configured
kubectl version --client

# Cluster access verified
kubectl cluster-info

# Namespace created
kubectl create namespace erlmcp

# Storage class available
kubectl get storageclass
```

---

## Deployment Manifests

### Namespace

```yaml
# k8s/namespace.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: erlmcp
  labels:
    name: erlmcp
    app: erlmcp
```

### ConfigMap

```yaml
# k8s/configmap.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-config
  namespace: erlmcp
data:
  sys.config: |
    [
      {erlmcp, [
        {cluster_mode, true},
        {max_connections, 25000},
        {enable_sharded_registry, true},
        {registry_shards, 256},
        {enable_backpressure, true},
        {enable_circuit_breaker, true},
        {enable_otel_traces, true},
        {otel_exporter, grpc},
        {otel_endpoint, "otel-collector:4317"}
      ]},
      {kernel, [
        {inet_dist_listen_min, 9100},
        {inet_dist_listen_max, 9120},
        {inet_default_connect_options, [
          {nodelay, true},
          {keepalive, true}
        ]}
      ]}
    ].
  vm.args: |
    +P 262144
    +Q 65536
    +A 64
    +MBas aobf
    +sbt db
    +sd 128
    +fnu 524288
    -setcookie ERLMCP_CLUSTER_COOKIE
    -env ERL_MAX_PORTS 65536
    -env ERL_MAX_ETS_TABLES 2000
```

### Secrets

```yaml
# k8s/secrets.yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-secrets
  namespace: erlmcp
type: Opaque
data:
  erlang-cookie: RVJMTUNQX0NMVVNURVJfQ09PS0lF  # Base64 encoded
  jwt-secret: <base64-encoded-jwt-secret>
  tls-cert: <base64-encoded-tls-cert>
  tls-key: <base64-encoded-tls-key>
```

### StatefulSet

```yaml
# k8s/statefulset.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  serviceName: erlmcp-headless
  replicas: 4
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
    spec:
      containers:
      - name: erlmcp
        image: erlmcp:2.1.0
        ports:
        - containerPort: 9201
          name: dist
        - containerPort: 8080
          name: http
        - containerPort: 9090
          name: metrics
        env:
        - name: MY_POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: MY_POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        - name: ERLANG_COOKIE
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: erlang-cookie
        volumeMounts:
        - name: config
          mountPath: /opt/erlmcp/releases/2.1.0/sys.config
          subPath: sys.config
        - name: config
          mountPath: /opt/erlmcp/releases/2.1.0/vm.args
          subPath: vm.args
        - name: data
          mountPath: /var/lib/erlmcp
        - name: certs
          mountPath: /etc/erlmcp/certs
          readOnly: true
        resources:
          requests:
            cpu: "2"
            memory: "4Gi"
          limits:
            cpu: "4"
            memory: "8Gi"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
      - name: otel-collector
        image: otel/opentelemetry-collector:0.91.0
        ports:
        - containerPort: 4317
          name: otlp-grpc
        command:
        - /otelcol
        - --config=/etc/otelcol-config.yaml
        volumeMounts:
        - name: otel-config
          mountPath: /etc/otelcol-config.yaml
          subPath: otelcol-config.yaml
      volumes:
      - name: config
        configMap:
          name: erlmcp-config
      - name: certs
        secret:
          secretName: erlmcp-secrets
      - name: otel-config
        configMap:
          name: otel-config
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: [ "ReadWriteOnce" ]
      storageClassName: "fast-ssd"
      resources:
        requests:
          storage: 10Gi
```

### Services

```yaml
# k8s/services.yaml
---
# Headless service for StatefulSet
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-headless
  namespace: erlmcp
spec:
  clusterIP: None
  selector:
    app: erlmcp
  ports:
  - port: 9201
    name: dist
  - port: 8080
    name: http
---
# ClusterIP for internal access
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  type: ClusterIP
  selector:
    app: erlmcp
  ports:
  - port: 8080
    targetPort: 8080
    name: http
  - port: 9090
    targetPort: 9090
    name: metrics
---
# NodePort for external access
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-external
  namespace: erlmcp
spec:
  type: NodePort
  selector:
    app: erlmcp
  ports:
  - port: 8080
    targetPort: 8080
    nodePort: 30080
    name: http
```

### Horizontal Pod Autoscaler

```yaml
# k8s/hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: erlmcp-hpa
  namespace: erlmcp
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: StatefulSet
    name: erlmcp
  minReplicas: 4
  maxReplicas: 20
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 30
      - type: Pods
        value: 2
        periodSeconds: 30
      selectPolicy: Max
```

### Pod Disruption Budget

```yaml
# k8s/pdb.yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: erlmcp-pdb
  namespace: erlmcp
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: erlmcp
```

---

## Service Mesh

### Istio Integration (Optional)

```mermaid
graph TB
    subgraph Mesh["Istio Service Mesh"]
        Ingress[Istio Ingress Gateway]
        Pilot[Pilot<br/>Traffic Management]
        Citadel[Citadel<br/>mTLS]

        subgraph Services["erlmcp Services"]
            Svc1[erlmcp-0]
            Svc2[erlmcp-1]
            Svc3[erlmcp-2]
            Svc4[erlmcp-3]
        end
    end

    subgraph Features["Mesh Features"]
        mTLS[Automatic mTLS]
        Observability[Observability<br/>Metrics, Traces, Logs]
        Traffic[Traffic Management<br/>Canary, Blue-Green]
        Policy[Policy<br/>Rate Limiting, Auth]
    end

    External[External Clients] --> Ingress
    Ingress --> Pilot
    Pilot --> Svc1
    Pilot --> Svc2
    Pilot --> Svc3
    Pilot --> Svc4

    Svc1 --> Citadel
    Svc2 --> Citadel
    Svc3 --> Citadel
    Svc4 --> Citadel

    Pilot --> Features

    style Mesh fill:#FFD700
    style Services fill:#90EE90
    style Features fill:#87CEEB
```

**Istio VirtualService** example:
```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  hosts:
  - "erlmcp.example.com"
  gateways:
  - erlmcp-gateway
  http:
  - match:
    - uri:
        prefix: /
    route:
    - destination:
        host: erlmcp
        subset: v2
      weight: 100
  - retry:
      attempts: 3
      perTryTimeout: 2s
```

---

## Auto-Scaling

### Scaling Flow Diagram

```mermaid
flowchart TD
    Monitor([Metrics Server<br/>Monitoring]) --> Threshold{Threshold<br/>Exceeded?}

    Threshold -->|CPU > 70%| ScaleUp[Scale Up<br/>Add Pods]
    Threshold -->|Memory > 80%| ScaleUp
    Threshold -->|Connections > 20K/pod| ScaleUp
    Threshold -->|CPU < 30%| ScaleDown[Scale Down<br/>Remove Pods]
    Threshold -->|Normal| Stable([Stable State])

    ScaleUp --> HPA[Horizontal Pod<br/>Autoscaler]

    HPA --> API[Kubernetes<br/>API]

    API --> NewPod[Create New<br/>Pod]

    NewPod --> Join[Pod Joins<br/>Cluster]

    Join --> Rebalance[Rebalance<br/>Connections]

    Rebalance --> Verify{Health<br/>Check?}

    Verify -->|Healthy| Monitor
    Verify -->|Unhealthy| Rollback[Rollback<br/>Scale Down]

    Rollback --> Monitor

    ScaleDown --> HPA
    HPA --> API
    API --> RemovePod[Remove Pod<br/>Gracefully]
    RemovePod --> Drain[Drain<br/>Connections]
    Drain --> Rebalance

    style Monitor fill:#87CEEB
    style ScaleUp fill:#90EE90
    style ScaleDown fill:#FFD700
    style Stable fill:#98FB98
    style Rollback fill:#FFB6C1
```

### Scaling Policies

**Scale Up** (when CPU > 70% or Memory > 80%):
- Add pods up to maxReplicas: 20
- Rate: 100% per 30 seconds (doubling)
- Or: Add 2 pods per 30 seconds (whichever is higher)

**Scale Down** (when CPU < 30% for 5 minutes):
- Remove pods down to minReplicas: 4
- Rate: 50% per 60 seconds
- Stabilization window: 300 seconds (5 minutes)

---

## Monitoring

### Prometheus Monitoring

```mermaid
graph LR
    subgraph Pods["erlmcp Pods"]
        P1[erlmcp-0]
        P2[erlmcp-1]
        P3[erlmcp-2]
        P4[erlmcp-3]
    end

    subgraph Metrics["Metrics Collection"]
        Exporters[/metrics endpoint]
        Annotations[prometheus.io annotations]
    end

    subgraph Prometheus["Prometheus Stack"]
        Scrape[Prometheus<br/>Scrape Config]
        Alert[AlertManager<br/>Alerts]
        Grafana[Grafana<br/>Dashboards]
    end

    P1 --> Exporters
    P2 --> Exporters
    P3 --> Exporters
    P4 --> Exporters

    Exporters --> Scrape
    Annotations --> Scrape

    Scrape --> Alert
    Scrape --> Grafana

    style Pods fill:#FFE4B5
    style Metrics fill:#90EE90
    style Prometheus fill:#87CEEB
```

**Prometheus ServiceMonitor** (if using Prometheus Operator):
```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  selector:
    matchLabels:
      app: erlmcp
  endpoints:
  - port: metrics
    interval: 15s
    path: /metrics
```

### Key Metrics to Monitor

**Pod Metrics**:
- CPU usage (target: <70%)
- Memory usage (target: <80%)
- Network I/O (bytes/sec)
- Restarts (target: 0)
- Age (uptime)

**Application Metrics**:
- Connections per pod (target: <25K)
- Message throughput (msg/sec)
- P50/P95/P99 latency
- Error rate (target: <0.1%)
- Circuit breaker state

**Cluster Metrics**:
- Total pods (min: 4, max: 20)
- Ready pods (target: 100%)
- Pending pods (target: 0)
- Pod distribution across nodes

---

## Failure Scenarios

### Pod Failure Recovery

```mermaid
stateDiagram-v2
    [*] --> Running: Pod created

    Running --> CrashLoopBackoff: Liveness probe fails
    Running --> NotReady: Readiness probe fails

    CrashLoopBackoff --> Restarting: Kubelet restarts pod
    Restarting --> Running: Restart successful
    Restarting --> Failed: Max restarts exceeded

    NotReady --> Running: Probe passes
    NotReady --> Failed: Probe timeout

    Failed --> [*]: Pod deleted
    Failed --> Running: Pod recreated

    note right of Running
        Accepting traffic
        Probes passing
        Metrics normal
    end note

    note right of CrashLoopBackoff
        Application crashed
        Automatic restart
        Backoff delay
    end note
```

### Kubernetes Resource Failure Matrix

```mermaid
graph TB
    subgraph Resources["Kubernetes Resources"]
        Pod[Pod<br/>erlmcp-*]
        Stateful[StatefulSet<br/>erlmcp]
        Service[Service<br/>erlmcp-headless]
        Config[ConfigMap<br/>erlmcp-config]
        PVC[PVC<br/>erlmcp-data]
        HPA[HPA<br/>erlmcp-hpa]
    end

    subgraph Failures["Failure Modes"]
        F1[Pod Crash]
        F2[StatefulSet Deleted]
        F3[Service Misconfigured]
        F4[ConfigMap Error]
        F5[PVC Full]
        F6[HPA Malfunction]
    end

    subgraph Recovery["Recovery"]
        R1[Auto Restart<br/><1min]
        R2[Recreate<br/>from YAML]
        R3[Fix Service<br/>Spec]
        R4[Rollback<br/>Config]
        R5[Expand PVC<br/>or Clean]
        R6[Adjust HPA<br/>Settings]
    end

    Pod --> F1
    Stateful --> F2
    Service --> F3
    Config --> F4
    PVC --> F5
    HPA --> F6

    F1 --> R1
    F2 --> R2
    F3 --> R3
    F4 --> R4
    F5 --> R5
    F6 --> R6

    style Pod fill:#4CAF50
    style Stateful fill:#2196F3
    style Service fill:#FF9800
    style Config fill:#FFEB3B
    style PVC fill:#F44336
    style HPA fill:#9C27B0
```

### Persistent Volume Failure Recovery

```mermaid
flowchart TD
    PVFail([PVC Failure<br/>Detected]) --> Diagnose{Diagnose<br/>Issue}

    Diagnose -->|Disk Full| Full[Volume Full]
    Diagnose -->|Corruption| Corruption[Data Corruption]
    Diagnose -->|Node Loss| NodeLoss[Node Loss]

    Full --> Expand1[Expand PVC<br/>Capacity]
    Expand1 --> Verify1{Verify<br/>Space?}

    Verify1 -->|Yes| Resume1([Resume<br/>Operations])
    Verify1 -->|No| Clean[Clean Up<br/>Logs/Cache]

    Clean --> Expand1

    Corruption --> Restore[Restore from<br/>Backup]
    Restore --> Verify2{Verify<br/>Integrity?}

    Verify2 -->|Yes| Resume2([Resume<br/>Operations])
    Verify2 -->|No| Recreate[Recreate PVC<br/>from Scratch]

    Recreate --> Restore

    NodeLoss --> Reattach[Attach to<br/>New Node]
    Reattach --> Verify3{Verify<br/>Mount?}

    Verify3 -->|Yes| Resume3([Resume<br/>Operations])
    Verify3 -->|No| Recreate

    style PVFail fill:#F44336
    style Resume1 fill:#4CAF50
    style Resume2 fill:#4CAF50
    style Resume3 fill:#4CAF50
    style Recreate fill:#FF9800
```

### Node Failure Handling

```mermaid
flowchart TD
    NodeFail([Node Failure<br/>Detected]) --> Controller{Controller<br/>Manager<br/>Detects}

    Controller --> Mark[Mark Node<br/>NotReady]

    Mark --> Evict[Evict Pods<br/>from Failed Node]

    Evict --> PDB{Pod Disruption<br/>Budget<br/>Satisfied?}

    PDB -->|Yes| Reschedule[Reschedule Pods<br/>to Healthy Nodes]
    PDB -->|No| Wait[Wait for<br/>MinAvailable]

    Wait --> PDB

    Reschedule --> NewPod[Create New Pods]

    NewPod --> Verify{Pods<br/>Ready?}

    Verify -->|Yes| Complete([Recovery<br/>Complete])
    Verify -->|No| Retry[Retry Pod<br/>Creation]

    Retry --> NewPod

    style NodeFail fill:#FFB6C1
    style Complete fill:#90EE90
    style Reschedule fill:#98FB98
    style Evict fill:#FFD700
```

### Network Partition Handling

```mermaid
flowchart TD
    Partition([Network<br/>Partition]) --> Detect{Partition<br/>Detected}

    Detect --> APINode{API Server<br/>Accessible?}

    APINode -->|Yes| Leader[Pod remains<br/>in cluster]
    APINode -->|No| Isolated[Pod isolated<br/>from cluster]

    Leader --> CheckLeader{Is Pod<br/>Leader?}

    CheckLeader -->|Yes| MaintainLeader[Maintain<br/>Leadership]
    CheckLeader -->|No| Follower[Continue as<br/>Follower]

    Isolated --> CanRecover{Can Recover<br/>to API Server?}

    CanRecover -->|Yes| Rejoin[Rejoin cluster<br/>on reconnect]
    CanRecover -->|No| Terminate[Terminate pod<br/>let K8s recreate]

    MaintainLeader --> Monitor([Monitor<br/>network])
    Follower --> Monitor
    Rejoin --> Monitor

    Monitor --> Partition

    Terminate --> Recover([Pod Recreated])

    style Partition fill:#FFB6C1
    style Recover fill:#90EE90
    style Leader fill:#98FB98
    style Isolated fill:#FFD700
```

---

## Maintenance

### Rolling Update Flow

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant K8s as Kubernetes
    participant Pod as Old Pods
    participant NewPod as New Pods
    participant LB as Load Balancer

    Dev->>K8s: kubectl apply -f new-version.yaml
    K8s->>K8s: Detect StatefulSet change

    Note over K8s: Rolling update strategy:<br/>maxUnavailable: 1

    K8s->>Pod: Pod erlmcp-3: Drain
    LB->>Pod: Stop sending traffic
    Pod->>LB: Acknowledge drain

    Pod->>Pod: Graceful shutdown<br/>5s grace period
    K8s->>Pod: Pod terminated

    K8s->>NewPod: Create erlmcp-3 (new version)
    NewPod->>LB: Ready for traffic
    LB->>NewPod: Send traffic

    Note over K8s: Health check passes

    K8s->>Pod: Pod erlmcp-2: Drain
    LB->>Pod: Stop sending traffic
    Pod->>LB: Acknowledge drain

    Pod->>Pod: Graceful shutdown
    K8s->>Pod: Pod terminated

    K8s->>NewPod: Create erlmcp-2 (new version)
    NewPod->>LB: Ready for traffic
    LB->>NewPod: Send traffic

    Note over K8s: Repeat for erlmcp-1, erlmcp-0

    NewPod-->>Dev: Rolling update complete
```

### Performing Rolling Updates

```bash
# Update image
kubectl set image statefulset/erlmcp \
  erlmcp=erlmcp:2.2.0 \
  -n erlmcp

# Watch rollout status
kubectl rollout status statefulset/erlmcp -n erlmcp

# Check revision history
kubectl rollout history statefulset/erlmcp -n erlmcp

# Rollback if needed
kubectl rollout undo statefulset/erlmcp -n erlmcp

# Rollback to specific revision
kubectl rollout undo statefulset/erlmcp --to-revision=2 -n erlmcp
```

### Cluster Scaling

```bash
# Scale up manually
kubectl scale statefulset/erlmcp --replicas=8 -n erlmcp

# Scale down manually
kubectl scale statefulset/erlmcp --replicas=4 -n erlmcp

# Check HPA status
kubectl get hpa -n erlmcp

# Describe HPA for metrics
kubectl describe hpa erlmcp-hpa -n erlmcp

# Disable autoscaling temporarily
kubectl autoscale statefulset erlmcp --min=4 --max=4 -n erlmcp
```

---

## Quick Reference

### Essential kubectl Commands

```bash
# Apply all manifests
kubectl apply -f k8s/ -n erlmcp

# Get all resources
kubectl get all -n erlmcp

# Get pod details
kubectl describe pod erlmcp-0 -n erlmcp

# View logs
kubectl logs -f erlmcp-0 -n erlmcp

# Execute in pod
kubectl exec -it erlmcp-0 -n erlmcp -- /bin/sh

# Port forward
kubectl port-forward erlmcp-0 8080:8080 -n erlmcp

# Get events
kubectl get events -n erlmcp --sort-by='.lastTimestamp'

# Delete pod (will be recreated)
kubectl delete pod erlmcp-0 -n erlmcp

# Edit StatefulSet
kubectl edit statefulset erlmcp -n erlmcp

# Get YAML from running resource
kubectl get statefulset erlmcp -n erlmcp -o yaml
```

### Troubleshooting

```bash
# Check pod status
kubectl get pods -n erlmcp -w

# Check pod logs
kubectl logs erlmcp-0 -n erlmcp --previous  # Previous instance

# Check events
kubectl describe pod erlmcp-0 -n erlmcp

# Check resource usage
kubectl top pods -n erlmcp
kubectl top nodes

# Connect to Erlang shell
kubectl exec -it erlmcp-0 -n erlmcp -- /opt/erlmcp/bin/erlmcp remote_console

# Check cluster connectivity from within pod
kubectl exec -it erlmcp-0 -n erlmcp -- erl -setcookie ERLMCP_CLUSTER_COOKIE -sname test
# In shell: net_adm:ping('erlmcp-1.erlmcp-headless.erlmcp.svc.cluster.local').

# Check StatefulSet status
kubectl get statefulset erlmcp -n erlmcp
kubectl describe statefulset erlmcp -n erlmcp
```

---

## Resource Management

### Resource Quota and Limits

```mermaid
graph TB
    subgraph Namespace["erlmcp Namespace"]
        subgraph Quotas["Resource Quotas"]
            RQ[ResourceQuota<br/>erlmcp-quota]
            LR[LimitRange<br/>erlmcp-limits]
        end

        subgraph Resources["Compute Resources"]
            CPU[CPU Requests<br/>2 cores/pod]
            Mem[Memory Requests<br/>4Gi/pod]
            CPUL[CPU Limits<br/>4 cores/pod]
            MemL[Memory Limits<br/>8Gi/pod]
        end

        subgraph Storage["Storage Resources"]
            Storage[PVC<br/>10Gi/pod]
            StorageClass[StorageClass<br/>fast-ssd]
        end
    end

    RQ --> CPU
    RQ --> Mem
    LR --> CPUL
    LR --> MemL
    RQ --> Storage
    Storage --> StorageClass

    style Namespace fill:#E3F2FD
    style Quotas fill:#BBDEFB
    style Resources fill:#C8E6C9
    style Storage fill:#FFCCBC
```

### Pod Priority and Preemption

```mermaid
flowchart TD
    Start([Resource<br/>Shortage]) --> PrioCheck{Check Pod<br/>Priorities}

    PrioCheck -->|High Prio| High[Critical Pods<br/>Priority: 1000<br/>StatefulSet]
    PrioCheck -->|Medium Prio| Medium[Standard Pods<br/>Priority: 500<br/>HPA Scaled]
    PrioCheck -->|Low Prio| Low[Best Effort<br/>Priority: 0<br/>Batch Jobs]

    High --> Protect[Protect from<br/>Preemption]
    Medium --> Evaluate{Evaluate<br/>Necessity}
    Low --> Preempt[Mark for<br/>Preemption]

    Evaluate -->|Critical| Protect
    Evaluate -->|Optional| Preempt

    Preempt --> Draining[Drain Low<br/>Priority Pods]
    Draining --> Schedule[Schedule High<br/>Priority Pods]

    Schedule --> Verify{Schedule<br/>Success?}

    Verify -->|Yes| Complete([Preemption<br/>Complete])
    Verify -->|No| Manual[Manual<br/>Intervention]

    Protect --> Monitor([Continue<br/>Monitoring])

    style Start fill:#FFD700
    style Complete fill:#4CAF50
    style Manual fill:#F44336
    style High fill:#FF9800
    style Medium fill:#FFEB3B
    style Low fill:#E0E0E0
```

## Network Policies

### Security Group Rules

```mermaid
graph LR
    subgraph External["External Traffic"]
        Web[Web Clients<br/>Port 8080]
        Metrics[Metrics<br/>Port 9090]
        Admin[Admin Access<br/>Port 9201]
    end

    subgraph Namespace["erlmcp Namespace"]
        Pod1[erlmcp Pods<br/>Port 8080, 9090, 9201]
    end

    subgraph Policies["Network Policies"]
        NP1[Allow Web<br/>Ingress]
        NP2[Allow Metrics<br/>Ingress]
        NP3[Allow Admin<br/>Ingress]
        NP4[Allow Pod-to-Pod<br/>Communication]
        NP5[Deny All Other<br/>Ingress]
    end

    Web -->|Allowed| NP1
    Metrics -->|Allowed| NP2
    Admin -->|Allowed| NP3

    NP1 --> Pod1
    NP2 --> Pod1
    NP3 --> Pod1

    Pod1 --> NP4
    NP4 --> Pod1

    NP5 -.->|Denied| Web
    NP5 -.->|Denied| Metrics

    style External fill:#FFE0B2
    style Namespace fill:#C8E6C9
    style Policies fill:#BBDEFB
```

## Multi-Cluster Deployment

### Federation Across Clusters

```mermaid
graph TB
    subgraph Cluster1["Cluster 1 (us-east-1)"]
        C1Pods[erlmcp StatefulSet<br/>4 Pods]
        C1LB[Load Balancer<br/>ALB]
    end

    subgraph Cluster2["Cluster 2 (us-west-2)"]
        C2Pods[erlmcp StatefulSet<br/>4 Pods]
        C2LB[Load Balancer<br/>ALB]
    end

    subgraph Cluster3["Cluster 3 (eu-west-1)"]
        C3Pods[erlmcp StatefulSet<br/>4 Pods]
        C3LB[Load Balancer<br/>ALB]
    end

    subgraph Federation["Kubernetes Federation"]
        Fed[Federation Control Plane]
        DNS[Global DNS<br/>Route53]
    end

    DNS --> Fed
    Fed -->|Route to closest| C1LB
    Fed -->|Route to closest| C2LB
    Fed -->|Route to closest| C3LB

    C1LB --> C1Pods
    C2LB --> C2Pods
    C3LB --> C3Pods

    C1Pods -.->|Data Sync| C2Pods
    C2Pods -.->|Data Sync| C3Pods
    C3Pods -.->|Data Sync| C1Pods

    style Cluster1 fill:#4CAF50
    style Cluster2 fill:#2196F3
    style Cluster3 fill:#FF9800
    style Federation fill:#9C27B0
```

## GitOps Deployment

### GitOps Workflow with ArgoCD

```mermaid
flowchart LR
    subgraph Git["Git Repository"]
        Manifests[Kubernetes Manifests]
        Config[Configuration<br/>sys.config, vm.args]
    end

    subgraph CI["CI/CD Pipeline"]
        Build[Build Image<br/>Docker Build]
        Push[Push to Registry<br/>Docker Hub]
        Update[Update Git Tag]
    end

    subgraph CD["ArgoCD"]
        Sync[Sync Controller]
        Apply[Apply to Cluster]
        Health[Health Check]
    end

    subgraph Cluster["Kubernetes Cluster"]
        Pods[erlmcp Pods]
    end

    Developer[Developer] -->|Commit| Git
    Git -->|Trigger| Build
    Build --> Push
    Push --> Update
    Update --> Git

    Git -->|Watch| Sync
    Sync --> Apply
    Apply --> Pods
    Apply --> Health
    Health -->|Healthy| Complete([Deployment<br/>Complete])
    Health -->|Unhealthy| Rollback[Auto Rollback]

    Rollback --> Sync

    style Git fill:#F44336
    style CI fill:#2196F3
    style CD fill:#4CAF50
    style Cluster fill:#FF9800
    style Complete fill:#FFD700
```

## References

- **Kubernetes Docs**: https://kubernetes.io/docs/
- **StatefulSets**: https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/
- **HPA**: https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/
- **Cluster Deployment**: [CLUSTER_DEPLOYMENT.md](./CLUSTER_DEPLOYMENT.md)
- **Deployment Guide**: [DEPLOYMENT_GUIDE_100X.md](./DEPLOYMENT_GUIDE_100X.md)

---

**Last Updated**: 2026-01-31
**Version**: 2.1.0
**Maintained By**: erlmcp Team
