# ERLMCP v3 Scalability Implementation Guide
## Practical Implementation for Fortune 500 Scale

## Overview

This guide provides step-by-step implementation details for deploying erlmcp v3 at enterprise scale, covering infrastructure setup, configuration, monitoring, and optimization.

## 1. Infrastructure Setup

### Kubernetes Cluster Configuration

```yaml
# k8s/erlmcp-cluster.yaml
apiVersion: v1
kind: Cluster
metadata:
  name: erlmcp-enterprise
spec:
  clusterNetwork:
    pods:
      cidrBlocks: ["10.244.0.0/16"]
    services:
      cidrBlocks: ["10.96.0.0/12"]
  controlPlaneEndpoint:
    host: erlmcp-control.example.com
    port: 6443
  infrastructure:
    type: cloud
    cloudConfig:
      provider: aws
      region: us-east-1
  etcd:
    local:
      diskSize: 100Gi
  controlPlane:
    machineType: m6i.xlarge
    replicas: 3
  workers:
    machineType: m6i.2xlarge
    replicas: 10
```

### Node Pools for Different Workloads

```yaml
# k8s/node-pools.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-node-pools
data:
  pools.yaml: |
    pools:
      - name: session-pool
        spec:
          replicas: 20
          minReplicas: 5
          maxReplicas: 50
          machineType: m6i.2xlarge
          labels:
            pool: session
          taints:
            - key: "erlmcp/purpose"
              value: "session"
              effect: "NoSchedule"

      - name: compute-pool
        spec:
          replicas: 10
          minReplicas: 3
          maxReplicas: 30
          machineType: m6i.4xlarge
          labels:
            pool: compute
          taints:
            - key: "erlmcp/purpose"
              value: "compute"
              effect: "NoSchedule"

      - name: storage-pool
        spec:
          replicas: 5
          minReplicas: 2
          maxReplicas: 10
          machineType: m6i.8xlarge
          labels:
            pool: storage
          taints:
            - key: "erlmcp/purpose"
              value: "storage"
              effect: "NoSchedule"
```

### Persistent Storage Configuration

```yaml
# k8s/storage.yaml
apiVersion: v1
kind: StorageClass
metadata:
  name: erlmcp-fast-sc
provisioner: kubernetes.io/aws-ebs
parameters:
  type: gp3
  iops: "16000"
  throughput: "1000"
reclaimPolicy: Delete
volumeBindingMode: WaitForFirstConsumer
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: erlmcp-data
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: erlmcp-fast-sc
  resources:
    requests:
      storage: 100Gi
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: erlmcp-sessions
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: erlmcp-fast-sc
  resources:
    requests:
      storage: 1Ti
```

## 2. Configuration Management

### Application Configuration

```erlang
% config/scalability.config
{scaling, [
    {horizontal_scaling, [
        {max_nodes, 100},
        {min_nodes, 3},
        {scaling_thresholds, [
            {cpu_threshold, 80},
            {memory_threshold, 85},
            {connection_threshold, 50000},
            {queue_threshold, 10000}
        ]},
        {scaling_cooldown, 300}, %% 5 minutes
        {batch_scaling, true},
        {batch_size, 5}
    ]},
    {session_management, [
        {shard_count, 16},
        {replication_factor, 3},
        {session_timeout, 3600},
        {max_sessions_per_node, 5000},
        {session_migration_enabled, true}
    ]},
    {caching, [
        {cache_strategy, hierarchical},
        {local_cache_size, 1000},
        {distributed_cache_ttl, 3600},
        {cache_invalidation_mode, event_driven}
    ]}
]}.

% network.config
{network, [
    {connection_pool, [
        {max_connections, 10000},
        {max_idle_time, 30000},
        {keep_alive, true},
        {tcp_cork, true}
    ]},
    {protocols, [
        {http_version, 2},
        {websocket_compression, true},
        {request_timeout, 30000}
    ]},
    {load_balancing, [
        {algorithm, least_connections},
        {health_check_interval, 5000},
        {connection_timeout, 10000}
    ]}
]}.
```

### Kubernetes Deployment Configuration

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-session
  labels:
    app: erlmcp
    component: session
spec:
  replicas: 20
  selector:
    matchLabels:
      app: erlmcp
      component: session
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 25%
      maxSurge: 25%
  template:
    metadata:
      labels:
        app: erlmcp
        component: session
    spec:
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
          - labelSelector:
              matchExpressions:
              - key: app
                operator: In
                values:
                - erlmcp
            topologyKey: "kubernetes.io/hostname"
        nodeAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
            nodeSelectorTerms:
            - matchExpressions:
              - key: pool
                operator: In
                values:
                - session
      containers:
      - name: erlmcp
        image: erlmcp:latest
        ports:
        - containerPort: 8080
          name: http
          protocol: TCP
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
          limits:
            memory: "4Gi"
            cpu: "2000m"
        env:
        - name: ERLMCP_CLUSTER_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.labels['app']
        - name: ERLMCP_NODE_NAME
          valueFrom:
          fieldRef:
            fieldPath: metadata.name
        - name: ERLMCP_CONFIG_PATH
          value: "/etc/erlmcp"
        volumeMounts:
        - name: config
          mountPath: /etc/erlmcp
        - name: data
          mountPath: /data
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
          initialDelaySeconds: 5
          periodSeconds: 5
      volumes:
      - name: config
        configMap:
          name: erlmcp-config
      - name: data
        persistentVolumeClaim:
          claimName: erlmcp-sessions
```

## 3. Monitoring and Observability

### Prometheus Configuration

```yaml
# monitoring/prometheus.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-config
data:
  prometheus.yml: |
    global:
      scrape_interval: 15s
      evaluation_interval: 15s
    alerting:
      alertmanagers:
      - static_configs:
        - targets:
          - alertmanager:9093
    rule_files:
    - "alert_rules.yml"
    scrape_configs:
    - job_name: 'erlmcp'
      kubernetes_sd_configs:
      - role: pod
      relabel_configs:
      - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
        action: keep
        regex: true
      - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_port]
        action: replace
        target_label: __address__
        regex: (.+)
      - source_labels: [__meta_kubernetes_pod_label_component]
        action: replace
        target_label: component
      - source_labels: [__meta_kubernetes_pod_label_app]
        action: replace
        target_label: app
```

### Alert Rules

```yaml
# monitoring/alert_rules.yml
groups:
- name: erlmcp_alerts
  rules:
  - alert: HighCPUUsage
    expr: 100 * (1 - avg by (app, component) (rate(container_cpu_usage_seconds_total[5m])) / sum by (app, component) (container_spec_cpu_quota{image!="", container!="POD"}))
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "High CPU usage on {{ $labels.component }} pod"
      description: "CPU usage is {{ $value }}% for more than 5 minutes"

  - alert: MemoryUsageHigh
    expr: 100 * (container_memory_working_set_bytes / container_spec_memory_limit_bytes)
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "High memory usage on {{ $labels.component }} pod"
      description: "Memory usage is {{ $value }}% for more than 5 minutes"

  - alert: ResponseTimeHigh
    expr: histogram_quantile(0.95, sum(rate(http_request_duration_seconds_bucket[5m])) by (le, app, component))
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High response time on {{ $labels.component }} pod"
      description: "95th percentile response time is {{ $value }}s"

  - alert: ErrorRateHigh
    expr: sum(rate(http_requests_total{status=~"5.."}[5m])) by (app, component) / sum(rate(http_requests_total[5m])) by (app, component) * 100
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "High error rate on {{ $labels.component }} pod"
      description: "Error rate is {{ $value }}% for more than 5 minutes"
```

### Grafana Dashboards

```json
{
  "dashboard": {
    "title": "ERLMCP Enterprise Dashboard",
    "panels": [
      {
        "title": "CPU Usage by Component",
        "type": "graph",
        "targets": [
          {
            "expr": "100 * (1 - avg by (app, component) (rate(container_cpu_usage_seconds_total[5m])) / sum by (app, component) (container_spec_cpu_quota{image!=\"\", container!=\"POD\"}))",
            "legendFormat": "{{ component }}"
          }
        ],
        "grid": {
          "thresholds": [
            "80",
            "90"
          ]
        }
      },
      {
        "title": "Memory Usage",
        "type": "graph",
        "targets": [
          {
            "expr": "container_memory_working_set_bytes / container_spec_memory_limit_bytes * 100",
            "legendFormat": "{{ component }}"
          }
        ]
      },
      {
        "title": "Response Time",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, sum(rate(http_request_duration_seconds_bucket[5m])) by (le, component))",
            "legendFormat": "{{ component }}"
          }
        ]
      },
      {
        "title": "Active Sessions",
        "type": "stat",
        "targets": [
          {
            "expr": "sum(erlmcp_active_sessions) by (component)"
          }
        ]
      },
      {
        "title": "Throughput",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(rate(http_requests_total[5m])) by (component)",
            "legendFormat": "{{ component }}"
          }
        ]
      }
    ]
  }
}
```

## 4. Performance Testing

### Load Testing Configuration

```erlang
% test/load_test.config
{load_test, [
    {scenarios, [
        {
            name, peak_load_scenario,
            duration, 3600, %% 1 hour
            rps, 50000,
            users, 10000,
            ramp_up, 300, %% 5 minutes
            operations, [
                {
                    name, mcp_request,
                    weight, 70,
                    params, #{
                        tool => echo,
                        arguments => #{
                            message => "Hello World"
                        }
                    }
                },
                {
                    name, session_create,
                    weight, 20,
                    params, #{
                        timeout => 3600
                    }
                },
                {
                    name, resource_subscribe,
                    weight, 10,
                    params, #{
                        resource => "/test/resource",
                        interval => 1000
                    }
                }
            ]
        }
    ]},
    {reporting, [
        {metrics, [
            {response_time, p99},
            {throughput, per_second},
            {error_rate, percentage},
            {connection_count, current}
        ]},
        {output, [
            {file, "load_test_report.json"},
            {prometheus, true},
            {console, true}
        ]}
    ]}
]}.
```

### Benchmark Script

```bash
#!/bin/bash
# scripts/run_benchmark.sh

set -e

# Configuration
CLUSTER_NAME="erlmcp-enterprise"
NAMESPACE="erlmcp"
CONCURRENT_USERS=10000
RPS=50000
DURATION=3600

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Setup
log_info "Setting up benchmark environment..."

# Check dependencies
if ! command -v kubectl &> /dev/null; then
    log_error "kubectl not found"
    exit 1
fi

if ! command -v erlc &> /dev/null; then
    log_error "erlc not found"
    exit 1
fi

# Get cluster endpoint
CLUSTER_ENDPOINT=$(kubectl get svc erlmcp-ingress -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')
if [ -z "$CLUSTER_ENDPOINT" ]; then
    log_error "Could not get cluster endpoint"
    exit 1
fi

log_info "Cluster endpoint: $CLUSTER_ENDPOINT"

# Prepare test data
log_info "Preparing test data..."
mkdir -p benchmark/results
mkdir -p benchmark/logs

# Run load test
log_info "Starting load test..."
log_info "Concurrent users: $CONCURRENT_USERS"
log_info "Requests per second: $RPS"
log_info "Duration: $DURATION seconds"

# Start prometheus recording
kubectl apply -f benchmark/prometheus-benchmark.yaml

# Run benchmark
erlc -o benchmark/ebin benchmark/load_test.erl
erl -pa benchmark/ebin -eval "erlmcp_benchmark:start($CONCURRENT_USERS, $RPS, $DURATION)" -noshell -s init stop

# Collect results
log_info "Collecting benchmark results..."
kubectl cp $NAMESPACE/prometheus-0:/prometheus benchmark/results/ -c prometheus

# Generate report
log_info "Generating benchmark report..."
erlc -o benchmark/ebin benchmark/report.erl
erl -pa benchmark/ebin -eval "erlmcp_report:generate()" -noshell -s init stop

# Cleanup
log_info "Cleaning up..."
kubectl delete -f benchmark/prometheus-benchmark.yaml

# Show results
echo ""
log_info "Benchmark completed successfully!"
echo "Results available in: benchmark/results/"
echo "Report available in: benchmark/report.html"
```

## 5. Deployment Automation

### CI/CD Pipeline

```yaml
# .github/workflows/deploy.yaml
name: Deploy ERLMCP Enterprise
on:
  push:
    branches: [ main ]
    paths:
      - 'apps/erlmcp_core/**'
      - 'config/**'
      - 'k8s/**'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Erlang
      uses: erlef/setup-beam@v1
      with:
        otp-version: 28.3.1

    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: |
          deps
          _build
        key: ${{ runner.os }}-erlang-${{ hashFiles('rebar.config.lock') }}

    - name: Compile
      run: |
        make compile

    - name: Run tests
      run: |
        make test

    - name: Build Docker image
      run: |
        docker build -t erlmcp:latest .

    - name: Push to registry
      run: |
        echo ${{ secrets.DOCKER_PASSWORD}} | docker login -u ${{ secrets.DOCKER_USERNAME}} --password-stdin
        docker tag erlmcp:latest docker.io/${{ secrets.DOCKER_USERNAME}}/erlmcp:latest
        docker push docker.io/${{ secrets.DOCKER_USERNAME}}/erlmcp:latest

  deploy:
    needs: build
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup kubectl
      uses: azure/setup-kubectl@v3
      with:
        version: 'v1.25.0'

    - name: Configure kubeconfig
      run: |
        echo "${{ secrets.KUBE_CONFIG}}" | base64 -d > ~/.kube/config

    - name: Deploy to staging
      run: |
        kubectl config use-context staging
        kubectl apply -f k8s/

    - name: Run smoke tests
      run: |
        kubectl wait --for=condition=available --timeout=300s deployment/erlmcp-session -n erlmcp
        kubectl exec -it deployment/erlmcp-session -n erlmcp -- erl -eval "application:start(erlmcp_core), erlmcp_client:ping()" -noshell

    - name: Promote to production
      if: success()
      run: |
        kubectl config use-context production
        kubectl apply -f k8s/
```

### Zero-Downtime Deployment Script

```bash
#!/bin/bash
# scripts/zero-downtime-deploy.sh

set -e

NAMESPACE="erlmcp"
APP_NAME="erlmcp"
IMAGE_TAG=$1

if [ -z "$IMAGE_TAG" ]; then
    echo "Usage: $0 <image-tag>"
    exit 1
fi

log_info "Starting zero-downtime deployment of $IMAGE_TAG..."

# Get current deployment
CURRENT_DEPLOYMENT=$(kubectl get deployment erlmcp-session -n $NAMESPACE -o json)

# Create new deployment
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-session-new
  namespace: $NAMESPACE
  labels:
    app: erlmcp
    component: session
    version: $IMAGE_TAG
spec:
  replicas: 20
  selector:
    matchLabels:
      app: erlmcp
      component: session
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 25%
      maxSurge: 25%
  template:
    metadata:
      labels:
        app: erlmcp
        component: session
        version: $IMAGE_TAG
    spec:
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
          - labelSelector:
              matchExpressions:
              - key: app
                operator: In
                values:
                - erlmcp
            topologyKey: "kubernetes.io/hostname"
      containers:
      - name: erlmcp
        image: docker.io/erlmcp/erlmcp:$IMAGE_TAG
        ports:
        - containerPort: 8080
          name: http
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
        env:
        - name: ERLMCP_CLUSTER_NAME
          value: "production"
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
---
EOF

# Wait for new deployment to be ready
log_info "Waiting for new deployment to be ready..."
kubectl wait --for=condition=available --timeout=300s deployment/erlmcp-session-new -n $NAMESPACE

# Drain old pods
log_info "Draining old pods..."
OLD_PODS=$(kubectl get pods -n $NAMESPACE -l app=erlmcp,component=session,version!=$IMAGE_TAG -o jsonpath='{.items[*].metadata.name}')

for pod in $OLD_PODS; do
    log_info "Draining $pod..."
    kubectl drain $pod --ignore-daemonsets --delete-emptydir-data --force
    kubectl delete pod $pod --ignore-not-found --now
done

# Update service to point to new deployment
log_info "Updating service..."
kubectl set selector service/erlmcp -n $NAMESPACE app=erlmcp,component=session,version=$IMAGE_TAG

# Verify deployment
log_info "Verifying deployment..."
kubectl wait --for=condition=available --timeout=300s deployment/erlmcp-session -n $NAMESPACE

# Cleanup
log_info "Cleaning up old deployments..."
kubectl delete deployment erlmcp-session-old --ignore-not-found --now

log_info "Deployment completed successfully!"
```

## 6. Cost Optimization

### Cost Monitoring Dashboard

```yaml
# cost-monitoring/grafana-dashboard.json
{
  "dashboard": {
    "title": "ERLMCP Cost Optimization",
    "panels": [
      {
        "title": "Cluster Cost by Component",
        "type": "piechart",
        targets": [
          {
            "expr": "sum(kube_node_info) by (component)",
            "legendFormat": "{{ component }}"
          }
        ]
      },
      {
        "title": "Cost Trend",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(cost_per_hour) by (component)",
            "legendFormat": "{{ component }}"
          }
        ]
      },
      {
        "title": "CPU Cost Efficiency",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(cpu_usage_cost) / sum(cpu_allocation_cost)",
            "legendFormat": "{{ component }}"
          }
        ]
      },
      {
        "title": "Recommendations",
        "type": "table",
        "targets": [
          {
            "expr": "cost_recommendations",
            "legendFormat": "{{ recommendation }}"
          }
        ]
      }
    ]
  }
}
```

### Spot Instance Configuration

```yaml
# k8s/spot-instance.yaml
apiVersion: v1
kind: ClusterAutoscaler
metadata:
  name: erlmcp-cluster-autoscaler
spec:
  scaleDown:
    enabled: true
    delayAfterAdd: "10m"
    delayAfterDelete: "2m"
    idleTime: "10m"
    policies:
    - type: pods
      value: 1
      timeWindow: "10m"
  scaleUp:
    enabled: true
    delayAfterAdd: "10m"
    delayAfterFailure: "5m"
    policies:
    - type: pods
      value: 3
      timeWindow: "10m"
  balanceSimilarNodeGroups: true
  expander: most-pods
  ignoreDaemonPods: true
  nodeGroupAutoscaling:
  - cloudProvider: aws
    clusterName: erlmcp-enterprise
    maxSize: 100
    minSize: 3
    nodeLabels:
      purpose: compute
    nodeGroup: erlmcp-compute-nodes
    zones:
    - us-east-1a
    - us-east-1b
    - us-east-1c
    useSpot: true
```

## 7. Disaster Recovery

### Backup Configuration

```yaml
# backup/backup-cronjob.yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: erlmcp-backup
  namespace: erlmcp
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: backup
            image: postgres:13
            command:
            - /bin/sh
            - -c
            - |
              pg_dump -h erlmcp-db-primary -U erlmcp -d erlmcp > /backup/erlmcp-$(date +%Y%m%d).sql
              aws s3 cp /backup/erlmcp-$(date +%Y%m%d).sql s3://erlmcp-backup/$(date +%Y%m%d)/
            volumeMounts:
            - name: backup
              mountPath: /backup
          restartPolicy: OnFailure
          volumes:
          - name: backup
            emptyDir: {}
```

### Disaster Recovery Drill Script

```bash
#!/bin/bash
# scripts/drill-disaster-recovery.sh

set -e

log_info "Starting disaster recovery drill..."

# Step 1: Simulate primary region failure
log_info "Step 1: Simulating primary region failure..."
kubectl config use-context primary-failover
kubectl scale deployment erlmcp-session --replicas=0

# Step 2: Verify failover to secondary region
log_info "Step 2: Verifying failover..."
kubectl config use-context secondary
kubectl wait --for=condition=available deployment erlmcp-session

# Step 3: Run load test against secondary
log_info "Step 3: Running load test against secondary..."
./scripts/run-benchmark.sh 10000 10000 300

# Step 4: Verify data consistency
log_info "Step 4: Verifying data consistency..."
kubectl exec -it deployment/erlmcp-session -- erl -eval "
    % Check session consistency
    Sessions = erlmcp_session_manager:get_all(),
    io:format(\"Total sessions: ~p~n\", [length(Sessions)]),
    ok.
" -noshell

# Step 5: Run back to primary
log_info "Step 5: Running back to primary..."
kubectl config use-context primary
kubectl apply -f k8s/

# Step 6: Verify primary is operational
log_info "Step 6: Verifying primary is operational..."
kubectl wait --for=condition=available deployment erlmcp-session

log_info "Disaster recovery drill completed successfully!"
```

## 8. Troubleshooting

### Common Issues and Solutions

```bash
# scripts/troubleshoot.sh
#!/bin/bash

show_usage() {
    echo "Usage: $0 <issue>"
    echo "Issues:"
    echo "  connections     - Check connection issues"
    echo "  performance     - Check performance issues"
    echo "  scaling         - Check scaling issues"
    echo "  memory          - Check memory issues"
    echo "  network         - Check network issues"
}

if [ $# -eq 0 ]; then
    show_usage
    exit 1
fi

case $1 in
    connections)
        echo "=== Connection Issues ==="
        kubectl get svc -n erlmcp
        kubectl get endpoints -n erlmcp
        kubectl describe svc erlmcp -n erlmcp
        ;;
    performance)
        echo "=== Performance Issues ==="
        kubectl top pods -n erlmcp
        kubectl describe pod -n erlmcp -l app=erlmcp | grep -i "memory\|cpu"
        ;;
    scaling)
        echo "=== Scaling Issues ==="
        kubectl get hpa -n erlmcp
        kubectl describe hpa erlmcp-hpa -n erlmcp
        ;;
    memory)
        echo "=== Memory Issues ==="
        kubectl describe pod -n erlmcp -l app=erlmcp | grep -A 10 -i "memory"
        kubectl get events -n erlmcp --sort-by='.metadata.creationTimestamp'
        ;;
    network)
        echo "=== Network Issues ==="
        kubectl describe pod -n erlmcp -l app=erlmcp | grep -i "network\|pod"
        kubectl get networkpolicy -n erlmcp
        ;;
    *)
        echo "Unknown issue: $1"
        show_usage
        exit 1
        ;;
esac
```

### Health Check Script

```erlang
% scripts/health_check.erl
-module(health_check).
-export([run_checks/0]).

run_checks() ->
    %% Check Erlang node
    case net_adm:ping(node()) of
        pang ->
            io:format("Error: Erlang node not available~n");
        pong ->
            io:format("OK: Erlang node available~n")
    end,

    %% Check ERLMCP services
    case application:get_application(erlmcp_core) of
        undefined ->
            io:format("Error: ERLMCP Core not running~n");
        {ok, _} ->
            io:format("OK: ERLMCP Core running~n")
    end,

    %% Check sessions
    case erlmcp_session_manager:count_sessions() of
        {ok, Count} ->
            io:format("OK: ~p active sessions~n", [Count]);
        {error, Reason} ->
            io:format("Error: Session count - ~p~n", [Reason])
    end,

    %% Check registry
    case erlmcp_registry:status() of
        {ok, Size} ->
            io:format("OK: Registry size ~p~n", [Size]);
        {error, Reason} ->
            io:format("Error: Registry - ~p~n", [Reason])
    end,

    %% Check database connectivity
    case erlmcp_db:ping() of
        pong ->
            io:format("OK: Database available~n");
        pang ->
            io:format("Error: Database not available~n")
    end,

    io:format("Health check completed~n").
```

## 9. Best Practices

### Deployment Best Practices

1. **Blue-Green Deployments**: Always maintain two versions
2. **Canary Releases**: Gradually roll out to production
3. **Rolling Updates**: Minimize downtime during updates
4. **Health Checks**: Implement proper readiness and liveness probes
5. **Auto-Scaling**: Configure appropriate scaling thresholds
6. **Resource Limits**: Set CPU and memory limits for all pods
7. **Persistent Volumes**: Use appropriate storage classes
8. **Security**: Implement network policies and RBAC
9. **Monitoring**: Comprehensive monitoring and alerting
10. **Documentation**: Maintain up-to-date documentation

### Security Best Practices

1. **Network Security**: Implement proper network policies
2. **Authentication**: Use OAuth/OIDC for authentication
3. **Encryption**: Encrypt data in transit and at rest
4. **Secrets Management**: Use Kubernetes secrets or external vault
5. **Audit Logging**: Implement comprehensive audit logging
6. **Regular Updates**: Keep all components up to date
7. **Vulnerability Scanning**: Regular vulnerability scanning
8. **Backup and Recovery**: Regular backups and disaster recovery drills
9. **Compliance**: Maintain compliance with relevant standards
10. **Incident Response**: Establish incident response procedures

### Performance Best Practices

1. **Load Testing**: Regular performance testing
2. **Caching**: Implement appropriate caching strategies
3. **Connection Pooling**: Use connection pooling for databases
4. **Database Optimization**: Regular database maintenance
5. **CDN Usage**: Use CDN for static content
6. **Compression**: Enable compression for text content
7. **Monitoring**: Monitor key performance metrics
8. **Benchmarking**: Regular benchmarking against targets
9. **Profiling**: Regular code profiling
10. **Optimization**: Continuous performance optimization

This implementation guide provides a comprehensive blueprint for deploying erlmcp v3 at enterprise scale, covering all aspects from infrastructure setup to optimization and maintenance.