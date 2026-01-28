# erlmcp v1.4.0 Deployment Guide

**Release:** January 27, 2026
**Version:** 1.4.0 "Production Hardened"
**Audience:** DevOps, SREs, Infrastructure teams
**Time to Deploy:** 15 minutes (GCP), 5 minutes (Local), 30 minutes (Kubernetes)

---

## Quick Start: Choose Your Path

### Path 1: GCP (Recommended for Production)
- ✅ Managed infrastructure (Compute Engine)
- ✅ Built-in monitoring (Cloud Monitoring)
- ✅ Easy scaling (Instance templates)
- **Time:** ~15 minutes

### Path 2: Local Development
- ✅ Minimal prerequisites (just Erlang/OTP)
- ✅ Full testing capability
- ✅ No infrastructure cost
- **Time:** ~5 minutes

### Path 3: Kubernetes (Advanced HA)
- ✅ Distributed architecture
- ✅ Self-healing
- ✅ Horizontal scaling
- **Time:** ~30 minutes (including cluster setup)

---

## Path 1: GCP Deployment (Production)

### Prerequisites

1. **GCP Account** with billing enabled
2. **gcloud CLI** installed (`brew install google-cloud-sdk`)
3. **Erlang/OTP 25+** installed locally (for building)
4. **Docker** installed locally (for container build)

### Step 1: Create GCP Project & Enable APIs

```bash
# Set project ID
export GCP_PROJECT="erlmcp-prod"
export GCP_REGION="us-central1"

# Create project (if needed)
gcloud projects create $GCP_PROJECT

# Set as default
gcloud config set project $GCP_PROJECT

# Enable required APIs
gcloud services enable compute.googleapis.com
gcloud services enable cloud-build.googleapis.com
gcloud services enable container.googleapis.com
gcloud services enable monitoring.googleapis.com
gcloud services enable logging.googleapis.com
```

### Step 2: Create Compute Engine VM

```bash
# Create VM instance
gcloud compute instances create erlmcp-prod-1 \
  --zone=$GCP_REGION-a \
  --machine-type=e2-standard-4 \
  --image-family=ubuntu-2204-lts \
  --image-project=ubuntu-os-cloud \
  --scopes=cloud-platform \
  --metadata=enable-oslogin=true

# Wait for VM to start
echo "Waiting for VM to start..."
sleep 30

# SSH into VM
gcloud compute ssh erlmcp-prod-1 --zone=$GCP_REGION-a
```

### Step 3: Install Erlang/OTP on VM

```bash
# SSH session (on GCP VM):

# Install dependencies
sudo apt-get update
sudo apt-get install -y build-essential git curl

# Install Erlang/OTP 25
sudo apt-get install -y erlang-base erlang-dev erlang-tools

# Verify installation
erl -eval "erlang:display(erlang:system_info(otp_release)), halt()." -noshell
# Output: "25" (or higher)

# Install rebar3
mkdir -p ~/.local/bin
wget https://s3.amazonaws.com/rebar3/rebar3 -O ~/.local/bin/rebar3
chmod +x ~/.local/bin/rebar3
export PATH="$PATH:$HOME/.local/bin"
```

### Step 4: Clone & Build erlmcp

```bash
# Still in SSH session:

# Clone repository
cd ~
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# Build release
make release
# OR:
rebar3 as prod release

# Verify build
ls -la _build/prod/rel/erlmcp/
```

### Step 5: Configure for GCP

Create `/home/[user]/erlmcp/config/sys.config`:

```erlang
[
  {erlmcp, [
    {server_defaults, #{
      max_subscriptions_per_resource => 1000,
      max_progress_tokens => 10000
    }},
    {transport_defaults, #{
      tcp => #{
        port => 5000,
        bind_address => "0.0.0.0",
        connect_timeout => 5000,
        keepalive => true,
        nodelay => true
      },
      http => #{
        port => 8080,
        bind_address => "0.0.0.0",
        connect_timeout => 5000,
        request_timeout => 30000,
        max_connections => 100
      }
    }}
  ]},

  {kernel, [
    {logger, [
      {handlers, [
        {default, logger_std_h, #{level => info}}
      ]}
    ]},
    {enable_kernel_poll, true}
  ]},

  {sasl, [
    {sasl_error_logger, {file, "logs/sasl.log"}}
  ]},

  {opentelemetry, [
    {span_processors, [
      opentelemetry_batch_span_processor
    ]},
    {traces_exporter, {opentelemetry_exporter, #{
      otlp_endpoint => "http://localhost:4317"
    }}}
  ]}
].
```

### Step 6: Start erlmcp Service

Option A: Run Directly

```bash
# Start erlmcp
_build/prod/rel/erlmcp/bin/erlmcp foreground
# Output: erlmcp@hostname boot complete
```

Option B: Create Systemd Service (Recommended)

```bash
# Create service file
sudo tee /etc/systemd/system/erlmcp.service > /dev/null <<EOF
[Unit]
Description=erlmcp MCP Server
After=network.target
Requires=network.target

[Service]
Type=simple
User=$(whoami)
WorkingDirectory=$HOME/erlmcp
ExecStart=$HOME/erlmcp/_build/prod/rel/erlmcp/bin/erlmcp foreground
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal
SyslogIdentifier=erlmcp

[Install]
WantedBy=multi-user.target
EOF

# Enable and start
sudo systemctl daemon-reload
sudo systemctl enable erlmcp
sudo systemctl start erlmcp

# Check status
sudo systemctl status erlmcp
```

### Step 7: Verify Deployment

```bash
# Health check (from local machine, replace EXTERNAL_IP)
export EXTERNAL_IP=$(gcloud compute instances describe erlmcp-prod-1 --zone=$GCP_REGION-a --format='get(networkInterfaces[0].accessConfigs[0].natIP)')

# Check HTTP port
curl http://$EXTERNAL_IP:8080/health

# Expected: JSON response or connection refused (if no health endpoint, that's OK)

# Check logs
sudo journalctl -u erlmcp -f

# Test with client
# From repo directory:
make test-client-tcp  # If configured for TCP
```

### Step 8: Set Up Monitoring (Cloud Monitoring)

1. **Enable Cloud Monitoring**
   ```bash
   gcloud services enable monitoring.googleapis.com
   ```

2. **Install Monitoring Agent** (on VM via SSH)
   ```bash
   curl https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh | sudo bash
   sudo apt-get update
   sudo apt-get install -y google-cloud-ops-agent
   ```

3. **Configure OTEL Export** in `sys.config`:
   ```erlang
   {opentelemetry, [
     {traces_exporter, {opentelemetry_exporter, #{
       otlp_endpoint => "http://localhost:4317"  % OTEL collector
     }}}
   ]}
   ```

4. **View Metrics in GCP Console**
   - Navigate to Cloud Monitoring
   - Create dashboard
   - Add metrics: `erlmcp_throughput`, `erlmcp_latency`, etc.

### Step 9: Configure Firewall Rules

```bash
# Allow TCP traffic from clients
gcloud compute firewall-rules create erlmcp-tcp \
  --allow=tcp:5000 \
  --direction=INGRESS \
  --source-ranges=0.0.0.0/0 \
  --target-tags=erlmcp

# Allow HTTP traffic
gcloud compute firewall-rules create erlmcp-http \
  --allow=tcp:8080 \
  --direction=INGRESS \
  --source-ranges=0.0.0.0/0 \
  --target-tags=erlmcp

# Apply tags to instance
gcloud compute instances add-tags erlmcp-prod-1 \
  --tags=erlmcp \
  --zone=$GCP_REGION-a
```

### GCP Deployment Complete ✅

**Verification Checklist:**
- [ ] VM created and running
- [ ] erlmcp built and running (check systemd status)
- [ ] Health check passes
- [ ] Firewall rules allow inbound traffic
- [ ] Cloud Monitoring collecting metrics

**Next Steps:**
- Deploy clients using EXTERNAL_IP
- Configure backups (snapshots)
- Set up autoscaling (create instance template)

---

## Path 2: Local Development Deployment

### Prerequisites

1. **Erlang/OTP 25+**
   ```bash
   # macOS
   brew install erlang@25

   # OR Linux
   sudo apt-get install erlang-base erlang-dev
   ```

2. **Make** and **Git**
   ```bash
   which make
   which git
   ```

### Step 1: Clone Repository

```bash
cd ~
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp
```

### Step 2: Compile & Test

```bash
# Clean build
make distclean

# Compile
make compile

# Run full test suite (recommended for validation)
make test

# Output:
# [✓] eunit tests pass
# [✓] ct (common test) tests pass
# [✓] property-based tests pass
# All tests passed: 100+ test cases
```

### Step 3: Start Development Console

```bash
# Start Erlang shell with app loaded
make console

# Erlang prompt appears:
# (erlmcp@hostname)1>
```

### Step 4: Run Example Scenarios

**In another terminal:**

```bash
cd /path/to/erlmcp

# Run simple client/server example
make test-client

# OR run calculator example
make test-calculator

# OR run weather example
make test-weather

# Output shows request/response cycle
```

### Step 5: Run Benchmarks (Optional)

```bash
# Throughput benchmark
make bench-throughput
# Output: 150K msg/sec (or close)

# Scaling benchmark
make bench-100k-registry
# Output: Scaling to 100K connections, linear latency growth

# Chaos scenarios
make chaos-all
# Output: All 8 scenarios pass
```

### Step 6: Configure for Your Use Case

**Edit `config/sys.config`:**

```erlang
[
  {erlmcp, [
    {server_defaults, #{
      max_subscriptions_per_resource => 1000,
      max_progress_tokens => 10000
    }},
    {transport_defaults, #{
      tcp => #{
        port => 5000,
        bind_address => "127.0.0.1",  % localhost only for dev
        connect_timeout => 5000
      },
      http => #{
        port => 8080,
        bind_address => "127.0.0.1"
      }
    }}
  ]}
].
```

**Then reload:**

```bash
# In console:
c(file:read_file_info("config/sys.config")).  % Reload config
```

### Local Development Complete ✅

**You can now:**
- ✅ Write and test Erlang code
- ✅ Run full test suite
- ✅ Benchmark performance locally
- ✅ Debug with Observer CLI

---

## Path 3: Kubernetes Deployment (Advanced HA)

### Prerequisites

1. **Kubernetes cluster** (any provider: GKE, EKS, AKS, or local minikube)
2. **kubectl** installed and configured
3. **Docker** installed (for building container images)
4. **Helm** installed (optional, simplifies deployment)

### Step 1: Build Docker Image

```bash
cd /path/to/erlmcp

# Build image
make docker-build

# Verify image created
docker images | grep erlmcp
# Output: erlmcp  v1.4.0  <ID>  <size>

# Tag for registry (GCP example)
docker tag erlmcp:v1.4.0 gcr.io/$GCP_PROJECT/erlmcp:v1.4.0
docker push gcr.io/$GCP_PROJECT/erlmcp:v1.4.0
```

### Step 2: Create Kubernetes ConfigMap

**File: `k8s/configmap.yaml`**

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-config
  namespace: default
data:
  sys.config: |
    [
      {erlmcp, [
        {server_defaults, #{
          max_subscriptions_per_resource => 1000,
          max_progress_tokens => 10000
        }},
        {transport_defaults, #{
          tcp => #{
            port => 5000,
            bind_address => "0.0.0.0",
            connect_timeout => 5000
          },
          http => #{
            port => 8080,
            bind_address => "0.0.0.0"
          }
        }}
      ]}
    ].
```

### Step 3: Create StatefulSet

**File: `k8s/statefulset.yaml`**

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlmcp
  namespace: default
spec:
  serviceName: erlmcp
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
    spec:
      containers:
      - name: erlmcp
        image: gcr.io/PROJECT/erlmcp:v1.4.0
        imagePullPolicy: IfNotPresent
        ports:
        - containerPort: 5000
          name: tcp
        - containerPort: 8080
          name: http
        env:
        - name: ERLANG_COOKIE
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: cookie
        - name: HOSTNAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        volumeMounts:
        - name: config
          mountPath: /app/config
        - name: data
          mountPath: /app/data
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
        resources:
          requests:
            cpu: "2"
            memory: "4Gi"
          limits:
            cpu: "4"
            memory: "8Gi"
      volumes:
      - name: config
        configMap:
          name: erlmcp-config
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: [ "ReadWriteOnce" ]
      resources:
        requests:
          storage: 10Gi
```

### Step 4: Create Service

**File: `k8s/service.yaml`**

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
  namespace: default
spec:
  clusterIP: None  # Headless service for StatefulSet
  selector:
    app: erlmcp
  ports:
  - port: 5000
    targetPort: 5000
    protocol: TCP
    name: tcp
  - port: 8080
    targetPort: 8080
    protocol: TCP
    name: http
---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-lb
  namespace: default
spec:
  type: LoadBalancer
  selector:
    app: erlmcp
  ports:
  - port: 5000
    targetPort: 5000
    protocol: TCP
    name: tcp
  - port: 8080
    targetPort: 8080
    protocol: TCP
    name: http
```

### Step 5: Deploy to Kubernetes

```bash
# Create secrets (replace with your cookie value)
kubectl create secret generic erlmcp-secrets \
  --from-literal=cookie=$(openssl rand -base64 32)

# Apply manifests
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/statefulset.yaml
kubectl apply -f k8s/service.yaml

# Wait for pods to start
kubectl rollout status statefulset/erlmcp -w

# Check status
kubectl get pods -l app=erlmcp

# Expected output (after a few seconds):
# NAME      READY   STATUS    RESTARTS   AGE
# erlmcp-0  1/1     Running   0          10s
# erlmcp-1  1/1     Running   0          8s
# erlmcp-2  1/1     Running   0          6s
```

### Step 6: Verify Deployment

```bash
# Check service
kubectl get svc erlmcp-lb

# Get external IP
export EXTERNAL_IP=$(kubectl get svc erlmcp-lb -o jsonpath='{.status.loadBalancer.ingress[0].ip}')

# Health check
curl http://$EXTERNAL_IP:8080/health

# Check logs
kubectl logs -l app=erlmcp -f

# Execute command in pod
kubectl exec -it erlmcp-0 -- bash
```

### Step 7: Monitor with Prometheus

**File: `k8s/servicemonitor.yaml`**

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp
  namespace: default
spec:
  selector:
    matchLabels:
      app: erlmcp
  endpoints:
  - port: http
    path: /metrics
    interval: 30s
```

```bash
# Apply if Prometheus operator is installed
kubectl apply -f k8s/servicemonitor.yaml
```

### Kubernetes Deployment Complete ✅

**Verification Checklist:**
- [ ] 3 pods running
- [ ] Health checks passing
- [ ] External IP assigned
- [ ] Can connect from client
- [ ] Metrics exported to Prometheus

**Next Steps:**
- Configure ingress for HTTPS
- Set up HPA (Horizontal Pod Autoscaler)
- Configure persistent storage
- Enable distributed Erlang between nodes

---

## Troubleshooting & Common Issues

### Issue: "Connection Refused" from Client

**Cause:** Firewall or port binding issue

**Resolution:**
```bash
# Check if erlmcp is listening
netstat -tlnp | grep 5000  # TCP port
netstat -tlnp | grep 8080  # HTTP port

# If not listening, check logs
sudo journalctl -u erlmcp -n 50  # Last 50 lines

# Verify binding address in config
grep "bind_address" config/sys.config
# Should be "0.0.0.0" for remote access
```

### Issue: Memory Usage Growing

**Cause:** Possible resource leak or high load

**Resolution:**
```bash
# Check memory in Erlang console
(erlmcp@hostname)1> erlang:memory().
# Look for 'processes' and 'ets' values

# Get process info
(erlmcp@hostname)1> erlang:processes_info().

# Force garbage collection (if test mode)
(erlmcp@hostname)1> erlang:garbage_collect().

# If persistent: Check handler cleanup
# Review /docs/operations/lifecycle.md
```

### Issue: High Latency or Timeouts

**Cause:** Overload, bad handler, or network issue

**Resolution:**
```bash
# Check OTEL metrics
curl http://localhost:8080/metrics | grep erlmcp_latency

# Check handler execution time
(erlmcp@hostname)1> erlmcp_metrics:get_handler_stats().

# If handler slow: Optimize handler function
# See /docs/performance_tuning.md

# Enable debug logging
(erlmcp@hostname)1> logger:set_module_level(erlmcp_server, debug).
```

### Issue: Kubernetes Pod Keep Restarting

**Cause:** Liveness probe failing

**Resolution:**
```bash
# Check pod logs
kubectl logs erlmcp-0 --previous

# Check events
kubectl describe pod erlmcp-0

# Increase initial delay
# Edit statefulset: initialDelaySeconds: 60
kubectl set env statefulset erlmcp ...  # Or edit manually

# Rebuild image if necessary
make docker-build
docker push gcr.io/$PROJECT/erlmcp:v1.4.0
kubectl delete pods -l app=erlmcp  # Force restart
```

---

## Post-Deployment Validation

Run these checks after deployment to verify everything works:

```bash
# 1. Connectivity test
cd /path/to/erlmcp
make test-client  # Should connect and show requests

# 2. Performance baseline
make bench-throughput

# 3. Chaos test (optional, production caution)
# make chaos-registry-crash  # Don't run in prod without planning!

# 4. Monitoring check
# Verify metrics appear in GCP Console or Prometheus

# 5. Log check
# Grep logs for ERRORS or CRITICAL

# 6. Resource usage
# Check memory, CPU, connections in monitoring
```

---

## Scaling Guidelines

### Single Node
- **Connections:** Up to 100K
- **Throughput:** 150K msg/sec
- **Memory:** ~4-8GB

### Multi-Node (Kubernetes)
- **Connections:** 100K per node × number of nodes
- **Throughput:** 150K per node
- **Load Balancer:** Distribute clients across nodes
- **Message Routing:** Via distributed Erlang (optional)

### Autoscaling (GCP)
```bash
# Create instance template
gcloud compute instance-templates create erlmcp-template \
  --machine-type=e2-standard-4 \
  --image-family=ubuntu-2204-lts \
  --scopes=cloud-platform \
  --metadata-from-file=startup-script=startup.sh

# Create autoscaling group
gcloud compute instance-groups managed create erlmcp-group \
  --base-instance-name=erlmcp \
  --template=erlmcp-template \
  --size=3

# Set autoscaling policy
gcloud compute instance-groups managed set-autoscaling erlmcp-group \
  --max-num-replicas=10 \
  --min-num-replicas=3 \
  --target-cpu-utilization=0.7
```

---

## Backup & Recovery

### GCP Snapshots
```bash
# Create snapshot
gcloud compute disks snapshot erlmcp-prod-1 \
  --snapshot-names=erlmcp-backup-$(date +%Y%m%d)

# List snapshots
gcloud compute snapshots list

# Restore from snapshot
gcloud compute instances create erlmcp-restore \
  --source-snapshot=erlmcp-backup-20260127
```

### Kubernetes Data
```bash
# PersistentVolume is in StatefulSet
# Automated backups via:
# - Cloud SQL automated backups (if using)
# - Velero for full cluster backups (recommended)

kubectl install velero ...  # See Velero docs
```

---

## Support & Resources

- **Documentation:** `/docs/`
- **Examples:** `/examples/`
- **Issues:** GitHub issues repository
- **Community:** Discussions on GitHub

---

**Version:** 1.4.0
**Date:** 2026-01-27
**Status:** ✅ APPROVED FOR PRODUCTION DEPLOYMENT
