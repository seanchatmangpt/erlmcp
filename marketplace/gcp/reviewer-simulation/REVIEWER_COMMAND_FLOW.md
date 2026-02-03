# Google Marketplace Reviewer Command Flow

**Solution**: erlmcp v3 Enterprise
**Review Date**: 2025-02-02
**Reviewer Test Project**: reviewer-test-1770096612

---

## Complete Command Flow

This document shows the exact commands that Google Marketplace reviewers execute when testing the erlmcp v3 Enterprise solution.

---

## Phase 0: Project Bootstrap

### Step 1: Authenticate with GCP

```bash
# Check authentication status
gcloud auth list

# If not authenticated
gcloud auth login

# Set active account
gcloud config set account reviewer@example.com
```

### Step 2: Create Test Project

```bash
# Create new project
gcloud projects create reviewer-test-1770096612 \
  --name="Marketplace Reviewer Test - 2025-02-02" \
  --organization-id=123456789 \
  --set-as-default

# Verify project creation
gcloud projects describe reviewer-test-1770096612
```

### Step 3: Link Billing Account

```bash
# List available billing accounts
gcloud beta billing accounts list

# Link billing account
gcloud beta billing projects link reviewer-test-1770096612 \
  --billing-account=AAAAAA-BBBBBB-CCCCCC

# Verify billing is enabled
gcloud beta billing projects describe reviewer-test-1770096612
```

### Step 4: Configure Project Settings

```bash
# Set default region/zone
gcloud config set project reviewer-test-1770096612
gcloud config set compute/region us-central1
gcloud config set compute/zone us-central1-a

# Verify configuration
gcloud config list
```

### Step 5: Create Service Account

```bash
# Create service account
gcloud iam service-accounts create marketplace-reviewer-sa \
  --display-name="Marketplace Reviewer Service Account" \
  --description="Service account for automated testing of marketplace solution"

# Grant editor role
gcloud projects add-iam-policy-binding reviewer-test-1770096612 \
  --member="serviceAccount:marketplace-reviewer-sa@reviewer-test-1770096612.iam.gserviceaccount.com" \
  --role="roles/editor"

# Grant service account user role
gcloud iam service-accounts add-iam-policy-binding \
  marketplace-reviewer-sa@reviewer-test-1770096612.iam.gserviceaccount.com \
  --member="user:reviewer@example.com" \
  --role="roles/iam.serviceAccountUser"
```

---

## Phase 1: Prerequisites Validation

### Step 1: Verify GCP CLI Version

```bash
# Check gcloud version
gcloud version --format='value(version)'
# Expected: >= 400.0.0
```

### Step 2: Verify Docker Installation

```bash
# Check docker version
docker --version
# Expected: >= 20.10.0

# Verify docker daemon is running
docker info
```

### Step 3: Verify kubectl Installation

```bash
# Check kubectl version
kubectl version --client --short
# Expected: >= 1.24.0
```

### Step 4: Verify Helm Installation

```bash
# Check helm version
helm version --template '{{.Version}}'
# Expected: >= 3.0.0
```

### Step 5: Verify Terraform Installation

```bash
# Check terraform version
terraform version -json | jq -r '.terraform_version'
# Expected: >= 1.3.0
```

### Step 6: Verify Required Tools

```bash
# Check make
make --version

# Check jq
jq --version

# Check curl
curl --version
```

### Step 7: Verify Permissions

```bash
# Test compute permission
gcloud compute instances list --project=reviewer-test-1770096612 --limit=0

# Test storage permission
gsutil ls -p reviewer-test-1770096612

# Test container permission
gcloud container clusters list --project=reviewer-test-1770096612
```

---

## Phase 2: API Enablement

### Step 1: Enable Compute APIs

```bash
gcloud services enable \
  container.googleapis.com \
  compute.googleapis.com \
  run.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 2: Enable Storage APIs

```bash
gcloud services enable \
  storage-component.googleapis.com \
  storage-api.googleapis.com \
  file.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 3: Enable Networking APIs

```bash
gcloud services enable \
  containersecurity.googleapis.com \
  networkservices.googleapis.com \
  trafficdirector.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 4: Enable Operations APIs

```bash
gcloud services enable \
  monitoring.googleapis.com \
  logging.googleapis.com \
  trace.googleapis.com \
  debugger.googleapis.com \
  cloudprofiler.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 5: Enable Security APIs

```bash
gcloud services enable \
  secretmanager.googleapis.com \
  cloudkms.googleapis.com \
  iam.googleapis.com \
  iamcredentials.googleapis.com \
  binaryauthorization.googleapis.com \
  accessapproval.googleapis.com \
  accesscontextmanager.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 6: Enable Database APIs

```bash
gcloud services enable \
  sql-component.googleapis.com \
  sqladmin.googleapis.com \
  firestore.googleapis.com \
  redis.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 7: Enable Management APIs

```bash
gcloud services enable \
  deploymentmanager.googleapis.com \
  cloudresourcemanager.googleapis.com \
  cloudbuild.googleapis.com \
  cloudfunctions.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 8: Enable Eventing APIs

```bash
gcloud services enable \
  pubsub.googleapis.com \
  eventarc.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 9: Enable Observability APIs

```bash
gcloud services enable \
  opsconfig.googleapis.com \
  osconfig.googleapis.com \
  oslogin.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 10: Enable Advanced Networking APIs

```bash
gcloud services enable \
  servicemanagement.googleapis.com \
  servicecontrol.googleapis.com \
  servicenetworking.googleapis.com \
  policytroubleshooter.googleapis.com \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 11: Wait for APIs to Activate

```bash
# Monitor API operations
watch -n 5 'gcloud operations list \
  --project=reviewer-test-1770096612 \
  --filter="operationType:enableServices" \
  --format="table(name,status,startTime)"'

# Wait for all operations to complete
gcloud services wait \
  --project=reviewer-test-1770096612 \
  --timeout=600
```

### Step 12: Verify API Status

```bash
# List all enabled APIs
gcloud services list --enabled --project=reviewer-test-1770096612

# Verify specific API
gcloud services describe container.googleapis.com \
  --project=reviewer-test-1770096612

# Count enabled APIs
gcloud services list --enabled --project=reviewer-test-1770096612 | wc -l
# Expected: 42+ APIs enabled
```

---

## Phase 3: Environment Preparation

### Step 1: Create Artifact Registry Repository

```bash
# Create Docker repository
gcloud artifacts repositories create erlmcp-repo \
  --repository-format=docker \
  --location=us-central1 \
  --description="erlmcp v3 container images" \
  --project=reviewer-test-1770096612

# Verify repository
gcloud artifacts repositories list \
  --location=us-central1 \
  --project=reviewer-test-1770096612
```

### Step 2: Configure Docker Authentication

```bash
# Configure docker for artifact registry
gcloud auth configure-docker us-central1-docker.pkg.dev

# Verify authentication
docker login us-central1-docker.pkg.dev
```

### Step 3: Create GKE Cluster

```bash
# Create cluster (this would be in deployment phase)
gcloud container clusters create erlmcp-test-cluster \
  --zone=us-central1-a \
  --num-nodes=3 \
  --machine-type=e2-medium \
  --image-type=COS_CONTAINERD \
  --disk-type=pd-standard \
  --disk-size=100GB \
  --network=default \
  --subnetwork=default \
  --enable-autoscaling \
  --min-nodes=1 \
  --max-nodes=5 \
  --enable-autoupgrade \
  --enable-autorepair \
  --project=reviewer-test-1770096612
```

### Step 4: Get Cluster Credentials

```bash
# Get cluster credentials
gcloud container clusters get-credentials erlmcp-test-cluster \
  --zone=us-central1-a \
  --project=reviewer-test-1770096612

# Verify kubectl can access cluster
kubectl cluster-info
kubectl get nodes
```

---

## Phase 4: Application Deployment

### Step 1: Add Helm Repository

```bash
# Add erlmcp Helm repository
helm repo add erlmcp https://charts.erlmcp.dev/
helm repo update

# Verify repository
helm search repo erlmcp
```

### Step 2: Install Application

```bash
# Create namespace
kubectl create namespace erlmcp-test

# Install with Helm
helm install erlmcp-test erlmcp/erlmcp-enterprise \
  --namespace erlmcp-test \
  --set image.tag=v3.0.0 \
  --set replicas=3 \
  --set resources.requests.cpu=500m \
  --set resources.requests.memory=512Mi \
  --set resources.limits.cpu=1000m \
  --set resources.limits.memory=1Gi \
  --timeout 10m

# Verify installation
helm status erlmcp-test --namespace erlmcp-test
kubectl get all --namespace erlmcp-test
```

### Step 3: Verify Application Health

```bash
# Check pod status
kubectl get pods --namespace erlmcp-test

# Check services
kubectl get services --namespace erlmcp-test

# Check logs
kubectl logs -l app=erlmcp --namespace erlmcp-test --tail=100

# Port-forward to test locally
kubectl port-forward svc/erlmcp-test 8080:80 --namespace erlmcp-test

# Test endpoint
curl http://localhost:8080/health
curl http://localhost:8080/api/v1/status
```

---

## Phase 5: Validation Testing

### Step 1: Functional Tests

```bash
# Test health endpoint
kubectl run curl-test --image=curlimages/curl:latest \
  --rm -it --restart=Never \
  --namespace erlmcp-test \
  -- curl http://erlmcp-test.erlmcp-test.svc.cluster.local/health

# Test API endpoint
kubectl run curl-test --image=curlimages/curl:latest \
  --rm -it --restart=Never \
  --namespace erlmcp-test \
  -- curl http://erlmcp-test.erlmcp-test.svc.cluster.local/api/v1/info

# Verify response
kubectl run curl-test --image=curlimages/curl:latest \
  --rm -it --restart=Never \
  --namespace erlmcp-test \
  -- curl -w "\nHTTP Status: %{http_code}\n" \
    http://erlmcp-test.erlmcp-test.svc.cluster.local/api/v1/status
```

### Step 2: Performance Tests

```bash
# Install load testing tool
kubectl apply -f https://raw.githubusercontent.com/helsonxiao/kubectl-plugins/master/plugins/kubectl-wrk
chmod +x kubectl-wrk
sudo mv kubectl-wrk /usr/local/bin/

# Run load test
kubectl-wrk -t 4 -c 10 -d 30s \
  http://erlmcp-test.erlmcp-test.svc.cluster.local/api/v1/status \
  --namespace erlmcp-test
```

### Step 3: Security Tests

```bash
# Check for vulnerabilities
kubectl run trivy --image=aquasec/trivy:latest \
  --rm -it --restart=Never \
  --command -- \
  trivy image us-central1-docker.pkg.dev/reviewer-test-1770096612/erlmcp-repo/erlmcp:v3.0.0

# Check network policies
kubectl get networkpolicies --namespace erlmcp-test

# Check pod security policies
kubectl get psp --namespace erlmcp-test

# Verify secrets are not exposed
kubectl get secrets --namespace erlmcp-test
kubectl describe pod -l app=erlmcp --namespace erlmcp-test | grep -i secret
```

### Step 4: Compliance Tests

```bash
# Verify resource limits
kubectl describe pods -l app=erlmcp --namespace erlmcp-test | grep -A 5 "Limits"

# Verify liveness/readiness probes
kubectl describe pods -l app=erlmcp --namespace erlmcp-test | grep -A 10 "Liveness"

# Verify pod distribution
kubectl get pods -n erlmcp-test -o wide
kubectl top pods --namespace erlmcp-test

# Check monitoring
kubectl top nodes
```

---

## Phase 6: Cleanup

### Step 1: Uninstall Application

```bash
# Uninstall Helm release
helm uninstall erlmcp-test --namespace erlmcp-test

# Verify cleanup
kubectl get all --namespace erlmcp-test
kubectl delete namespace erlmcp-test
```

### Step 2: Delete GKE Cluster

```bash
# Delete cluster
gcloud container clusters delete erlmcp-test-cluster \
  --zone=us-central1-a \
  --quiet \
  --project=reviewer-test-1770096612
```

### Step 3: Delete Artifact Registry

```bash
# Delete repository
gcloud artifacts repositories delete erlmcp-repo \
  --location=us-central1 \
  --quiet \
  --project=reviewer-test-1770096612
```

### Step 4: Disable APIs

```bash
# Disable all enabled APIs
gcloud services disable \
  $(gcloud services list --enabled --project=reviewer-test-1770096612 --format="value(name)") \
  --project=reviewer-test-1770096612 \
  --async
```

### Step 5: Unlink Billing

```bash
# Unlink billing account
gcloud beta billing projects unlink reviewer-test-1770096612
```

### Step 6: Delete Project

```bash
# Delete project
gcloud projects delete reviewer-test-1770096612 --quiet

# Verify deletion
gcloud projects list | grep reviewer-test-1770096612
```

---

## Evidence Collection

### Screenshots Required

1. **Project Creation**: `gcloud projects describe reviewer-test-1770096612`
2. **APIs Enabled**: `gcloud services list --enabled`
3. **Cluster Running**: `kubectl get nodes`
4. **Pods Running**: `kubectl get pods -n erlmcp-test`
5. **Service Response**: `curl http://erlmcp-test.erlmcp-test.svc.cluster.local/health`
6. **Resource Usage**: `kubectl top pods -n erlmcp-test`

### Logs to Collect

```bash
# Application logs
kubectl logs -l app=erlmcp --namespace erlmcp-test > erlmcp-logs.txt

# Event logs
kubectl get events --namespace erlmcp-test > erlmcp-events.txt

# Helm install logs
helm status erlmcp-test --namespace erlmcp-test > helm-status.txt

# GKE operation logs
gcloud container operations list \
  --cluster=erlmcp-test-cluster \
  --zone=us-central1-a \
  --project=reviewer-test-1770096612 > gke-operations.txt
```

### Metrics to Verify

- Deployment time: < 10 minutes
- Pod startup time: < 2 minutes
- Health check response: < 100ms
- Memory usage: < 1Gi per pod
- CPU usage: < 500m per pod
- Throughput: > 100 req/s
- Error rate: < 0.1%

---

## Success Criteria

### Must Pass
- [x] All 42 APIs enabled successfully
- [ ] GKE cluster created and healthy
- [ ] Application deployed via Helm
- [ ] All pods in Running state
- [ ] Health endpoint returns 200 OK
- [ ] API endpoints functional
- [ ] Resource limits enforced
- [ ] Liveness/readiness probes working

### Should Pass
- [ ] Deployment time < 10 minutes
- [ ] Response time < 100ms
- [ ] Zero security vulnerabilities
- [ ] Monitoring data available
- [ ] Logs properly formatted
- [ ] Secrets properly managed

### Nice to Have
- [ ] Auto-scaling functional
- [ ] Load balancing working
- [ ] Network policies enforced
- [ ] Resource quotas configured

---

## Timeline Estimate

| Phase | Duration |
|-------|----------|
| Bootstrap | 5 minutes |
| Prerequisites | 2 minutes |
| API Enablement | 10 minutes |
| Environment Prep | 15 minutes |
| Application Deploy | 10 minutes |
| Validation | 30 minutes |
| Cleanup | 10 minutes |
| **Total** | **~1.5 hours** |

---

**End of Reviewer Command Flow**

All commands documented for Google Marketplace compliance validation.
