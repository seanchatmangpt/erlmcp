#!/bin/bash
# deploy_helm.sh - Deploy erlmcp to GKE using Helm or kubectl
#
# Usage:
#   ./deploy_helm.sh [version] [project-id] [region] [profile]
#
# Profiles: dev, staging, prod, gov
#
# Example:
#   ./deploy_helm.sh 0.7.0 taiea-v1 us-central1 prod

set -e

# Configuration
VERSION="${1:-0.7.0}"
PROJECT_ID="${2:-taiea-v1}"
REGION="${3:-us-central1}"
PROFILE="${4:-prod}"
CLUSTER_NAME="erlmcp-${PROFILE}"
ZONE="${REGION}-a"
ARTIFACT_REGISTRY_REPO="erlmcp-repo"
IMAGE_NAME="erlmcp"

# Derived variables
IMAGE_REGISTRY="${REGION}-docker.pkg.dev"
IMAGE_URI="${IMAGE_REGISTRY}/${PROJECT_ID}/${ARTIFACT_REGISTRY_REPO}/${IMAGE_NAME}:${VERSION}"

echo "=== erlmcp Helm Deployment Script ==="
echo "Project ID: ${PROJECT_ID}"
echo "Version: ${VERSION}"
echo "Profile: ${PROFILE}"
echo "Cluster: ${CLUSTER_NAME}"
echo "Zone: ${ZONE}"
echo "Image URI: ${IMAGE_URI}"
echo ""

# Step 1: Verify cluster exists
echo "[1/6] Checking GKE cluster..."
if ! gcloud container clusters describe ${CLUSTER_NAME} --zone=${ZONE} --project=${PROJECT_ID} &>/dev/null; then
    echo "Creating cluster: ${CLUSTER_NAME}..."

    case ${PROFILE} in
        dev)
            NODE_COUNT=1
            MACHINE_TYPE="n2-standard-2"
            ;;
        staging)
            NODE_COUNT=2
            MACHINE_TYPE="n2-standard-4"
            ;;
        prod|gov)
            NODE_COUNT=3
            MACHINE_TYPE="n2-standard-4"
            ;;
    esac

    gcloud container clusters create ${CLUSTER_NAME} \
        --zone=${ZONE} \
        --num-nodes=${NODE_COUNT} \
        --machine-type=${MACHINE_TYPE} \
        --enable-stackdriver-kubernetes \
        --enable-ip-alias \
        --enable-autoscaling \
        --min-nodes=2 \
        --max-nodes=10 \
        --addons=HttpLoadBalancing,HorizontalPodAutoscaling \
        --project=${PROJECT_ID}
fi
echo "✓ Cluster ready: ${CLUSTER_NAME}"
echo ""

# Step 2: Get cluster credentials
echo "[2/6] Configuring kubectl..."
gcloud container clusters get-credentials ${CLUSTER_NAME} \
    --zone=${ZONE} \
    --project=${PROJECT_ID}

kubectl cluster-info
echo "✓ kubectl configured"
echo ""

# Step 3: Create namespace
echo "[3/6] Setting up namespace..."
kubectl create namespace erlmcp --dry-run=client -o yaml | kubectl apply -f -
kubectl config set-context --current --namespace=erlmcp
echo "✓ Namespace ready"
echo ""

# Step 4: Create image pull secret
echo "[4/6] Creating image pull secret..."
kubectl create secret docker-registry gcr-secret \
    --docker-server=${IMAGE_REGISTRY} \
    --docker-username=_json_key \
    --docker-password="$(gcloud auth print-access-token)" \
    --namespace=erlmcp \
    --dry-run=client -o yaml | kubectl apply -f -

echo "✓ Image pull secret created"
echo ""

# Step 5: Create deployment manifest based on profile
echo "[5/6] Creating deployment manifest..."

case ${PROFILE} in
    dev)
        REPLICAS=1
        CPU_REQUEST="250m"
        CPU_LIMIT="500m"
        MEMORY_REQUEST="256Mi"
        MEMORY_LIMIT="512Mi"
        ;;
    staging)
        REPLICAS=2
        CPU_REQUEST="500m"
        CPU_LIMIT="1000m"
        MEMORY_REQUEST="512Mi"
        MEMORY_LIMIT="1Gi"
        ;;
    prod)
        REPLICAS=3
        CPU_REQUEST="1000m"
        CPU_LIMIT="2000m"
        MEMORY_REQUEST="1Gi"
        MEMORY_LIMIT="2Gi"
        ;;
    gov)
        REPLICAS=3
        CPU_REQUEST="1000m"
        CPU_LIMIT="2000m"
        MEMORY_REQUEST="1Gi"
        MEMORY_LIMIT="2Gi"
        ;;
    *)
        echo "ERROR: Unknown profile: ${PROFILE}"
        exit 1
        ;;
esac

cat > /tmp/erlmcp-deployment-${PROFILE}.yaml <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-config
  namespace: erlmcp
type: Opaque
stringData:
  erlang-cookie: "erlmcp_${PROFILE}_secret_change_me"
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
  namespace: erlmcp
  labels:
    app: erlmcp
    version: "${VERSION}"
    profile: "${PROFILE}"
spec:
  replicas: ${REPLICAS}
  selector:
    matchLabels:
      app: erlmcp
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  template:
    metadata:
      labels:
        app: erlmcp
        version: "${VERSION}"
        profile: "${PROFILE}"
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
        prometheus.io/path: "/metrics"
    spec:
      serviceAccountName: erlmcp
      imagePullSecrets:
        - name: gcr-secret
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 1000
      containers:
      - name: erlmcp
        image: ${IMAGE_URI}
        imagePullPolicy: IfNotPresent
        ports:
        - name: http
          containerPort: 8080
          protocol: TCP
        - name: metrics
          containerPort: 9090
          protocol: TCP
        env:
        - name: ERLMCP_ENV
          value: "${PROFILE}"
        - name: ERLANG_COOKIE
          valueFrom:
            secretKeyRef:
              name: erlmcp-config
              key: erlang-cookie
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        resources:
          requests:
            cpu: ${CPU_REQUEST}
            memory: ${MEMORY_REQUEST}
          limits:
            cpu: ${CPU_LIMIT}
            memory: ${MEMORY_LIMIT}
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
          failureThreshold: 2
        volumeMounts:
        - name: erlmcp-data
          mountPath: /var/lib/erlmcp
      volumes:
      - name: erlmcp-data
        emptyDir: {}
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchExpressions:
                - key: app
                  operator: In
                  values:
                  - erlmcp
              topologyKey: kubernetes.io/hostname
---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
  namespace: erlmcp
  labels:
    app: erlmcp
spec:
  type: LoadBalancer
  selector:
    app: erlmcp
  ports:
  - name: http
    port: 8080
    targetPort: 8080
    protocol: TCP
  - name: metrics
    port: 9090
    targetPort: 9090
    protocol: TCP
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: erlmcp
EOF

echo "✓ Deployment manifest created"
echo ""

# Step 6: Apply deployment
echo "[6/6] Applying deployment..."
kubectl apply -f /tmp/erlmcp-deployment-${PROFILE}.yaml

# Wait for rollout
echo "Waiting for deployment rollout..."
if kubectl rollout status deployment/erlmcp -n erlmcp --timeout=5m; then
    echo "✓ Deployment rolled out successfully"
else
    echo "⚠ Deployment rollout timeout. Check pod status with:"
    echo "  kubectl get pods -n erlmcp -o wide"
    exit 1
fi
echo ""

# Final status
echo "=== Deployment Complete ==="
echo "Profile: ${PROFILE}"
echo "Replicas: ${REPLICAS}"
echo "Image: ${IMAGE_URI}"
echo ""

# Get service endpoint
ENDPOINT=$(kubectl get service erlmcp -n erlmcp \
    -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "pending")

if [ "${ENDPOINT}" != "pending" ]; then
    echo "Service Endpoint: http://${ENDPOINT}:8080"
    echo "Metrics Endpoint: http://${ENDPOINT}:9090/metrics"
else
    echo "Service endpoint pending. Check with:"
    echo "  kubectl get service erlmcp -n erlmcp"
fi
echo ""

echo "Useful commands:"
echo "  kubectl get pods -n erlmcp"
echo "  kubectl logs -n erlmcp -l app=erlmcp"
echo "  kubectl port-forward -n erlmcp svc/erlmcp 8080:8080"
