#!/bin/bash
# verify_deployment.sh - Verify erlmcp deployment health and status
#
# Usage:
#   ./verify_deployment.sh [project-id] [profile]
#
# Example:
#   ./verify_deployment.sh taiea-v1 prod

set -e

# Configuration
PROJECT_ID="${1:-taiea-v1}"
PROFILE="${2:-prod}"
CLUSTER_NAME="erlmcp-${PROFILE}"
ZONE="us-central1-a"

echo "=== erlmcp Deployment Verification ==="
echo "Project ID: ${PROJECT_ID}"
echo "Profile: ${PROFILE}"
echo "Cluster: ${CLUSTER_NAME}"
echo ""

# Helper function for status output
print_status() {
    if [ $1 -eq 0 ]; then
        echo "✓ $2"
    else
        echo "✗ $2"
    fi
}

# Step 1: Verify cluster connection
echo "[1/8] Verifying cluster connection..."
if gcloud container clusters describe ${CLUSTER_NAME} --zone=${ZONE} --project=${PROJECT_ID} &>/dev/null; then
    gcloud container clusters get-credentials ${CLUSTER_NAME} \
        --zone=${ZONE} \
        --project=${PROJECT_ID} > /dev/null 2>&1
    print_status 0 "Cluster accessible"
else
    print_status 1 "Cluster not found"
    exit 1
fi
echo ""

# Step 2: Check namespace
echo "[2/8] Checking erlmcp namespace..."
if kubectl get namespace erlmcp &>/dev/null; then
    print_status 0 "Namespace exists"
else
    print_status 1 "Namespace not found"
    exit 1
fi
echo ""

# Step 3: Check deployment status
echo "[3/8] Checking deployment status..."
DESIRED=$(kubectl get deployment erlmcp -n erlmcp -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "0")
CURRENT=$(kubectl get deployment erlmcp -n erlmcp -o jsonpath='{.status.replicas}' 2>/dev/null || echo "0")
READY=$(kubectl get deployment erlmcp -n erlmcp -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")

echo "Desired replicas: ${DESIRED}"
echo "Current replicas: ${CURRENT}"
echo "Ready replicas: ${READY}"

if [ "${READY}" == "${DESIRED}" ]; then
    print_status 0 "All pods ready"
else
    print_status 1 "Not all pods ready"
fi
echo ""

# Step 4: Check pod status
echo "[4/8] Checking pod status..."
kubectl get pods -n erlmcp -o wide --no-headers | while read line; do
    POD_NAME=$(echo "$line" | awk '{print $1}')
    STATUS=$(echo "$line" | awk '{print $3}')
    if [ "${STATUS}" == "Running" ]; then
        echo "✓ ${POD_NAME} - Running"
    else
        echo "✗ ${POD_NAME} - ${STATUS}"
    fi
done
echo ""

# Step 5: Check service
echo "[5/8] Checking service..."
SERVICE_TYPE=$(kubectl get service erlmcp -n erlmcp -o jsonpath='{.spec.type}' 2>/dev/null)
ENDPOINT=$(kubectl get service erlmcp -n erlmcp -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null)

echo "Service type: ${SERVICE_TYPE}"
if [ -z "${ENDPOINT}" ]; then
    echo "⚠ LoadBalancer endpoint pending (may take a few minutes)"
else
    echo "Service endpoint: http://${ENDPOINT}:8080"
    print_status 0 "Service has endpoint"
fi
echo ""

# Step 6: Health check
echo "[6/8] Checking health endpoint..."
if [ ! -z "${ENDPOINT}" ]; then
    HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" "http://${ENDPOINT}:8080/health" 2>/dev/null || echo "000")
    if [ "${HTTP_CODE}" == "200" ]; then
        print_status 0 "Health endpoint responding"
    else
        echo "Health endpoint returned: ${HTTP_CODE}"
    fi
else
    echo "⚠ Skipping health check (endpoint not ready)"
fi
echo ""

# Step 7: Check metrics
echo "[7/8] Checking metrics endpoint..."
if [ ! -z "${ENDPOINT}" ]; then
    METRICS_CODE=$(curl -s -o /dev/null -w "%{http_code}" "http://${ENDPOINT}:9090/metrics" 2>/dev/null || echo "000")
    if [ "${METRICS_CODE}" == "200" ]; then
        print_status 0 "Metrics endpoint responding"
        echo "Metrics sample:"
        curl -s "http://${ENDPOINT}:9090/metrics" 2>/dev/null | grep "erlmcp_" | head -5
    else
        echo "Metrics endpoint returned: ${METRICS_CODE}"
    fi
else
    echo "⚠ Skipping metrics check (endpoint not ready)"
fi
echo ""

# Step 8: Check logs
echo "[8/8] Checking pod logs..."
POD=$(kubectl get pod -n erlmcp -l app=erlmcp -o jsonpath='{.items[0].metadata.name}' 2>/dev/null)
if [ ! -z "${POD}" ]; then
    echo "Recent logs from ${POD}:"
    kubectl logs -n erlmcp ${POD} --tail=5 || echo "No logs available"
else
    echo "⚠ No pods found"
fi
echo ""

# Summary
echo "=== Verification Complete ==="
echo "Deployment: ${PROFILE}"
echo "Ready pods: ${READY}/${DESIRED}"
echo ""

if [ "${READY}" == "${DESIRED}" ] && [ ! -z "${ENDPOINT}" ]; then
    echo "✓ Deployment is HEALTHY"
    echo ""
    echo "Next steps:"
    echo "  1. Test service: curl http://${ENDPOINT}:8080"
    echo "  2. View logs: kubectl logs -n erlmcp -l app=erlmcp"
    echo "  3. Monitor metrics: kubectl port-forward -n erlmcp svc/erlmcp 9090:9090"
else
    echo "⚠ Deployment needs attention"
    echo ""
    echo "Debugging steps:"
    echo "  1. Check pod details: kubectl describe pods -n erlmcp"
    echo "  2. View logs: kubectl logs -n erlmcp -l app=erlmcp"
    echo "  3. Check events: kubectl get events -n erlmcp --sort-by='.lastTimestamp'"
fi
