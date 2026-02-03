#!/bin/bash

# Blue-green deployment switch script
set -e

ENVIRONMENT=$1
NAMESPACE="erlmcp-$ENVIRONMENT"

if [ -z "$ENVIRONMENT" ]; then
    echo "Usage: $0 <environment> (staging|production)"
    exit 1
fi

# Validate environment
if [ "$ENVIRONMENT" != "staging" ] && [ "$ENVIRONMENT" != "production" ]; then
    echo "Error: Environment must be either 'staging' or 'production'"
    exit 1
fi

echo "Starting blue-green switch for $ENVIRONMENT..."

# Check current state
CURRENT_BLUE=$(kubectl get deployment erlmcp-blue -n $NAMESPACE -o jsonpath='{.status.conditions[?(@.type=="Available")].status}' 2>/dev/null || echo "not-found")
CURRENT_GREEN=$(kubectl get deployment erlmcp-green -n $NAMESPACE -o jsonpath='{.status.conditions[?(@.type=="Available")].status}' 2>/dev/null || echo "not-found")

if [ "$CURRENT_BLUE" != "True" ] && [ "$CURRENT_GREEN" != "True" ]; then
    echo "Error: Neither blue nor green deployment is available"
    exit 1
fi

# Determine active deployment
if [ "$CURRENT_BLUE" = "True" ]; then
    ACTIVE_BLUE=true
    ACTIVE_GREEN=false
else
    ACTIVE_BLUE=false
    ACTIVE_GREEN=true
fi

echo "Current state:"
echo "  Blue deployment: $ACTIVE_BLUE"
echo "  Green deployment: $ACTIVE_GREEN"

if [ "$ENVIRONMENT" = "production" ]; then
    # For production, use Ingress to switch traffic
    if [ "$ACTIVE_BLUE" = true ]; then
        echo "Switching traffic from blue to green..."
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/0/http/paths/0/backend/service/name", "value": "erlmcp-green"}]'
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/1/http/paths/0/backend/service/name", "value": "erlmcp-green"}]'
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/2/http/paths/0/backend/service/name", "value": "erlmcp-green"}]'
    else
        echo "Switching traffic from green to blue..."
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/0/http/paths/0/backend/service/name", "value": "erlmcp-blue"}]'
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/1/http/paths/0/backend/service/name", "value": "erlmcp-blue"}]'
        kubectl patch ingress erlmcp-green-ingress -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/spec/rules/2/http/paths/0/backend/service/name", "value": "erlmcp-blue"}]'
    fi
else
    # For staging, use Service
    if [ "$ACTIVE_BLUE" = true ]; then
        echo "Switching traffic from blue to green in staging..."
        kubectl patch service erlmcp -n $NAMESPACE --type=json -p='[{"op": "add", "path": "/spec/selector/component", "value": "erlmcp-green"}]'
    else
        echo "Switching traffic from green to blue in staging..."
        kubectl patch service erlmcp -n $NAMESPACE --type=json -p='[{"op": "add", "path": "/spec/selector/component", "value": "erlmcp-blue"}]'
    fi
fi

# Wait for traffic switch
echo "Waiting for traffic to switch..."
sleep 10

# Verify switch
if [ "$ENVIRONMENT" = "production" ]; then
    # Check load balancer status
    LB_HOSTNAME=$(kubectl get ingress erlmcp-green-ingress -n $NAMESPACE -o jsonpath='{.rules[0].host}')
    echo "Verifying traffic to $LB_HOSTNAME..."

    # Multiple health checks
    for i in {1..5}; do
        if curl -f -s https://$LB_HOSTNAME/health >/dev/null 2>&1; then
            echo "✓ Health check passed"
        else
            echo "✗ Health check failed"
            exit 1
        fi
        sleep 2
    done
else
    # Check service endpoint
    SERVICE_ENDPOINT=$(kubectl get service erlmcp -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')
    echo "Verifying traffic to $SERVICE_ENDPOINT..."

    if curl -f -s http://$SERVICE_ENDPOINT/health >/dev/null 2>&1; then
        echo "✓ Health check passed"
    else
        echo "✗ Health check failed"
        exit 1
    fi
fi

echo "Blue-green switch completed successfully"

# Generate deployment report
cat > artifacts/deployment-report-$ENVIRONMENT.json <<EOF
{
    "environment": "$ENVIRONMENT",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "switch_completed": true,
    "active_deployment": "$ACTIVE_BLUE ? blue : green",
    "health_checks": "passed",
    "traffic_switched": true
}
EOF