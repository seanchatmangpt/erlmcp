#!/bin/bash

# Automated rollback script for erlmcp deployment
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

echo "Starting automated rollback for $ENVIRONMENT..."

# Check if rollback is already in progress
ROLLBACK_STATE=$(kubectl get configmap rollback-state -n $NAMESPACE -o jsonpath='{.data.rollback_in_progress}' 2>/dev/null || echo "false")
if [ "$ROLLBACK_STATE" = "true" ]; then
    echo "Error: Rollback already in progress"
    exit 1
fi

# Check health of current deployment
CURRENT_DEPLOYMENT=$(kubectl get deployment -n $NAMESPACE --selector=app=erlmcp -o jsonpath='{.items[0].metadata.name}')
if [ -z "$CURRENT_DEPLOYMENT" ]; then
    echo "Error: No current deployment found"
    exit 1
fi

# Check pod status
POD_COUNT=$(kubectl get pods -n $NAMESPACE --selector=app=erlmcp -o jsonpath='{.items[*].status.phase}' | tr ' ' '\n' | grep -c "Running")
TOTAL_PODS=$(kubectl get deployment $CURRENT_DEPLOYMENT -n $NAMESPACE -o jsonpath='{.spec.replicas}')

echo "Current deployment: $CURRENT_DEPLOYMENT"
echo "Healthy pods: $POD_COUNT/$TOTAL_PODS"

# Check critical endpoints
HEALTH_CHECKS=(
    "http://$(kubectl get service erlmcp -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')/health"
    "http://$(kubectl get service erlmcp -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')/mcp/health"
)

HEALTH_FAILED=0
for endpoint in "${HEALTH_CHECKS[@]}"; do
    if ! curl -f -s "$endpoint" >/dev/null 2>&1; then
        echo "❌ Health check failed: $endpoint"
        HEALTH_FAILED=$((HEALTH_FAILED + 1))
    fi
done

# Check error rate
ERROR_RATE=$(kubectl logs -n $NAMESPACE --selector=app=erlmcp --tail=100 | grep -E "ERROR|CRITICAL" | wc -l)
ERROR_THRESHOLD=10

# Check response time
RESPONSE_TIME=$(curl -o /dev/null -s -w '%{time_total}' http://$(kubectl get service erlmcp -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')/health)
RESPONSE_THRESHOLD=5

# Decision to rollback
ROLLBACK_NEEDED=false
ROLLBACK_REASON=""

if [ $HEALTH_FAILED -gt 0 ]; then
    ROLLBACK_NEEDED=true
    ROLLBACK_REASON="Health checks failed: $HEALTH_FAILED/$(${#HEALTH_CHECKS[@]})"
elif [ $ERROR_RATE -gt $ERROR_THRESHOLD ]; then
    ROLLBACK_NEEDED=true
    ROLLBACK_REASON="High error rate: $ERROR_RATE errors"
elif [ "$(echo "$RESPONSE_TIME > $RESPONSE_THRESHOLD" | bc)" -eq 1 ]; then
    ROLLBACK_NEEDED=true
    ROLLBACK_REASON="Slow response time: ${RESPONSE_TIME}s"
fi

if [ "$ROLLBACK_NEEDED" = "true" ]; then
    echo "⚠️  Rollback triggered: $ROLLBACK_REASON"

    # Set rollback state
    kubectl create configmap rollback-state -n $NAMESPACE --from-literal=rollback_in_progress=true --from-literal=rollback_reason="$ROLLBACK_REASON" --dry-run=client -o yaml | kubectl apply -f -

    # Get previous stable deployment
    PREVIOUS_DEPLOYMENT=$(kubectl get deployments -n $NAMESPACE --sort-by=.metadata.creationTimestamp | grep -v "$CURRENT_DEPLOYMENT" | tail -1 | awk '{print $1}')

    if [ -z "$PREVIOUS_DEPLOYMENT" ]; then
        echo "Error: No previous deployment to rollback to"
        exit 1
    fi

    echo "Rolling back to: $PREVIOUS_DEPLOYMENT"

    # Stop current deployment
    kubectl scale deployment $CURRENT_DEPLOYMENT -n $NAMESPACE --replicas=0

    # Wait for pods to terminate
    kubectl wait --for=delete pod -l app=erlmcp -n $NAMESPACE --timeout=300s

    # Restore previous deployment
    kubectl rollout restart deployment $PREVIOUS_DEPLOYMENT -n $NAMESPACE

    # Wait for deployment to be ready
    kubectl rollout status deployment $PREVIOUS_DEPLOYMENT -n $NAMESPACE --timeout=600s

    # Verify health after rollback
    echo "Verifying health after rollback..."
    sleep 30

    ROLLBACK_VERIFIED=true
    for endpoint in "${HEALTH_CHECKS[@]}"; do
        if ! curl -f -s "$endpoint" >/dev/null 2>&1; then
            echo "❌ Post-rollback health check failed: $endpoint"
            ROLLBACK_VERIFIED=false
        fi
    done

    if [ "$ROLLBACK_VERIFIED" = "true" ]; then
        echo "✅ Rollback completed successfully"

        # Update state
        kubectl patch configmap rollback-state -n $NAMESPACE --type=json -p='[{"op": "replace", "path": "/data/rollback_completed", "value": "true"}]'

        # Alert (Slack notification)
        curl -X POST -H 'Content-type: application/json' --data "{\"text\":\"🔄 Rollback completed successfully for $ENVIRONMENT. Rolled back to $PREVIOUS_DEPLOYMENT. Reason: $ROLLBACK_REASON\"}" $SLACK_WEBHOOK

        # Clean up
        kubectl delete configmap rollback-state -n $NAMESPACE
    else
        echo "❌ Rollback verification failed"
        exit 1
    fi
else
    echo "✅ All checks passed, no rollback needed"
fi

# Generate report
cat > artifacts/rollback-report-$ENVIRONMENT.json <<EOF
{
    "environment": "$ENVIRONMENT",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "current_deployment": "$CURRENT_DEPLOYMENT",
    "health_failed": $HEALTH_FAILED,
    "error_rate": $ERROR_RATE,
    "response_time": $RESPONSE_TIME,
    "rollback_needed": "$ROLLBACK_NEEDED",
    "rollback_reason": "$ROLLBACK_REASON",
    "rollback_completed": "$ROLLBACK_VERIFIED"
}
EOF