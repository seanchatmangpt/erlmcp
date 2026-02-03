#!/bin/bash

# erlmcp Core Startup Script for Docker Swarm
# Enterprise Edition with High Availability Features

set -euo pipefail

# Configuration
CLUSTER_SIZE=${ERLMCP_CLUSTER_SIZE:-3}
REGISTRY_BACKEND=${ERLMCP_REGISTRY_BACKEND:-gproc}
SESSION_BACKEND=${ERLMCP_SESSION_BACKEND:-ets}
ENABLE_OTEL=${ERLMCP_OTEL_ENABLED:-true}
NODE_NAME=${HOSTNAME}
HEALTH_CHECK_INTERVAL=${HEALTH_CHECK_INTERVAL:-30}

# Wait for dependencies
wait_for_service() {
    local service=$1
    local port=$2
    local timeout=${3:-60}

    echo "Waiting for $service on port $port..."
    local i=0
    while ! nc -z $service $port && [ $i -lt $timeout ]; do
        sleep 1
        i=$((i+1))
    done

    if [ $i -eq $timeout ]; then
        echo "Error: $service not available after $timeout seconds"
        exit 1
    fi

    echo "$service is ready"
}

# Wait for registry service
if [ "$REGISTRY_BACKEND" = "gproc" ]; then
    wait_for_service erlmcp-registry 8081
fi

# Wait for session backend
if [ "$SESSION_BACKEND" = "ets" ]; then
    # ETS is local, no need to wait
    :
elif [ "$SESSION_BACKEND" = "mnesia" ]; then
    wait_for_service erlmcp-session-db 3306
elif [ "$SESSION_BACKEND" = "dets" ]; then
    # DETS is local, no need to wait
    :
fi

# Enable OpenTelemetry if requested
if [ "$ENABLE_OTEL" = "true" ]; then
    export OTEL_EXPORTER_OTLP_ENDPOINT=http://erlmcp-otel-collector:4317
    export OTEL_SERVICE_NAME=erlmcp-core
    export OTEL_RESOURCE_ATTRIBUTES=cluster=erlmcp-swarm,environment=production
fi

# Start erlmcp core
echo "Starting erlmcp core with cluster size: $CLUSTER_SIZE"
echo "Registry backend: $REGISTRY_BACKEND"
echo "Session backend: $SESSION_BACKEND"
echo "Node name: $NODE_NAME"

# Export environment variables
export ERLMCP_PROFILE=swarm
export ERLMCP_NODE_NAME=erlmcp-core@$NODE_NAME
export ERLMCP_CLUSTER_SIZE=$CLUSTER_SIZE
export ERLMCP_REGISTRY_BACKEND=$REGISTRY_BACKEND
export ERLMCP_SESSION_BACKEND=$SESSION_BACKEND
export ERLMCP_ENABLE_OTEL=$ENABLE_OTEL

# Start the application
exec /app/bin/erlmcp_core console