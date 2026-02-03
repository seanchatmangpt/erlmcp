#!/bin/bash

# erlmcp Transport Startup Script for Docker Swarm
# Supports stdio, tcp, http, and websocket transports

set -euo pipefail

# Configuration
TRANSPORT_TYPE=${1:-stdio}
PORT=${ERLMCP_TRANSPORT_PORT:-8080}
MAX_CONNECTIONS=${ERLMCP_MAX_CONNECTIONS:-1000}
TLS_ENABLED=${ERLMCP_TRANSPORT_TLS:-false}
NODE_NAME=${HOSTNAME}
HEALTH_CHECK_INTERVAL=${HEALTH_CHECK_INTERVAL:-30}

# Wait for core service
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

wait_for_service erlmcp-core 8080

# Setup TLS if enabled
if [ "$TLS_ENABLED" = "true" ]; then
    TLS_CERT=${ERLMCP_TLS_CERT_FILE:-/app/tls/cert.pem}
    TLS_KEY=${ERLMCP_TLS_KEY_FILE:-/app/tls/key.pem}

    if [ ! -f "$TLS_CERT" ] || [ ! -f "$TLS_KEY" ]; then
        echo "Error: TLS certificates not found"
        exit 1
    fi
fi

# Set transport-specific configurations
case "$TRANSPORT_TYPE" in
    stdio)
        echo "Starting stdio transport on port $PORT"
        export ERLMCP_TRANSPORT_TYPE=stdio
        export ERLMCP_TRANSPORT_PORT=$PORT
        ;;
    tcp)
        echo "Starting TCP transport on port $PORT with TLS=$TLS_ENABLED"
        export ERLMCP_TRANSPORT_TYPE=tcp
        export ERLMCP_TRANSPORT_PORT=$PORT
        export ERLMCP_TRANSPORT_TLS=$TLS_ENABLED
        export ERLMCP_TRANSPORT_CERT_FILE=$TLS_CERT
        export ERLMCP_TRANSPORT_KEY_FILE=$TLS_KEY
        ;;
    http)
        echo "Starting HTTP transport on port $PORT with TLS=$TLS_ENABLED"
        export ERLMCP_TRANSPORT_TYPE=http
        export ERLMCP_TRANSPORT_PORT=$PORT
        export ERLMCP_TRANSPORT_TLS=$TLS_ENABLED
        export ERLMCP_TRANSPORT_CERT_FILE=$TLS_CERT
        export ERLMCP_TRANSPORT_KEY_FILE=$TLS_KEY
        ;;
    ws)
        echo "Starting WebSocket transport on port $PORT"
        export ERLMCP_TRANSPORT_TYPE=ws
        export ERLMCP_TRANSPORT_PORT=$PORT
        ;;
    *)
        echo "Error: Unknown transport type: $TRANSPORT_TYPE"
        exit 1
        ;;
esac

# Set common configurations
export ERLMCP_PROFILE=swarm
export ERLMCP_NODE_NAME=erlmcp-$TRANSPORT_TYPE@$NODE_NAME
export ERLMCP_MAX_CONNECTIONS=$MAX_CONNECTIONS
export ERLMCP_ENABLE_OTEL=true

# Enable OpenTelemetry
export OTEL_EXPORTER_OTLP_ENDPOINT=http://erlmcp-otel-collector:4317
export OTEL_SERVICE_NAME=erlmcp-$TRANSPORT_TYPE
export OTEL_RESOURCE_ATTRIBUTES=cluster=erlmcp-swarm,environment=production,transport=$TRANSPORT_TYPE

# Start the transport
echo "Starting erlmcp $TRANSPORT_TYPE transport"
exec /app/bin/erlmcp_transports console