#!/bin/sh
# Docker entrypoint script for erlmcp v3

# Set default values for environment variables if not set
export ERLMCP_NODE_NAME=${ERLMCP_NODE_NAME:-"erlmcp-$(hostname)"}
export ERLMCP_COOKIE=${ERLMCP_COOKIE:-"$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)"}
export ERLMCP_PROFILE=${ERLMCP_PROFILE:-"prod"}

# Wait for required services
wait_for_service() {
    local service=$1
    local port=$2
    local timeout=${3:-30}

    echo "Waiting for $service on port $port..."
    for i in $(seq 1 $timeout); do
        if nc -z $service $port >/dev/null 2>&1; then
            echo "$service is ready"
            return 0
        fi
        sleep 1
        echo "Waiting for $service... $i/$timeout"
    done
    echo "Timeout waiting for $service"
    return 1
}

# Validate environment
validate_environment() {
    required_vars="ERLMCP_COOKIE"

    for var in $required_vars; do
        if [ -z "${!var}" ]; then
            echo "ERROR: $var is required but not set"
            exit 1
        fi
    done

    # Generate JWT secret if not provided
    if [ -z "$ERLMCP_JWT_SECRET" ]; then
        export ERLMCP_JWT_SECRET="$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1)"
        echo "Generated JWT secret: $ERLMCP_JWT_SECRET"
    fi
}

# Prepare configuration
prepare_config() {
    # Create config directory if it doesn't exist
    mkdir -p /etc/erlmcp

    # Substitute environment variables in configuration files
    envsubst < /etc/erlmcp/sys.config > /tmp/sys.config
    mv /tmp/sys.config /etc/erlmcp/sys.config

    envsubst < /etc/erlmcp/vm.args > /tmp/vm.args
    mv /tmp/vm.args /etc/erlmcp/vm.args

    # Set proper permissions
    chown erlmcp:erlmcp /etc/erlmcp/sys.config /etc/erlmcp/vm.args
    chmod 600 /etc/erlmcp/sys.config /etc/erlmcp/vm.args

    echo "Configuration prepared"
}

# Health check setup
setup_health_checks() {
    # Create health check script
    cat > /opt/erlmcp/bin/health_check.sh << 'EOF'
#!/bin/sh
# Health check script

# Check if the node is running
if ! nc -z localhost 8080 >/dev/null 2>&1; then
    echo "Node is not responding on port 8080"
    exit 1
fi

# Check if the node is alive
if ! /opt/erlmcp/bin/erlmcp ping >/dev/null 2>&1; then
    echo "Node is not alive"
    exit 1
fi

# Check memory usage
MEM_INFO=$(ps -p $(pgrep -f beam.smp) -o rss=)
if [ "$MEM_INFO" -gt 524288 ]; then  # 512MB
    echo "Memory usage too high: $MEM_INFO KB"
    exit 1
fi

# Check process count
PROCESS_COUNT=$(pgrep -f erlmcp | wc -l)
if [ "$PROCESS_COUNT" -eq 0 ]; then
    echo "No erlmcp processes found"
    exit 1
fi

echo "Health check passed"
exit 0
EOF

    chmod +x /opt/erlmcp/bin/health_check.sh
}

# Initialize cluster if needed
initialize_cluster() {
    if [ "$ERLMCP_CLUSTER_MODE" = "init" ]; then
        echo "Initializing cluster..."
        /opt/erlmcp/bin/erlmcp cluster init
    elif [ "$ERLMCP_CLUSTER_MODE" = "join" ] && [ -n "$ERLMCP_SEED_NODES" ]; then
        echo "Joining cluster with seed nodes: $ERLMCP_SEED_NODES"
        /opt/erlmcp/bin/erlmcp cluster join $ERLMCP_SEED_NODES
    fi
}

# Start monitoring
start_monitoring() {
    # Start metrics collection
    if [ "$ERLMCP_METRICS_ENABLED" = "true" ]; then
        echo "Starting metrics collection..."
        /opt/erlmcp/bin/erlmcp metrics start &
    fi

    # Start logging to file
    if [ -n "$ERLMCP_LOG_FILE" ]; then
        tail -f /var/log/erlmcp/*.log > $ERLMCP_LOG_FILE &
    fi
}

# Signal handling
handle_signals() {
    # Clean shutdown on SIGTERM
    trap 'echo "Received SIGTERM, shutting down..."; /opt/erlmcp/bin/erlmcp stop; exit 0' TERM
    # Ignore SIGPIPE to prevent crashes
    trap '' PIPE
}

# Main execution
main() {
    echo "Starting erlmcp v3..."

    # Validate environment
    validate_environment

    # Prepare configuration
    prepare_config

    # Setup health checks
    setup_health_checks

    # Initialize cluster if needed
    initialize_cluster

    # Setup signal handling
    handle_signals

    # Start monitoring
    start_monitoring

    # Execute the command
    echo "Starting erlmcp with node name: $ERLMCP_NODE_NAME"
    exec "$@"
}

# Check if this is the main process
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi