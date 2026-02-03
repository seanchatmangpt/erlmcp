#!/bin/sh
# Health probes for Prometheus

# Liveness probe
if [ "$1" = "liveness" ]; then
    # Check if prometheus is running
    pgrep prometheus > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Prometheus process not found"
        exit 1
    fi

    # Check if metrics are available
    curl -f http://localhost:9090/api/v1/status/healthy > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Prometheus not healthy"
        exit 1
    fi

    echo "Prometheus is alive"
    exit 0
fi

# Readiness probe
if [ "$1" = "readiness" ]; then
    # Check if prometheus is running
    pgrep prometheus > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Prometheus process not found"
        exit 1
    fi

    # Check if metrics are available
    curl -f http://localhost:9090/api/v1/query?query=up > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Cannot query Prometheus metrics"
        exit 1
    fi

    # Check if storage is writable
    if [ ! -d "/data" ] || [ ! -w "/data" ]; then
        echo "Storage not writable"
        exit 1
    fi

    echo "Prometheus is ready"
    exit 0
fi

# Default health check
curl -f http://localhost:9090/api/v1/status/healthy > /dev/null 2>&1
exit $?