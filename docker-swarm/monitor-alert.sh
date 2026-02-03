#!/bin/bash

# erlmcp Monitoring and Alert Script
# Enterprise Monitoring with Custom Alerts

set -euo pipefail

# Configuration
GRAFANA_URL=${GRAFANA_URL:-"http://localhost:3000"}
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9090"}
ALERT_WEBHOOK=${ALERT_WEBHOOK:-""}
SLACK_WEBHOOK=${SLACK_WEBHOOK:-""}

# Function to send alert to Slack
send_slack_alert() {
    local service=$1
    local message=$2
    local severity=$3

    if [ -z "$SLACK_WEBHOOK" ]; then
        echo "Warning: Slack webhook not configured"
        return 0
    fi

    local color="danger"
    if [ "$severity" = "warning" ]; then
        color="warning"
    fi

    curl -X POST -H 'Content-type: application/json' \
        --data "{\"text\":\"🚨 erlmcp Alert: $service\",\"attachments\":[{\"color\":\"$color\",\"text\":\"$message\"}]}" \
        "$SLACK_WEBHOOK" >/dev/null 2>&1
}

# Function to check service health
check_service_health() {
    local service=$1
    local port=$2
    local timeout=${3:-10}

    if ! nc -z "$service" "$port" >/dev/null 2>&1; then
        echo "ERROR: Service $service on port $port is not responding"
        return 1
    fi

    return 0
}

# Function to check resource usage
check_resource_usage() {
    local service=$1
    local cpu_threshold=$2
    local mem_threshold=$3

    # Get CPU usage
    local cpu_usage=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=100 - (avg by (service) (rate(container_cpu_usage_seconds_total[5m])) * 100)" | \
        jq '.data.result[0].value[1] // 0' | cut -d'.' -f1)

    # Get memory usage
    local mem_usage=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=(container_memory_usage_bytes / container_spec_memory_limit_bytes) * 100" | \
        jq '.data.result[0].value[1] // 0' | cut -d'.' -f1)

    if [ "${cpu_usage:-0}" -gt "$cpu_threshold" ]; then
        echo "WARNING: High CPU usage: ${cpu_usage}% on service $service"
        send_slack_alert "$service" "High CPU usage: ${cpu_usage}%" "warning"
    fi

    if [ "${mem_usage:-0}" -gt "$mem_threshold" ]; then
        echo "WARNING: High memory usage: ${mem_usage}% on service $service"
        send_slack_alert "$service" "High memory usage: ${mem_usage}%" "warning"
    fi
}

# Function to check metrics
check_metrics() {
    local service=$1

    # Check error rate
    local error_rate=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=(rate(http_requests_total{service=\"$service\",status=~\"5..\"}[5m]) / rate(http_requests_total{service=\"$service\"}[5m])) * 100" | \
        jq '.data.result[0].value[1] // 0')

    if [ "${error_rate:-0}" -gt 5 ]; then
        echo "ERROR: High error rate: ${error_rate}% on service $service"
        send_slack_alert "$service" "High error rate: ${error_rate}%" "danger"
    fi

    # Check response time
    local response_time=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=histogram_quantile(0.95, rate(http_request_duration_seconds_bucket{service=\"$service\"}[5m]))" | \
        jq '.data.result[0].value[1] // 0')

    if [ "${response_time:-0}" -gt 2 ]; then
        echo "WARNING: Slow response time: ${response_time}s on service $service"
        send_slack_alert "$service" "Slow response time: ${response_time}s" "warning"
    fi
}

# Function to check storage
check_storage() {
    local service=$1

    # Check disk usage
    local disk_usage=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=(1 - (disk_free_bytes / disk_total_bytes)) * 100" | \
        jq '.data.result[0].value[1] // 0')

    if [ "${disk_usage:-0}" -gt 85 ]; then
        echo "ERROR: High disk usage: ${disk_usage}% on node for service $service"
        send_slack_alert "$service" "High disk usage: ${disk_usage}%" "danger"
    fi
}

# Function to check database connections
check_database() {
    # Check MySQL connections
    local mysql_connections=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=mysql_global_status_threads_connected" | \
        jq '.data.result[0].value[1] // 0')

    local mysql_max_connections=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=mysql_global_variables_max_connections" | \
        jq '.data.result[0].value[1] // 0')

    if [ "$mysql_max_connections" -gt 0 ]; then
        local usage=$(echo "scale=2; $mysql_connections / $mysql_max_connections * 100" | bc -l)
        if [ "$(echo "$usage > 80" | bc -l)" -eq 1 ]; then
            echo "WARNING: High MySQL connection usage: ${usage}%"
            send_slack_alert "Database" "MySQL connections: ${usage}%" "warning"
        fi
    fi

    # Check Redis connections
    local redis_connections=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=redis_connected_clients" | \
        jq '.data.result[0].value[1] // 0')

    local redis_max_clients=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=redis_config_maxclients" | \
        jq '.data.result[0].value[1] // 0')

    if [ "$redis_max_clients" -gt 0 ]; then
        local usage=$(echo "scale=2; $redis_connections / $redis_max_clients * 100" | bc -l)
        if [ "$(echo "$usage > 80" | bc -l)" -eq 1 ]; then
            echo "WARNING: High Redis connection usage: ${usage}%"
            send_slack_alert "Database" "Redis connections: ${usage}%" "warning"
        fi
    fi
}

# Function to check erlmcp-specific metrics
check_erlmcp_metrics() {
    # Check active sessions
    local active_sessions=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=erlmcp_sessions_active" | \
        jq '.data.result[0].value[1] // 0')

    local max_sessions=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=erlmcp_sessions_max" | \
        jq '.data.result[0].value[1] // 10000')

    if [ "$max_sessions" -gt 0 ]; then
        local usage=$(echo "scale=2; $active_sessions / $max_sessions * 100" | bc -l)
        if [ "$(echo "$usage > 80" | bc -l)" -eq 1 ]; then
            echo "WARNING: High session usage: ${usage}%"
            send_slack_alert "erlmcp-core" "Active sessions: ${usage}%" "warning"
        fi
    fi

    # Check active connections
    local active_connections=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=erlmcp_connections_active" | \
        jq '.data.result[0].value[1] // 0')

    local max_connections=$(curl -s "$PROMETHEUS_URL/api/v1/query" \
        --data-urlencode "query=erlmcp_connections_max" | \
        jq '.data.result[0].value[1] // 5000')

    if [ "$max_connections" -gt 0 ]; then
        local usage=$(echo "scale=2; $active_connections / $max_connections * 100" | bc -l)
        if [ "$(echo "$usage > 90" | bc -l)" -eq 1 ]; then
            echo "WARNING: High connection usage: ${usage}%"
            send_slack_alert "erlmcp-transport" "Active connections: ${usage}%" "warning"
        fi
    fi
}

# Main monitoring function
monitor_services() {
    echo "Starting erlmcp service monitoring..."

    services=(
        "erlmcp-core:8080"
        "erlmcp-transport-stdio:9101"
        "erlmcp-transport-tcp:9102"
        "erlmcp-transport-http:9103"
        "prometheus:9090"
        "grafana:3000"
        "elasticsearch:9200"
        "kibana:5601"
    )

    while true; do
        echo "=== Health Check: $(date) ==="

        for service in "${services[@]}"; do
            local service_name=$(echo "$service" | cut -d':' -f1)
            local port=$(echo "$service" | cut -d':' -f2)

            if ! check_service_health "$service_name" "$port"; then
                send_slack_alert "$service_name" "Service not responding" "danger"
            else
                echo "✓ $service_name is healthy"

                # Check resource usage
                check_resource_usage "$service_name" 80 85

                # Check erlmcp-specific metrics
                if [[ "$service_name" == "erlmcp-"* ]]; then
                    check_metrics "$service_name"
                    check_erlmcp_metrics
                fi
            fi
        done

        # Check database connections
        check_database

        # Check storage
        check_storage "all"

        echo "Sleeping for 60 seconds..."
        sleep 60
    done
}

# Function to show dashboard links
show_dashboards() {
    echo "erlmcp Monitoring Dashboards:"
    echo "============================"
    echo "Grafana: $GRAFANA_URL"
    echo "Prometheus: $PROMETHEUS_URL"
    echo "Kibana: http://localhost:5601"
    echo ""
    echo "Default Credentials:"
    echo "Grafana: admin/admin (change this!)"
    echo "Kibana: elastic/elastic (change this!)"
    echo ""
    echo "Prometheus Queries:"
    echo "  - Service health: up{job=\"erlmcp\"}"
    echo "  - CPU usage: 100 - (avg by (service) (rate(container_cpu_usage_seconds_total[5m])) * 100)"
    echo "  - Memory usage: (container_memory_usage_bytes / container_spec_memory_limit_bytes) * 100"
    echo "  - Error rate: (rate(http_requests_total{status=~\"5..\"}[5m]) / rate(http_requests_total[5m])) * 100"
    echo "  - Response time: histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))"
}

# Main execution
case "${1:-}" in
    monitor)
        monitor_services
        ;;
    dashboards)
        show_dashboards
        ;;
    check)
        check_service_health "$2" "$3"
        ;;
    *)
        echo "Usage: $0 {monitor|dashboards|check}"
        echo ""
        echo "Commands:"
        echo "  monitor    - Start continuous monitoring"
        echo "  dashboards - Show dashboard links"
        echo "  check      - Check specific service health"
        exit 1
        ;;
esac