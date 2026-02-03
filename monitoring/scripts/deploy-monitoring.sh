#!/bin/bash

# Enterprise Monitoring Stack Deployment Script for erlmcp v3
# This script deploys the complete monitoring stack with all components

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Docker is installed
check_docker() {
    if ! command -v docker &> /dev/null; then
        print_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    print_success "Docker is installed"
}

# Check if Docker Compose is installed
check_docker_compose() {
    if ! command -v docker-compose &> /dev/null; then
        print_error "Docker Compose is not installed. Please install Docker Compose first."
        exit 1
    fi
    print_success "Docker Compose is installed"
}

# Create necessary directories
create_directories() {
    print_status "Creating monitoring directories..."
    mkdir -p prometheus/data
    mkdir -p prometheus/rules
    mkdir -p prometheus/recording_rules
    mkdir -p alertmanager/data
    mkdir -p grafana/data
    mkdir -p grafana/dashboards
    mkdir -p grafana/datasources
    mkdir -p loki/data
    mkdir -p otel/data
    mkdir -p logs

    # Copy dashboards to grafana
    cp -r dashboards/* grafana/dashboards/
    cp datasources/prometheus.yml grafana/datasources/

    print_success "Directories created successfully"
}

# Start monitoring stack
start_monitoring_stack() {
    print_status "Starting monitoring stack..."

    # Pull latest images
    docker-compose -f docker/docker-compose.monitoring.yml pull

    # Start services
    docker-compose -f docker/docker-compose.monitoring.yml up -d

    print_success "Monitoring stack started successfully"
}

# Wait for services to be ready
wait_for_services() {
    print_status "Waiting for services to be ready..."

    # Wait for Prometheus
    timeout 60 bash -c 'until curl -f http://localhost:9090/api/v1/status/health > /dev/null 2>&1; do sleep 2; done'
    print_success "Prometheus is ready"

    # Wait for Grafana
    timeout 60 bash -c 'until curl -f http://localhost:3000/api/health > /dev/null 2>&1; do sleep 2; done'
    print_success "Grafana is ready"

    # Wait for Loki
    timeout 60 bash -c 'until curl -f http://localhost:3100/loki/api/v1/status/buildinfo > /dev/null 2>&1; do sleep 2; done'
    print_success "Loki is ready"

    # Wait for Alertmanager
    timeout 60 bash -c 'until curl -f http://localhost:9093/-/healthy > /dev/null 2>&1; do sleep 2; done'
    print_success "Alertmanager is ready"
}

# Initialize Grafana
initialize_grafana() {
    print_status "Initializing Grafana..."

    # Wait for Grafana to be ready
    timeout 120 bash -c 'until curl -f http://localhost:3000/api/users > /dev/null 2>&1; do sleep 5; done'

    # Set admin password
    curl -X PUT -H "Content-Type: application/json" -d '{"name":"admin","email":"admin@erlmcp.com","password":"prom-operator"}' \
         http://localhost:3000/api/admin/users/1/password

    # Create datasources
    curl -X POST -H "Content-Type: application/json" -d @datasources/prometheus.yml \
         http://localhost:3000/api/datasources

    # Import dashboards
    import_dashboard "erlmcp-overview" "d/erlmcp-overview"
    import_dashboard "erlmcp-performance" "d/erlmcp-performance"
    import_dashboard "erlmcp-sla" "d/erlmcp-sla"

    print_success "Grafana initialized successfully"
}

# Import dashboard
import_dashboard() {
    local dashboard_id=$1
    local url_path=$2

    curl -X POST -H "Content-Type: application/json" \
         -d "{\"dashboardId\":\"$dashboard_id\",\"overwrite\":true}" \
         http://localhost:3000/api/dashboards/import
}

# Verify deployment
verify_deployment() {
    print_status "Verifying deployment..."

    # Check Prometheus
    if curl -f http://localhost:9090/api/v1/query?query=up > /dev/null 2>&1; then
        print_success "Prometheus is accessible"
    else
        print_error "Prometheus is not accessible"
        return 1
    fi

    # Check Grafana
    if curl -f http://localhost:3000/api/health > /dev/null 2>&1; then
        print_success "Grafana is accessible"
    else
        print_error "Grafana is not accessible"
        return 1
    fi

    # Check Loki
    if curl -f http://localhost:3100/loki/api/v1/status/buildinfo > /dev/null 2>&1; then
        print_success "Loki is accessible"
    else
        print_error "Loki is not accessible"
        return 1
    fi

    # Check Alertmanager
    if curl -f http://localhost:9093/-/healthy > /dev/null 2>&1; then
        print_success "Alertmanager is accessible"
    else
        print_error "Alertmanager is not accessible"
        return 1
    fi

    print_success "All services are accessible"
}

# Display access information
display_access_info() {
    print_status "Monitoring stack deployed successfully!"
    echo ""
    echo "Access Information:"
    echo "=================="
    echo "Grafana: http://localhost:3000"
    echo "  - Username: admin"
    echo "  - Password: prom-operator"
    echo ""
    echo "Prometheus: http://localhost:9090"
    echo ""
    echo "Alertmanager: http://localhost:9093"
    echo ""
    echo "Loki: http://localhost:3100"
    echo ""
    echo "OpenTelemetry Collector: http://localhost:4317 (gRPC) / http://localhost:4318 (HTTP)"
    echo ""
    echo "Log files:"
    echo "  - Prometheus: ./prometheus/data"
    echo "  - Grafana: ./grafana/data"
    echo "  - Loki: ./loki/data"
    echo "  - Alertmanager: ./alertmanager/data"
    echo ""
    echo "Dashboard URLs:"
    echo "  - Overview: http://localhost:3000/d/erlmcp-overview"
    echo "  - Performance: http://localhost:3000/d/erlmcp-performance"
    echo "  - SLA: http://localhost:3000/d/erlmcp-sla"
    echo ""
    echo "To view logs:"
    echo "  docker-compose -f docker/docker-compose.monitoring.yml logs -f"
    echo ""
    echo "To stop the stack:"
    echo "  docker-compose -f docker/docker-compose.monitoring.yml down"
    echo ""
    echo "To restart services:"
    echo "  docker-compose -f docker/docker-compose.monitoring.yml restart"
}

# Main deployment process
main() {
    echo "======================================"
    echo "erlmcp v3 Enterprise Monitoring Stack Deployment"
    echo "======================================"
    echo ""

    check_docker
    check_docker_compose
    create_directories
    start_monitoring_stack
    wait_for_services
    initialize_grafana
    verify_deployment
    display_access_info

    print_success "Deployment completed successfully!"
}

# Script options
case "${1:-}" in
    start)
        start_monitoring_stack
        ;;
    stop)
        print_status "Stopping monitoring stack..."
        docker-compose -f docker/docker-compose.monitoring.yml down
        print_success "Monitoring stack stopped"
        ;;
    restart)
        print_status "Restarting monitoring stack..."
        docker-compose -f docker/docker-compose.monitoring.yml restart
        print_success "Monitoring stack restarted"
        ;;
    logs)
        docker-compose -f docker/docker-compose.monitoring.yml logs -f
        ;;
    clean)
        print_status "Cleaning monitoring stack..."
        docker-compose -f docker/docker-compose.monitoring.yml down -v
        print_success "Monitoring stack cleaned"
        ;;
    *)
        main
        ;;
esac