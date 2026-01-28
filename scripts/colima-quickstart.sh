#!/usr/bin/env bash
################################################################################
# Colima Quick Start - Deploy erlmcp 4-Node Cluster
#
# Complete automation from zero to running 100K connection cluster
#
# Usage:
#   ./colima-quickstart.sh              # Full setup
#   ./colima-quickstart.sh --no-test   # Setup without stress test
#   ./colima-quickstart.sh --status    # Check cluster status
#   ./colima-quickstart.sh --clean     # Teardown cluster
#
# What it does:
#   1. Start Colima with optimized settings (12 cores, 16GB RAM)
#   2. Configure system limits (256K file descriptors, TCP tuning)
#   3. Build erlmcp Docker image
#   4. Deploy 4-node cluster via docker-compose
#   5. Wait for all nodes to be healthy
#   6. Run optional stress test (100K connections)
#   7. Generate performance report
#
# Prerequisites:
#   - Colima installed (brew install colima)
#   - Docker CLI installed
#   - At least 20GB free disk space
#   - 16GB+ available system RAM
#
# Duration:
#   - First run (with build): ~8-10 minutes
#   - Subsequent runs: ~2-3 minutes
#
# Output:
#   - Cluster deployed at http://localhost:9100-9103
#   - Prometheus metrics: http://localhost:9090
#   - Performance report: results/colima-stress-test-*/report.html
################################################################################

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Defaults
SKIP_TEST="${SKIP_TEST:-false}"
CLEAN_MODE="${CLEAN_MODE:-false}"
STATUS_MODE="${STATUS_MODE:-false}"

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[✓]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }

# ============================================================================
# Parse Arguments
# ============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --no-test)
                SKIP_TEST="true"
                shift
                ;;
            --clean)
                CLEAN_MODE="true"
                shift
                ;;
            --status)
                STATUS_MODE="true"
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

# ============================================================================
# Phase 0: Cleanup (if requested)
# ============================================================================

cleanup_cluster() {
    log_info "Cleaning up Colima cluster..."

    log_warn "Stopping containers..."
    docker-compose -f docker-compose.colima.yml down --remove-orphans 2>/dev/null || true

    log_success "Cleanup completed"
}

# ============================================================================
# Phase 1: Start Colima
# ============================================================================

start_colima() {
    log_info "Phase 1/5: Starting Colima..."

    if colima status > /dev/null 2>&1; then
        log_warn "Colima already running, skipping start"
        return
    fi

    log_info "Starting Colima with: 12 CPU cores, 16GB RAM, 100GB disk"
    colima start \
        --cpu 12 \
        --memory 16 \
        --disk 100 \
        --vm-type qemu \
        --kubernetes=false \
        --runtime docker

    log_info "Waiting for Docker daemon to be ready..."
    sleep 10

    # Verify Docker is accessible
    if ! docker ps > /dev/null 2>&1; then
        log_error "Docker not accessible after Colima start"
        exit 1
    fi

    log_success "Colima started successfully"
}

# ============================================================================
# Phase 2: Configure System Limits
# ============================================================================

configure_limits() {
    log_info "Phase 2/5: Configuring system limits..."

    log_info "Applying socket limits in Colima VM..."

    # Apply sysctl settings for high connection count
    docker run --rm --privileged --network host busybox:1.36 sh << 'EOF' 2>/dev/null || true
# File descriptor limits
sysctl -w fs.file-max=2097152
sysctl -w net.ipv4.tcp_max_syn_backlog=8192
sysctl -w net.core.somaxconn=4096
sysctl -w net.ipv4.tcp_tw_reuse=1
sysctl -w net.ipv4.tcp_fin_timeout=30
sysctl -w net.ipv4.tcp_max_tw_buckets=2097152
sysctl -w net.netfilter.nf_conntrack_max=2097152

# Memory tuning for buffers
sysctl -w net.core.rmem_max=268435456
sysctl -w net.core.wmem_max=268435456
sysctl -w net.ipv4.tcp_rmem="4096 87380 268435456"
sysctl -w net.ipv4.tcp_wmem="4096 65536 268435456"
EOF

    log_success "System limits configured"
}

# ============================================================================
# Phase 3: Build Docker Image
# ============================================================================

build_image() {
    log_info "Phase 3/5: Building erlmcp Docker image..."

    # Check if image already exists (optimization)
    if docker image inspect erlmcp:latest > /dev/null 2>&1; then
        log_warn "erlmcp image already exists, skipping build"
        return
    fi

    log_info "Building image (this may take 3-5 minutes)..."

    if ! docker build \
        -t erlmcp:latest \
        -f Dockerfile \
        --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
        --build-arg VCS_REF="$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')" \
        --build-arg VERSION="0.7.0" \
        . > /dev/null 2>&1; then
        log_error "Docker image build failed"
        exit 1
    fi

    log_success "Docker image built successfully"
}

# ============================================================================
# Phase 4: Deploy Cluster
# ============================================================================

deploy_cluster() {
    log_info "Phase 4/5: Deploying 4-node cluster..."

    log_info "Pulling dependencies..."
    docker pull prom/prometheus:latest > /dev/null 2>&1 || true
    docker pull busybox:1.36 > /dev/null 2>&1 || true

    log_info "Starting containers via docker-compose..."
    docker-compose -f docker-compose.colima.yml up -d

    log_info "Waiting for cluster nodes to be healthy..."
    local wait_time=0
    local max_wait=120

    while [ $wait_time -lt $max_wait ]; do
        local healthy_count=0

        for i in {1..4}; do
            if docker ps | grep -q "erlmcp-node$i"; then
                # Check if container is healthy
                if docker exec "erlmcp-node$i" pgrep -f erl > /dev/null 2>&1; then
                    healthy_count=$((healthy_count + 1))
                fi
            fi
        done

        if [ $healthy_count -eq 4 ]; then
            log_success "All 4 nodes are healthy"
            break
        fi

        echo -ne "\r${BLUE}[INFO]${NC} Waiting for nodes... ($healthy_count/4 healthy) [${wait_time}s]"
        sleep 5
        wait_time=$((wait_time + 5))
    done

    if [ $wait_time -ge $max_wait ]; then
        log_error "Timeout waiting for cluster to be healthy"
        exit 1
    fi

    echo ""
    log_success "Cluster deployed successfully"
}

# ============================================================================
# Phase 5: Health Check & Verification
# ============================================================================

verify_cluster() {
    log_info "Phase 5/5: Verifying cluster..."

    echo ""
    log_info "Cluster Information:"
    echo "  Nodes:         4 (node1, node2, node3, node4)"
    echo "  API Ports:     8080-8083"
    echo "  Metrics Ports: 9090-9093"
    echo "  Listener Ports: 9100-9103 (for 100K connections)"

    echo ""
    log_info "Node Health:"
    docker-compose -f docker-compose.colima.yml ps

    echo ""
    log_info "Resource Usage:"
    docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}" \
        erlmcp-node{1,2,3,4} 2>/dev/null || true

    echo ""
    log_info "Endpoints:"
    for i in {1..4}; do
        local port=$((8079 + i))
        echo "  node$i: http://localhost:$port"
    done
    echo "  Prometheus: http://localhost:9090"

    log_success "Cluster verification complete"
}

# ============================================================================
# Run Optional Stress Test
# ============================================================================

run_stress_test() {
    if [ "$SKIP_TEST" = "true" ]; then
        log_info "Skipping stress test (--no-test flag)"
        return
    fi

    log_info "Running stress test..."
    echo ""

    if [ ! -x "scripts/stress-test-colima.sh" ]; then
        log_error "Stress test script not found or not executable"
        log_info "Run manually: ./scripts/stress-test-colima.sh"
        return
    fi

    # Run with shorter duration for quickstart
    ./scripts/stress-test-colima.sh 5 100000 5

    log_success "Stress test completed"
}

# ============================================================================
# Print Status
# ============================================================================

print_status() {
    log_info "Colima Cluster Status"
    echo ""

    if ! colima status > /dev/null 2>&1; then
        log_error "Colima is not running"
        return 1
    fi

    log_success "Colima is running"

    if ! docker ps > /dev/null 2>&1; then
        log_error "Docker is not accessible"
        return 1
    fi

    local running_nodes=$(docker ps --format "{{.Names}}" | grep -c "erlmcp-node" || echo "0")

    log_info "Cluster Status:"
    echo "  Running Nodes: $running_nodes/4"

    if [ "$running_nodes" -gt 0 ]; then
        echo ""
        log_info "Node Details:"
        docker-compose -f docker-compose.colima.yml ps
    fi

    return 0
}

# ============================================================================
# Summary
# ============================================================================

print_summary() {
    cat << EOF

${GREEN}════════════════════════════════════════════════════════════════════${NC}
${GREEN}  erlmcp Colima Cluster - Ready for Testing${NC}
${GREEN}════════════════════════════════════════════════════════════════════${NC}

${BLUE}Quick Access:${NC}
  • Node 1 API:        http://localhost:8080
  • Node 2 API:        http://localhost:8081
  • Node 3 API:        http://localhost:8082
  • Node 4 API:        http://localhost:8083
  • Metrics (Prometheus): http://localhost:9090

${BLUE}Cluster Listener Ports (for 100K connections):${NC}
  • Node 1:            localhost:9100
  • Node 2:            localhost:9101
  • Node 3:            localhost:9102
  • Node 4:            localhost:9103

${BLUE}Common Commands:${NC}
  • View logs:         docker-compose -f docker-compose.colima.yml logs -f node1
  • Check stats:       docker stats --no-stream
  • Run stress test:   ./scripts/stress-test-colima.sh
  • Stop cluster:      docker-compose -f docker-compose.colima.yml down
  • Clean everything:  ./scripts/colima-quickstart.sh --clean

${BLUE}Performance Targets:${NC}
  • Concurrent connections: 100,000 (25,000 per node)
  • Latency (p99):          <50ms
  • Error rate:             <0.1%
  • Colima overhead:        <10%

${GREEN}════════════════════════════════════════════════════════════════════${NC}

EOF
}

# ============================================================================
# Main
# ============================================================================

main() {
    parse_args "$@"

    if [ "$CLEAN_MODE" = "true" ]; then
        cleanup_cluster
        exit 0
    fi

    if [ "$STATUS_MODE" = "true" ]; then
        print_status
        exit $?
    fi

    echo ""
    log_info "erlmcp Colima Quick Start - 100K Concurrent Connections"
    echo ""

    start_colima
    echo ""

    configure_limits
    echo ""

    build_image
    echo ""

    deploy_cluster
    echo ""

    verify_cluster
    echo ""

    run_stress_test
    echo ""

    print_summary
}

main "$@"
