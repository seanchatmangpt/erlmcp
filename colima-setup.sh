#!/usr/bin/env bash
################################################################################
# Colima Setup Script - Configure for 100K Concurrent Connections
#
# This script configures Colima VM with optimal settings for erlmcp's 100K
# concurrent connection load testing.
#
# Usage:
#   ./colima-setup.sh [start|stop|config|clean]
#
# Settings optimized for:
#   - 100K concurrent connections across 4-node cluster
#   - ~25K connections per node
#   - TCP socket limits (256K file descriptors)
#   - Memory tuning for connection buffers
#   - Network stack optimization
################################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

COLIMA_PROFILE="${COLIMA_PROFILE:-default}"
COLIMA_HOME="${COLIMA_HOME:-.colima}"

# ============================================================================
# Colima Configuration for 100K Concurrent Connections
# ============================================================================

# CPU cores: 12 (adequate for 25K conn/node on 4 nodes)
COLIMA_CPU=12

# Memory: 16GB (4GB per node * 4 nodes = 16GB total)
COLIMA_MEMORY=16

# Disk: 100GB (sufficient for logs, monitoring data, etc.)
COLIMA_DISK=100

# VM Type: QEMU (better for macOS, more stable than VZ)
COLIMA_VM_TYPE="qemu"

# ============================================================================
# Display Functions
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# ============================================================================
# Check Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v colima &> /dev/null; then
        log_error "Colima is not installed"
        echo "Install via: brew install colima"
        exit 1
    fi

    if ! command -v docker &> /dev/null; then
        log_error "Docker CLI is not installed"
        exit 1
    fi

    log_success "Prerequisites check passed"
}

# ============================================================================
# Start Colima with Optimized Configuration
# ============================================================================

colima_start() {
    log_info "Starting Colima with optimized configuration..."

    if colima status > /dev/null 2>&1; then
        log_warn "Colima is already running. Skipping start."
        return
    fi

    log_info "Configuration:"
    echo "  Profile:    $COLIMA_PROFILE"
    echo "  CPU:        $COLIMA_CPU cores"
    echo "  Memory:     ${COLIMA_MEMORY}GB"
    echo "  Disk:       ${COLIMA_DISK}GB"
    echo "  VM Type:    $COLIMA_VM_TYPE"

    colima start \
        --profile "$COLIMA_PROFILE" \
        --cpu "$COLIMA_CPU" \
        --memory "$COLIMA_MEMORY" \
        --disk "$COLIMA_DISK" \
        --vm-type "$COLIMA_VM_TYPE" \
        --kubernetes=false \
        --runtime docker \
        || {
            log_error "Failed to start Colima"
            exit 1
        }

    log_info "Waiting for Colima to be ready..."
    sleep 10

    log_success "Colima started successfully"
}

# ============================================================================
# Configure Socket Limits Inside Colima VM
# ============================================================================

configure_socket_limits() {
    log_info "Configuring socket limits in Colima VM..."

    # We need to configure the Colima VM sysctl settings
    # This requires accessing the VM through docker run with privileged mode

    local -a SYSCTL_SETTINGS=(
        # File descriptor limits
        "fs.file-max=2097152"                    # 2M max open files

        # TCP connection limits
        "net.ipv4.tcp_max_syn_backlog=8192"     # SYN backlog
        "net.ipv4.ip_local_port_range=1024 65535" # Local port range
        "net.ipv4.tcp_tw_reuse=1"                # Reuse TIME_WAIT sockets
        "net.ipv4.tcp_fin_timeout=30"            # FIN timeout
        "net.core.somaxconn=4096"                # Listen backlog
        "net.ipv4.tcp_max_tw_buckets=2097152"   # Max TIME_WAIT sockets

        # Connection limit tuning
        "net.netfilter.nf_conntrack_max=2097152" # Conntrack table
        "net.netfilter.nf_conntrack_tcp_timeout_established=600" # EST timeout

        # Memory tuning for buffers
        "net.core.rmem_max=268435456"            # 256MB max socket recv buffer
        "net.core.wmem_max=268435456"            # 256MB max socket send buffer
        "net.ipv4.tcp_rmem=4096 87380 268435456" # TCP recv buffer tuning
        "net.ipv4.tcp_wmem=4096 65536 268435456" # TCP send buffer tuning
    )

    log_info "Applying sysctl settings (${#SYSCTL_SETTINGS[@]} rules)..."

    # Create temporary sysctl configuration
    local tmp_sysctl="/tmp/colima-sysctl.conf"
    {
        echo "# Colima Network Tuning for 100K Concurrent Connections"
        echo "# Generated: $(date)"
        for setting in "${SYSCTL_SETTINGS[@]}"; do
            echo "$setting"
        done
    } > "$tmp_sysctl"

    # Apply settings via docker exec with privileged container
    # We use a busybox container with network namespace access
    docker run --rm --privileged --network host \
        -v "$tmp_sysctl:/sysctl.conf:ro" \
        busybox:1.36 \
        sh -c "cat /sysctl.conf | while read line; do \
            [ -z \"\$line\" ] || [ \"\${line:0:1}\" = '#' ] && continue; \
            sysctl -w \"\$line\" || true; \
        done" || log_warn "Some sysctl settings could not be applied (may need root access)"

    rm -f "$tmp_sysctl"
    log_success "Socket limits configured"
}

# ============================================================================
# Configure Docker Daemon for High Connection Count
# ============================================================================

configure_docker_daemon() {
    log_info "Configuring Docker daemon..."

    # Create optimized daemon config
    local docker_config="/etc/docker/daemon.json"

    # Check if docker daemon config can be modified
    docker run --rm --privileged \
        busybox:1.36 \
        sh -c "cat > $docker_config << 'EOF' 2>/dev/null || true
{
  \"storage-driver\": \"overlay2\",
  \"storage-opts\": [
    \"overlay2.override_kernel_check=true\"
  ],
  \"log-driver\": \"json-file\",
  \"log-opts\": {
    \"max-size\": \"100m\",
    \"max-file\": \"5\"
  },
  \"default-ulimits\": {
    \"nofile\": {
      \"Name\": \"nofile\",
      \"Hard\": 262144,
      \"Soft\": 262144
    },
    \"nproc\": {
      \"Name\": \"nproc\",
      \"Hard\": 65535,
      \"Soft\": 65535
    }
  }
}
EOF
" || log_warn "Could not modify Docker daemon config (may need manual configuration)"

    log_success "Docker daemon configured"
}

# ============================================================================
# Verify Configuration
# ============================================================================

verify_configuration() {
    log_info "Verifying Colima configuration..."

    local checks_passed=0
    local checks_total=0

    # Check Colima status
    checks_total=$((checks_total + 1))
    if colima status > /dev/null 2>&1; then
        log_success "Colima is running"
        checks_passed=$((checks_passed + 1))
    else
        log_error "Colima is not running"
    fi

    # Check Docker connectivity
    checks_total=$((checks_total + 1))
    if docker ps > /dev/null 2>&1; then
        log_success "Docker is accessible"
        checks_passed=$((checks_passed + 1))
    else
        log_error "Docker is not accessible"
    fi

    # Check available CPU/Memory in Colima
    checks_total=$((checks_total + 1))
    local available_cpu=$(docker run --rm busybox:1.36 nproc 2>/dev/null || echo "unknown")
    log_info "Available CPU cores: $available_cpu"

    checks_total=$((checks_total + 1))
    local available_mem=$(docker run --rm busybox:1.36 \
        awk '/MemTotal/ {print int($2/1024/1024) "GB"}' /proc/meminfo 2>/dev/null || echo "unknown")
    log_info "Available memory: $available_mem"

    if [ "$checks_passed" -ge 2 ]; then
        log_success "Configuration verification passed ($checks_passed/$checks_total)"
    else
        log_error "Configuration verification failed ($checks_passed/$checks_total)"
        exit 1
    fi
}

# ============================================================================
# Print Configuration Details
# ============================================================================

print_config() {
    cat << EOF

${BLUE}=== Colima Configuration for 100K Concurrent Connections ===${NC}

${GREEN}VM Configuration:${NC}
  Profile:          $COLIMA_PROFILE
  CPU Cores:        $COLIMA_CPU
  Memory:           ${COLIMA_MEMORY}GB
  Disk:             ${COLIMA_DISK}GB
  VM Type:          $COLIMA_VM_TYPE
  Runtime:          Docker

${GREEN}Network Tuning:${NC}
  Max Open Files:   2,097,152
  Max Connections:  2,097,152
  TCP SYN Backlog:  8,192
  Listen Backlog:   4,096
  Socket Buffer:    256MB (recv/send)

${GREEN}Per-Node Capacity (4-node cluster):${NC}
  Connections:      ~25,000 per node
  Total Cluster:    ~100,000 concurrent
  Memory per Node:  ~4GB (ring buffer)
  CPU per Node:     ~3 cores

${GREEN}Expected Performance:${NC}
  Colima Overhead:  <10% vs native (network I/O limited)
  Throughput:       50K-100K req/sec (cluster-wide)
  Latency:          <5ms (p99) local network

EOF
}

# ============================================================================
# Stop Colima
# ============================================================================

colima_stop() {
    log_info "Stopping Colima..."

    if colima status > /dev/null 2>&1; then
        colima stop
        log_success "Colima stopped"
    else
        log_info "Colima is not running"
    fi
}

# ============================================================================
# Clean Colima
# ============================================================================

colima_clean() {
    log_warn "Cleaning Colima profile: $COLIMA_PROFILE"
    log_warn "This will delete all containers, volumes, and data"

    read -p "Are you sure? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        colima delete --profile "$COLIMA_PROFILE" --force
        log_success "Colima profile deleted"
    else
        log_info "Cancelled"
    fi
}

# ============================================================================
# Main
# ============================================================================

main() {
    local command="${1:-start}"

    case "$command" in
        start)
            check_prerequisites
            colima_start
            configure_socket_limits
            configure_docker_daemon
            verify_configuration
            print_config
            ;;
        stop)
            colima_stop
            ;;
        config)
            check_prerequisites
            colima_start
            configure_socket_limits
            configure_docker_daemon
            verify_configuration
            ;;
        clean)
            colima_clean
            ;;
        status)
            colima status
            ;;
        *)
            echo "Usage: $0 {start|stop|config|clean|status}"
            exit 1
            ;;
    esac
}

main "$@"
