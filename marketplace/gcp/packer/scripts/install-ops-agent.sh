#!/bin/bash
# Google Cloud Ops Agent Installation Script for erlmcp
# This script installs and configures the Ops Agent for logging and monitoring

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running on GCE
check_gce() {
    if ! curl -s -H "Metadata-Flavor: Google" \
       "http://metadata.google.internal/computeMetadata/v1/" &>/dev/null; then
        log_warn "Not running on Google Compute Engine"
        log_warn "Ops Agent installation may not work properly"
    fi
}

# Detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$ID
        OS_VERSION=$VERSION_ID
    else
        log_error "Cannot detect OS"
        exit 1
    fi
    log_info "Detected OS: $OS $OS_VERSION"
}

# Install Ops Agent
install_ops_agent() {
    log_info "Installing Google Cloud Ops Agent..."

    case "$OS" in
        ubuntu|debian)
            curl -sSO https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh
            sudo bash add-google-cloud-ops-agent-repo.sh --also-install
            sudo rm -f add-google-cloud-ops-agent-repo.sh
            ;;
        centos|rhel|rocky|almalinux)
            curl -sSO https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh
            sudo bash add-google-cloud-ops-agent-repo.sh --also-install
            sudo rm -f add-google-cloud-ops-agent-repo.sh
            ;;
        *)
            log_error "Unsupported OS: $OS"
            exit 1
            ;;
    esac

    log_info "Ops Agent installed successfully"
}

# Configure Ops Agent for erlmcp
configure_ops_agent() {
    log_info "Configuring Ops Agent for erlmcp..."

    CONFIG_DIR="/etc/google-cloud-ops-agent"
    CONFIG_FILE="$CONFIG_DIR/config.yaml"

    sudo mkdir -p "$CONFIG_DIR"

    sudo tee "$CONFIG_FILE" > /dev/null <<'EOF'
# Google Cloud Ops Agent Configuration for erlmcp
metrics:
  service:
    enable_consumption_monitor: false
  receivers:
    # Erlang VM metrics via Prometheus endpoint
    prometheus_exporter:
      type: prometheus_exporter
      config:
        scrape_interval: 60s
        metrics_path: /metrics
        static_configs:
          - targets: ['localhost:9100']
            labels:
              service: 'erlmcp'
  processors:
    metrics_filter:
      type: exclude_metrics
      # Exclude noisy system metrics
      patterns:
        - .*_cpu_seconds_total
        - .*_memory_.*
  service:
    pipelines:
      erlmcp_pipeline:
        receivers: [prometheus_exporter]
        processors: [metrics_filter]
        exporters: [google_cloud_monitoring]

logging:
  receivers:
    erlmapp_files:
      type: files
      include_paths:
        - /var/log/erlmcp/*.log
        - /opt/erlmcp/log/*.log
        - /var/log/syslog
      labels:
        service: 'erlmcp'
    systemd_journal:
      type: systemd_journal
  processors:
    parse_erlmcp_logs:
      type: parse_json
      field: body
    add_service_label:
      type: add_labels
      labels:
        service: erlmcp
  service:
    pipelines:
      erlmcp_logs:
        receivers: [erlmapp_files, systemd_journal]
        processors: [parse_erlmcp_logs, add_service_label]
        exporters: [google_cloud_logging]
EOF

    log_info "Configuration written to $CONFIG_FILE"
}

# Restart Ops Agent
restart_ops_agent() {
    log_info "Restarting Ops Agent..."

    if command -v systemctl &> /dev/null; then
        sudo systemctl restart google-cloud-ops-agent
        sudo systemctl enable google-cloud-ops-agent
        log_info "Ops Agent restarted with systemctl"
    elif command -v service &> /dev/null; then
        sudo service google-cloud-ops-agent restart
        log_info "Ops Agent restarted with service"
    else
        log_error "Cannot restart Ops Agent - no systemctl or service command"
        exit 1
    fi
}

# Verify Ops Agent status
verify() {
    log_info "Verifying Ops Agent status..."

    sleep 5

    if command -v systemctl &> /dev/null; then
        if sudo systemctl is-active --quiet google-cloud-ops-agent; then
            log_info "Ops Agent is running"
        else
            log_error "Ops Agent failed to start"
            sudo systemctl status google-cloud-ops-agent
            exit 1
        fi
    fi

    # Check if metrics endpoint is accessible
    if curl -s http://localhost:9100/metrics &> /dev/null; then
        log_info "Prometheus metrics endpoint is accessible"
    else
        log_warn "Prometheus metrics endpoint not yet accessible (may need to start erlmcp)"
    fi
}

# Main execution
main() {
    log_info "Starting Google Cloud Ops Agent installation for erlmcp..."

    check_gce
    detect_os
    install_ops_agent
    configure_ops_agent
    restart_ops_agent
    verify

    log_info "Google Cloud Ops Agent installation complete!"
}

# Run main function
main "$@"
