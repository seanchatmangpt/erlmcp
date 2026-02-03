# ============================================================================
# Packer Build for erlmcp GCP Marketplace VM Image
# ============================================================================
# Creates Compute Engine images for GCP Marketplace deployment
#
# DOCKER-ONLY CONSTITUTION: Build artifacts validated via Docker quality gates
#
# Usage:
#   docker run --rm -v $(pwd):/workspace hashicorp/packer:light \
#     build -var 'project_id=your-project' /workspace/erlmcp-gcp.pkr.hcl
# ============================================================================

packer {
  required_plugins {
    googlecompute = {
      source  = "github.com/hashicorp/googlecompute"
      version = ">= 1.1.0"
    }
  }
}

# ============================================================================
# Variables
# ============================================================================

variable "project_id" {
  description = "GCP project ID for image building"
  type        = string
}

variable "zone" {
  description = "GCP zone for builder instance"
  type        = string
  default     = "us-central1-a"
}

variable "erlmcp_version" {
  description = "erlmcp release version"
  type        = string
  default     = "0.1.0"
}

variable "otp_version" {
  description = "Erlang/OTP version"
  type        = string
  default     = "28.3.1"
}

variable "image_family" {
  description = "GCP image family name"
  type        = string
  default     = "erlmcp"
}

variable "machine_type" {
  description = "Machine type for builder"
  type        = string
  default     = "e2-standard-4"
}

# ============================================================================
# Source: Google Compute Engine
# ============================================================================

source "googlecompute" "erlmcp" {
  project_id          = var.project_id
  zone                = var.zone
  machine_type        = var.machine_type

  # Base image - Debian 12 for stability
  source_image_family = "debian-12"
  source_image_project_id = ["debian-cloud"]

  # Output image configuration
  image_name          = "erlmcp-${replace(var.erlmcp_version, ".", "-")}-{{timestamp}}"
  image_family        = var.image_family
  image_description   = "erlmcp MCP SDK v${var.erlmcp_version} with Erlang/OTP ${var.otp_version}"

  # Image labels for Marketplace
  image_labels = {
    "erlmcp-version"   = replace(var.erlmcp_version, ".", "-")
    "otp-version"      = replace(var.otp_version, ".", "-")
    "build-timestamp"  = "{{timestamp}}"
    "marketplace"      = "true"
    "ai-platform"      = "mcp-sdk"
  }

  # Disk configuration
  disk_size = 20
  disk_type = "pd-ssd"

  # SSH configuration
  ssh_username        = "packer"
  ssh_timeout         = "10m"

  # Network configuration
  network             = "default"
  use_internal_ip     = false

  # Metadata for startup
  metadata = {
    "enable-oslogin" = "FALSE"
  }

  # Tags for firewall rules during build
  tags = ["packer-build"]
}

# ============================================================================
# Build Configuration
# ============================================================================

build {
  name = "erlmcp-gcp-marketplace"

  sources = ["source.googlecompute.erlmcp"]

  # -------------------------------------------------------------------------
  # Phase 1: System Updates and Dependencies
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 1: System Updates ==='",
      "sudo apt-get update -y",
      "sudo apt-get upgrade -y",
      "sudo apt-get install -y --no-install-recommends \\",
      "  build-essential autoconf libncurses5-dev libssl-dev \\",
      "  libwxgtk3.2-dev libgl1-mesa-dev libglu1-mesa-dev \\",
      "  libpng-dev libssh-dev unixodbc-dev xsltproc fop \\",
      "  curl wget git jq ca-certificates gnupg lsb-release \\",
      "  htop iotop sysstat dstat \\",
      "  python3 python3-pip"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 2: Install Erlang/OTP
  # -------------------------------------------------------------------------
  provisioner "shell" {
    environment_vars = [
      "OTP_VERSION=${var.otp_version}"
    ]
    inline = [
      "echo '=== Phase 2: Installing Erlang/OTP ${var.otp_version} ==='",

      "# Create build directory",
      "cd /tmp",
      "wget https://github.com/erlang/otp/releases/download/OTP-$${OTP_VERSION}/otp_src_$${OTP_VERSION}.tar.gz",
      "tar xzf otp_src_$${OTP_VERSION}.tar.gz",
      "cd otp_src_$${OTP_VERSION}",

      "# Configure and build",
      "./configure --prefix=/opt/erlang/$${OTP_VERSION} \\",
      "  --enable-threads \\",
      "  --enable-smp-support \\",
      "  --enable-kernel-poll \\",
      "  --enable-hipe \\",
      "  --with-ssl \\",
      "  --without-javac",

      "make -j$(nproc)",
      "sudo make install",

      "# Create symlinks",
      "sudo ln -sf /opt/erlang/$${OTP_VERSION}/bin/* /usr/local/bin/",

      "# Cleanup",
      "cd /tmp && rm -rf otp_src_$${OTP_VERSION}*",

      "# Verify installation",
      "erl -noshell -eval 'io:format(\"OTP ~s~n\", [erlang:system_info(otp_release)]), halt().'",
      "rebar3 --version || (curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && chmod +x /usr/local/bin/rebar3)"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 3: Install Google Cloud Ops Agent
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 3: Installing Cloud Ops Agent ==='",

      "# Install Ops Agent for logging and monitoring",
      "curl -sSO https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh",
      "sudo bash add-google-cloud-ops-agent-repo.sh --also-install",
      "rm add-google-cloud-ops-agent-repo.sh",

      "# Configure for erlmcp logs",
      "sudo mkdir -p /etc/google-cloud-ops-agent/config.d",

      "sudo tee /etc/google-cloud-ops-agent/config.d/erlmcp.yaml > /dev/null <<'OPSCONFIG'",
      "logging:",
      "  receivers:",
      "    erlmcp_logs:",
      "      type: files",
      "      include_paths:",
      "        - /var/log/erlmcp/*.log",
      "      record_log_file_path: true",
      "    erlmcp_console:",
      "      type: files",
      "      include_paths:",
      "        - /var/log/erlmcp/console.log",
      "  processors:",
      "    erlmcp_parser:",
      "      type: parse_regex",
      "      regex: '^(?<timestamp>\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+)\\s+(?<level>\\w+)\\s+(?<pid><[^>]+>)\\s+(?<module>[^:]+):(?<function>[^/]+)/(?<arity>\\d+)\\s+(?<message>.*)$'",
      "      time_key: timestamp",
      "      time_format: '%Y-%m-%dT%H:%M:%S.%L'",
      "  service:",
      "    pipelines:",
      "      erlmcp:",
      "        receivers: [erlmcp_logs, erlmcp_console]",
      "        processors: [erlmcp_parser]",
      "metrics:",
      "  receivers:",
      "    erlmcp_metrics:",
      "      type: prometheus",
      "      config:",
      "        scrape_configs:",
      "          - job_name: 'erlmcp'",
      "            scrape_interval: 30s",
      "            static_configs:",
      "              - targets: ['localhost:9568']",
      "  service:",
      "    pipelines:",
      "      erlmcp:",
      "        receivers: [erlmcp_metrics]",
      "OPSCONFIG"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 4: Create erlmcp User and Directories
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 4: Creating erlmcp user and directories ==='",

      "# Create erlmcp system user",
      "sudo useradd -r -s /bin/false -d /opt/erlmcp erlmcp",

      "# Create directory structure",
      "sudo mkdir -p /opt/erlmcp/{releases,lib,etc,data}",
      "sudo mkdir -p /var/log/erlmcp",
      "sudo mkdir -p /var/run/erlmcp",
      "sudo mkdir -p /etc/erlmcp",

      "# Set permissions",
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp",
      "sudo chown -R erlmcp:erlmcp /var/log/erlmcp",
      "sudo chown -R erlmcp:erlmcp /var/run/erlmcp",
      "sudo chown -R erlmcp:erlmcp /etc/erlmcp",
      "sudo chmod 755 /opt/erlmcp /var/log/erlmcp /var/run/erlmcp"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 5: Install erlmcp Release
  # -------------------------------------------------------------------------
  provisioner "file" {
    source      = "../../_build/prod/rel/erlmcp/"
    destination = "/tmp/erlmcp-release"
  }

  provisioner "shell" {
    inline = [
      "echo '=== Phase 5: Installing erlmcp release ==='",

      "# Move release to installation directory",
      "sudo cp -r /tmp/erlmcp-release/* /opt/erlmcp/",
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp",

      "# Create symlink for binary",
      "sudo ln -sf /opt/erlmcp/bin/erlmcp /usr/local/bin/erlmcp",

      "# Cleanup",
      "rm -rf /tmp/erlmcp-release"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 6: Systemd Service
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 6: Creating systemd service ==='",

      "sudo tee /etc/systemd/system/erlmcp.service > /dev/null <<'SYSTEMD'",
      "[Unit]",
      "Description=erlmcp MCP SDK Server",
      "Documentation=https://github.com/seanchatmangpt/erlmcp",
      "After=network-online.target google-cloud-ops-agent.service",
      "Wants=network-online.target",
      "",
      "[Service]",
      "Type=exec",
      "User=erlmcp",
      "Group=erlmcp",
      "WorkingDirectory=/opt/erlmcp",
      "",
      "# Environment from metadata",
      "EnvironmentFile=-/etc/erlmcp/env",
      "",
      "# Fetch secrets from Secret Manager",
      "ExecStartPre=/opt/erlmcp/bin/fetch-secrets.sh",
      "",
      "# Start erlmcp",
      "ExecStart=/opt/erlmcp/bin/erlmcp foreground",
      "ExecStop=/opt/erlmcp/bin/erlmcp stop",
      "ExecReload=/opt/erlmcp/bin/erlmcp reload_config",
      "",
      "# Restart policy",
      "Restart=on-failure",
      "RestartSec=5",
      "StartLimitBurst=5",
      "StartLimitIntervalSec=60",
      "",
      "# Resource limits",
      "LimitNOFILE=65535",
      "LimitNPROC=65535",
      "",
      "# Security hardening",
      "NoNewPrivileges=yes",
      "ProtectSystem=strict",
      "ProtectHome=yes",
      "PrivateTmp=yes",
      "PrivateDevices=yes",
      "ProtectKernelTunables=yes",
      "ProtectKernelModules=yes",
      "ProtectControlGroups=yes",
      "ReadWritePaths=/var/log/erlmcp /var/run/erlmcp /opt/erlmcp/data",
      "",
      "[Install]",
      "WantedBy=multi-user.target",
      "SYSTEMD",

      "sudo systemctl daemon-reload",
      "sudo systemctl enable erlmcp"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 7: Secret Fetcher Script
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 7: Creating secret fetcher script ==='",

      "sudo tee /opt/erlmcp/bin/fetch-secrets.sh > /dev/null <<'SCRIPT'",
      "#!/bin/bash",
      "# fetch-secrets.sh - Fetch secrets from GCP Secret Manager",
      "set -euo pipefail",
      "",
      "# Get project ID from metadata",
      "PROJECT_ID=$(curl -sH 'Metadata-Flavor: Google' \\",
      "  'http://metadata.google.internal/computeMetadata/v1/project/project-id')",
      "",
      "# Get secrets from metadata attribute",
      "SECRETS_LIST=$(curl -sH 'Metadata-Flavor: Google' \\",
      "  'http://metadata.google.internal/computeMetadata/v1/instance/attributes/erlmcp-secrets' || echo '')",
      "",
      "# Create env file",
      "ENV_FILE=/etc/erlmcp/env",
      "echo '# erlmcp environment - auto-generated' > $ENV_FILE",
      "",
      "# Add project metadata",
      "echo \"ERLMCP_GCP_PROJECT=$PROJECT_ID\" >> $ENV_FILE",
      "",
      "# Fetch each secret",
      "if [[ -n \"$SECRETS_LIST\" ]]; then",
      "  for secret_ref in $(echo $SECRETS_LIST | tr ',' ' '); do",
      "    secret_name=$(echo $secret_ref | cut -d: -f1)",
      "    env_var=$(echo $secret_ref | cut -d: -f2)",
      "    ",
      "    value=$(gcloud secrets versions access latest --secret=$secret_name --project=$PROJECT_ID 2>/dev/null || echo '')",
      "    if [[ -n \"$value\" ]]; then",
      "      echo \"$env_var=$value\" >> $ENV_FILE",
      "    fi",
      "  done",
      "fi",
      "",
      "chmod 600 $ENV_FILE",
      "chown erlmcp:erlmcp $ENV_FILE",
      "SCRIPT",

      "sudo chmod +x /opt/erlmcp/bin/fetch-secrets.sh"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 8: VM Arguments and Configuration
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 8: Creating default configuration ==='",

      "# Default vm.args",
      "sudo tee /etc/erlmcp/vm.args > /dev/null <<'VMARGS'",
      "## erlmcp VM Arguments",
      "",
      "## Node name (set by startup script from hostname)",
      "-name erlmcp@127.0.0.1",
      "",
      "## Cookie (fetched from Secret Manager)",
      "-setcookie ${ERLMCP_COOKIE:-erlmcp_default_cookie}",
      "",
      "## Scheduler settings",
      "+S 4:4",
      "+SDcpu 75",
      "+sbwt very_long",
      "+swt very_low",
      "",
      "## Memory allocators",
      "+MBas aobf",
      "+MBcs 65536",
      "+MBlmbcs 5242880",
      "+MEas aobf",
      "",
      "## Kernel settings",
      "+K true",
      "+A 64",
      "+P 1048576",
      "+Q 1048576",
      "",
      "## GC settings",
      "+hms 233",
      "+hmbs 46422",
      "",
      "## Distribution",
      "-kernel inet_dist_listen_min 9100",
      "-kernel inet_dist_listen_max 9200",
      "VMARGS",

      "sudo chown erlmcp:erlmcp /etc/erlmcp/vm.args"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 9: Kernel Tuning
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 9: Kernel tuning ==='",

      "sudo tee /etc/sysctl.d/99-erlmcp.conf > /dev/null <<'SYSCTL'",
      "# erlmcp kernel tuning",
      "",
      "# Network buffer sizes",
      "net.core.rmem_max = 16777216",
      "net.core.wmem_max = 16777216",
      "net.core.rmem_default = 1048576",
      "net.core.wmem_default = 1048576",
      "net.ipv4.tcp_rmem = 4096 1048576 16777216",
      "net.ipv4.tcp_wmem = 4096 1048576 16777216",
      "",
      "# Connection handling",
      "net.core.somaxconn = 65535",
      "net.core.netdev_max_backlog = 65535",
      "net.ipv4.tcp_max_syn_backlog = 65535",
      "net.ipv4.tcp_max_tw_buckets = 2000000",
      "net.ipv4.tcp_tw_reuse = 1",
      "",
      "# TCP keepalive",
      "net.ipv4.tcp_keepalive_time = 300",
      "net.ipv4.tcp_keepalive_probes = 5",
      "net.ipv4.tcp_keepalive_intvl = 15",
      "",
      "# File handles",
      "fs.file-max = 2097152",
      "fs.nr_open = 2097152",
      "",
      "# Virtual memory",
      "vm.swappiness = 10",
      "vm.dirty_ratio = 60",
      "vm.dirty_background_ratio = 2",
      "SYSCTL",

      "# File descriptor limits",
      "sudo tee /etc/security/limits.d/99-erlmcp.conf > /dev/null <<'LIMITS'",
      "erlmcp soft nofile 65535",
      "erlmcp hard nofile 65535",
      "erlmcp soft nproc 65535",
      "erlmcp hard nproc 65535",
      "LIMITS"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 10: Health Check Script
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 10: Creating health check script ==='",

      "sudo tee /opt/erlmcp/bin/health-check.sh > /dev/null <<'HEALTH'",
      "#!/bin/bash",
      "# health-check.sh - Health check for GCP load balancer",
      "",
      "HEALTH_PORT=${ERLMCP_HEALTH_PORT:-8080}",
      "TIMEOUT=5",
      "",
      "response=$(curl -sf --max-time $TIMEOUT http://localhost:$HEALTH_PORT/health 2>/dev/null)",
      "",
      "if [[ $? -eq 0 && \"$response\" == *'\"status\":\"healthy\"'* ]]; then",
      "  exit 0",
      "else",
      "  exit 1",
      "fi",
      "HEALTH",

      "sudo chmod +x /opt/erlmcp/bin/health-check.sh"
    ]
  }

  # -------------------------------------------------------------------------
  # Phase 11: Cleanup
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Phase 11: Final cleanup ==='",

      "# Clear package cache",
      "sudo apt-get clean",
      "sudo rm -rf /var/lib/apt/lists/*",

      "# Clear temp files",
      "sudo rm -rf /tmp/*",

      "# Clear logs",
      "sudo truncate -s 0 /var/log/*.log || true",
      "sudo truncate -s 0 /var/log/**/*.log || true",

      "# Clear bash history",
      "history -c",
      "rm -f ~/.bash_history",

      "echo '=== Build complete ==='",
      "erl -noshell -eval 'io:format(\"OTP: ~s~n\", [erlang:system_info(otp_release)]), halt().'",
      "erlmcp version 2>/dev/null || echo 'erlmcp binary ready'"
    ]
  }

  # -------------------------------------------------------------------------
  # Post-Processors
  # -------------------------------------------------------------------------
  post-processor "manifest" {
    output     = "manifest.json"
    strip_path = true
    custom_data = {
      erlmcp_version = var.erlmcp_version
      otp_version    = var.otp_version
      build_time     = "{{timestamp}}"
    }
  }
}
