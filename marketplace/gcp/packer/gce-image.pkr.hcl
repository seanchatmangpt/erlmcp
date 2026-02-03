# Packer template for building erlmcp GCE VM Image
# Target: Google Compute Engine custom image with OTP 28.3.1
#
# Build:
#   packer build -var "project_id=your-project" gce-image.pkr.hcl
#
# Requirements:
#   - Packer >= 1.9.0
#   - gcloud configured with application default credentials
#   - Google Compute Engine API enabled
#   - IAM permissions: compute.instances.create, compute.images.create

# ============================================================================
# Variables
# ============================================================================
variable "project_id" {
  type        = string
  description = "GCP project ID"
  default     = "${env("GOOGLE_CLOUD_PROJECT")}"
}

variable "zone" {
  type        = string
  description = "GCP zone for temporary build instance"
  default     = "us-central1-a"
}

variable "source_image_family" {
  type        = string
  description = "Source image family"
  default     = "ubuntu-2204-lts"
}

variable "source_image_project" {
  type        = string
  description = "Project containing the source image"
  default     = "ubuntu-os-cloud"
}

variable "machine_type" {
  type        = string
  description = "Machine type for build instance"
  default     = "e2-medium"
}

variable "image_family" {
  type        = string
  description = "Image family for created image"
  default     = "erlmcp-3"
}

variable "image_name" {
  type        = string
  description = "Base name for created image (timestamp will be appended)"
  default     = "erlmcp-3-0-0"
}

variable "erlmcp_version" {
  type        = string
  description = "erlmcp version to install"
  default     = "3.0.0"
}

variable "build_date" {
  type        = string
  description = "Build date in ISO 8601 format"
  default     = "${formatdate("YYYY-MM-DD", timestamp())}"
}

variable "git_sha" {
  type        = string
  description = "Git commit SHA"
  default     = "unknown"
}

# ============================================================================
# Source
# ============================================================================
source "googlecompute" "erlmcp" {
  project_id              = var.project_id
  zone                    = var.zone
  source_image_family     = var.source_image_family
  source_image_project_id = var.source_image_project
  machine_type            = var.machine_type
  image_name              = "${var.image_name}-${replace(var.build_date, "-", "")}"
  image_family            = var.image_family
  image_labels = {
    erlmcp_version    = var.erlmcp_version
    build_date        = var.build_date
    git_sha           = var.git_sha
    maintainer        = "erlmcp"
    source_image_hash = "{{ .SourceImageDetails }}"
  }
  ssh_username = "ubuntu"
  tags = [
    "packer-builder",
    "erlmcp-build"
  ]
}

# ============================================================================
# Build Steps
# ============================================================================
build {
  name = "erlmcp-gce-image"
  sources = [
    "source.googlecompute.erlmcp"
  ]

  # ========================================================================
  # Provisioner: File Upload
  # ========================================================================
  provisioner "file" {
    destination = "/tmp/erlmcp-provisioner.sh"
    content = <<-EOT
      #!/bin/bash
      set -euo pipefail

      ERLMCP_VERSION="${var.erlmcp_version}"
      ERLANG_VERSION="28"
      OTP_VERSION="28.3.1"

      echo "=== Installing erlmcp ${ERLMCP_VERSION} ==="

      # Add Erlang Solutions repository
      curl -fsSL https://binaries2.erlang-solutions.com/GPG-KEY-pmanager.asc \
        | gpg --dearmor -o /usr/share/keyrings/erlang-solutions.gpg
      echo "deb [signed-by=/usr/share/keyrings/erlang-solutions.gpg] \
        https://binaries2.erlang-solutions.com/ubuntu $(lsb_release -cs) contrib" \
        | tee /etc/apt/sources.list.d/erlang-solutions.list

      # Install build dependencies
      apt-get update
      apt-get install -y \
        build-essential \
        autoconf \
        libncurses5-dev \
        libssl-dev \
        libwxgtk3.2-dev \
        libgl1-mesa-dev \
        libglu1-mesa-dev \
        libpng-dev \
        unixodbc-dev \
        xsltproc \
        fop \
        libxml2-utils \
        wget \
        curl \
        git \
        ca-certificates

      # Install Erlang/OTP from Erlang Solutions
      apt-get install -y \
        esl-erlang \
        elixir \
        fop

      # Verify installation
      erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

      # Install rebar3
      curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 \
        -o /usr/local/bin/rebar3
      chmod +x /usr/local/bin/rebar3

      # Create erlmcp user
      useradd -m -s /bin/bash -u 1000 erlmcp || true
      mkdir -p /opt/erlmcp
      chown -R erlmcp:erlmcp /opt/erlmcp

      # Install erlmcp from GitHub release (if available)
      # or build from source
      cd /tmp
      git clone https://github.com/banyan-platform/erlmcp.git || true
      cd erlmcp || exit 1

      # Build production release
      su - erlmcp -c "cd /tmp/erlmcp && rebar3 as prod release"

      # Copy release to installation directory
      cp -r /tmp/erlmcp/_build/prod/rel/erlmcp/* /opt/erlmcp/
      chown -R erlmcp:erlmcp /opt/erlmcp

      # Create systemd service
      cat > /etc/systemd/system/erlmcp.service <<'EOF'
      [Unit]
      Description=erlmcp Erlang/OTP MCP Server
      After=network.target
      Wants=network-online.target

      [Service]
      Type=notify
      User=erlmcp
      Group=erlmcp
      WorkingDirectory=/opt/erlmcp
      Environment="ERLMCP_ENV=production"
      Environment="ERLMCP_VERSION=${ERLMCP_VERSION}"
      Environment="ERL_AFLAGS=-proto_dist inet_tls"
      Environment="ERL_DIST_PORT=9100"
      ExecStart=/opt/erlmcp/bin/erlmcp foreground
      ExecStop=/opt/erlmcp/bin/erlmcp stop
      Restart=always
      RestartSec=10
      LimitNOFILE=65536
      LimitNPROC=4096

      [Install]
      WantedBy=multi-user.target
      EOF

      systemctl daemon-reload
      systemctl enable erlmcp

      # Create startup script for secret injection
      cat > /opt/erlmcp/bin/gcp-startup.sh <<'EOF'
      #!/bin/bash
      set -euo pipefail

      # Fetch secrets from Secret Manager
      get_secret() {
        local secret_name="$1"
        local output_var="$2"
        if gcloud secrets versions access latest --secret="${secret_name}" 2>/dev/null; then
          export "${output_var}=$(gcloud secrets versions access latest --secret="${secret_name}")"
        fi
      }

      # Try to fetch secrets (will succeed if IAM is configured)
      get_secret "erlmcp-erlang-cookie" "ERLANG_COOKIE"
      get_secret "erlmcp-db-password" "DB_PASSWORD"
      get_secret "erlmcp-redis-password" "REDIS_PASSWORD"

      # Start erlmcp
      exec /opt/erlmcp/bin/erlmcp foreground
      EOF
      chmod +x /opt/erlmcp/bin/gcp-startup.sh
      chown erlmcp:erlmcp /opt/erlmcp/bin/gcp-startup.sh

      # Configure firewall
      ufw allow 8080/tcp comment "erlmcp HTTP API"
      ufw allow 9100/tcp comment "erlmcp Metrics"
      ufw allow 9100:9200/tcp comment "erlmcp Distribution"

      echo "=== erlmcp installation complete ==="
      echo "Version: ${ERLMCP_VERSION}"
      echo "Erlang/OTP: $(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), halt().')"
    EOT
  }

  provisioner "shell" {
    inline = [
      "chmod +x /tmp/erlmcp-provisioner.sh",
      "sudo /tmp/erlmcp-provisioner.sh"
    ]
  }

  # ========================================================================
  # Install Google Cloud Ops Agent
  # ========================================================================
  provisioner "shell" {
    inline = [
      "curl -sSO https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh",
      "sudo bash add-google-cloud-ops-agent-repo.sh --also-install",
      "sudo rm -f add-google-cloud-ops-agent-repo.sh"
    ]
  }

  # ========================================================================
  # Configure Ops Agent for erlmcp
  # ========================================================================
  provisioner "file" {
    destination = "/tmp/google-cloud-ops-agent.conf"
    content = <<-EOT
      metrics:
        service:
          enable_consumption_monitor: false
        receivers:
          # Erlang VM metrics via Prometheus endpoint
          prometheus_exporter:
            config:
              scrape_interval: 60s
              metrics_path: /metrics
              static_configs:
                - targets: ['localhost:9100']
        processors:
          metrics_filter:
            # Only include erlmcp-specific metrics
            metric_whitelist:
              - erlmcp_*
              - erlang_*
              - otp_*
        service:
          pipelines:
            erlmcp_pipeline:
              receivers: [prometheus_exporter]
              processors: [metrics_filter]
              exporters: [google_cloud_monitoring]

      logging:
        receivers:
          erlmcp_files:
            type: files
            include_paths:
              - /var/log/erlmcp/*.log
              - /opt/erlmcp/log/*.log
          erlmcp_syslog:
            type: syslog
            listen_host: 127.0.0.1
            listen_port: 9001
        processors:
          parse_erlmcp_logs:
            type: parse_json
        service:
          pipelines:
            erlmcp_logs:
              receivers: [erlmcp_files, erlmcp_syslog]
              processors: [parse_erlmcp_logs]
              exporters: [google_cloud_logging]
    EOT
  }

  provisioner "shell" {
    inline = [
      "sudo cp /tmp/google-cloud-ops-agent.conf /etc/google-cloud-ops-agent/config.yaml",
      "sudo systemctl restart google-cloud-ops-agent"
    ]
  }

  # ========================================================================
  # Image Metadata
  # ========================================================================
  provisioner "shell" {
    inline = [
      "echo 'erlmcp ${var.erlmcp_version}' | sudo tee /etc/erlmcp-version",
      "echo 'Build Date: ${var.build_date}' | sudo tee -a /etc/erlmcp-version",
      "echo 'Git SHA: ${var.git_sha}' | sudo tee -a /etc/erlmcp-version"
    ]
  }

  # ========================================================================
  # Cleanup
  # ========================================================================
  provisioner "shell" {
    inline = [
      "sudo apt-get clean",
      "sudo apt-get autoremove -y",
      "sudo rm -rf /tmp/* /var/tmp/*",
      "sudo rm -rf /home/ubuntu/.cache"
    ]
  }
}
