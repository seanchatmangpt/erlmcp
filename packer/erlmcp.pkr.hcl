# ==============================================================================
# ERLMCP PACKER TEMPLATE - Immutable Image Builder
# ==============================================================================
# Builds pre-baked machine images with OTP 28+ and erlmcp dependencies.
# Follows BEAMOps principles: immutable artifacts, explicit config.
#
# Usage:
#   packer init packer/
#   packer build -var 'gcp_project=your-project' packer/erlmcp.pkr.hcl
#
# Targets:
#   - GCP (Google Compute Engine images)
#   - AWS (AMI)
#   - Docker (for local testing)
# ==============================================================================

packer {
  required_plugins {
    googlecompute = {
      source  = "github.com/hashicorp/googlecompute"
      version = ">= 1.1.0"
    }
    amazon = {
      source  = "github.com/hashicorp/amazon"
      version = ">= 1.2.0"
    }
    docker = {
      source  = "github.com/hashicorp/docker"
      version = ">= 1.0.0"
    }
  }
}

# ==============================================================================
# Variables
# ==============================================================================

variable "otp_version" {
  type        = string
  default     = "28.3.1"
  description = "Erlang/OTP version to install"
}

variable "rebar3_version" {
  type        = string
  default     = "3.24.0"
  description = "Rebar3 version to install"
}

variable "erlmcp_version" {
  type        = string
  default     = "0.6.0"
  description = "erlmcp release version"
}

variable "gcp_project" {
  type        = string
  default     = ""
  description = "GCP project ID for image storage"
}

variable "gcp_zone" {
  type        = string
  default     = "us-central1-a"
  description = "GCP zone for builder instance"
}

variable "aws_region" {
  type        = string
  default     = "us-east-1"
  description = "AWS region for AMI"
}

variable "image_family" {
  type        = string
  default     = "erlmcp"
  description = "Image family name"
}

variable "base_image" {
  type        = string
  default     = "debian-12-bookworm-v20240312"
  description = "Base image for GCP"
}

variable "ami_base" {
  type        = string
  default     = "ami-0c7217cdde317cfec" # Debian 12 us-east-1
  description = "Base AMI for AWS"
}

# ==============================================================================
# Local Variables
# ==============================================================================

locals {
  timestamp   = formatdate("YYYYMMDD-hhmmss", timestamp())
  image_name  = "erlmcp-otp${var.otp_version}-${local.timestamp}"

  # Common provisioning script
  install_otp_script = <<-EOF
    #!/bin/bash
    set -euo pipefail

    echo "=== Installing OTP ${var.otp_version} ==="

    # Install dependencies
    export DEBIAN_FRONTEND=noninteractive
    apt-get update
    apt-get install -y --no-install-recommends \
      build-essential \
      autoconf \
      libncurses5-dev \
      libssl-dev \
      libwxgtk3.2-dev \
      libgl1-mesa-dev \
      libglu1-mesa-dev \
      libpng-dev \
      libssh-dev \
      unixodbc-dev \
      xsltproc \
      fop \
      git \
      curl \
      ca-certificates \
      gnupg \
      lsb-release

    # Install Erlang/OTP from Erlang Solutions
    curl -fsSL https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb -o /tmp/erlang-solutions.deb
    dpkg -i /tmp/erlang-solutions.deb
    apt-get update
    apt-get install -y esl-erlang=1:${var.otp_version}-1

    # Verify installation
    erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

    echo "=== Installing Rebar3 ${var.rebar3_version} ==="
    curl -fsSL https://github.com/erlang/rebar3/releases/download/${var.rebar3_version}/rebar3 -o /usr/local/bin/rebar3
    chmod +x /usr/local/bin/rebar3
    rebar3 --version

    echo "=== Setting up erlmcp user ==="
    useradd -r -m -s /bin/bash -u 1000 erlmcp || true
    mkdir -p /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp
    chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp

    echo "=== Configuring system limits ==="
    cat >> /etc/security/limits.conf <<LIMITS
    erlmcp soft nofile 65536
    erlmcp hard nofile 65536
    erlmcp soft nproc 32768
    erlmcp hard nproc 32768
    LIMITS

    echo "=== Configuring sysctl for BEAM ==="
    cat >> /etc/sysctl.d/99-erlmcp.conf <<SYSCTL
    # Increase ephemeral port range
    net.ipv4.ip_local_port_range = 1024 65535

    # Reduce TIME_WAIT for faster port recycling
    net.ipv4.tcp_fin_timeout = 30
    net.ipv4.tcp_tw_reuse = 1

    # Increase connection backlog
    net.core.somaxconn = 65535
    net.core.netdev_max_backlog = 65535

    # TCP buffer sizes
    net.core.rmem_max = 16777216
    net.core.wmem_max = 16777216
    net.ipv4.tcp_rmem = 4096 87380 16777216
    net.ipv4.tcp_wmem = 4096 65536 16777216

    # VM memory settings
    vm.swappiness = 10
    vm.overcommit_memory = 1
    SYSCTL

    sysctl -p /etc/sysctl.d/99-erlmcp.conf || true

    echo "=== Creating systemd service ==="
    cat > /etc/systemd/system/erlmcp.service <<SERVICE
    [Unit]
    Description=erlmcp MCP Server
    After=network.target

    [Service]
    Type=simple
    User=erlmcp
    Group=erlmcp
    WorkingDirectory=/opt/erlmcp
    Environment=HOME=/opt/erlmcp
    Environment=LANG=en_US.UTF-8
    Environment=ERL_CRASH_DUMP=/var/log/erlmcp/erl_crash.dump
    ExecStart=/opt/erlmcp/bin/erlmcp foreground
    ExecStop=/opt/erlmcp/bin/erlmcp stop
    Restart=on-failure
    RestartSec=5
    LimitNOFILE=65536
    LimitNPROC=32768

    # Security hardening
    NoNewPrivileges=yes
    ProtectSystem=strict
    ProtectHome=yes
    ReadWritePaths=/opt/erlmcp /var/log/erlmcp /var/lib/erlmcp
    PrivateTmp=yes

    [Install]
    WantedBy=multi-user.target
    SERVICE

    systemctl daemon-reload

    echo "=== Cleanup ==="
    apt-get clean
    rm -rf /var/lib/apt/lists/* /tmp/*

    echo "=== Image build complete ==="
    erl -eval 'io:format("OTP ~s ready~n", [erlang:system_info(otp_release)]), halt().' -noshell
  EOF
}

# ==============================================================================
# GCP Image Builder
# ==============================================================================

source "googlecompute" "erlmcp" {
  project_id          = var.gcp_project
  zone                = var.gcp_zone
  source_image_family = "debian-12"
  source_image        = var.base_image
  ssh_username        = "packer"
  machine_type        = "e2-standard-4"
  disk_size           = 20
  disk_type           = "pd-ssd"

  image_name          = local.image_name
  image_family        = var.image_family
  image_description   = "erlmcp with OTP ${var.otp_version} - Built ${local.timestamp}"

  image_labels = {
    otp_version    = replace(var.otp_version, ".", "-")
    erlmcp_version = replace(var.erlmcp_version, ".", "-")
    builder        = "packer"
    managed_by     = "terraform"
  }

  # Network tags for firewall rules during build
  tags = ["packer-builder"]

  # Use preemptible instance for cost savings
  preemptible = true
}

# ==============================================================================
# AWS AMI Builder
# ==============================================================================

source "amazon-ebs" "erlmcp" {
  region        = var.aws_region
  source_ami    = var.ami_base
  instance_type = "t3.large"
  ssh_username  = "admin"

  ami_name        = local.image_name
  ami_description = "erlmcp with OTP ${var.otp_version} - Built ${local.timestamp}"

  ami_groups = ["all"] # Make public (remove for private AMIs)

  tags = {
    Name           = local.image_name
    OTP_Version    = var.otp_version
    erlmcp_Version = var.erlmcp_version
    Builder        = "packer"
    ManagedBy      = "terraform"
  }

  # EBS volume configuration
  launch_block_device_mappings {
    device_name           = "/dev/xvda"
    volume_size           = 20
    volume_type           = "gp3"
    delete_on_termination = true
  }

  # Use spot instance for cost savings
  spot_price = "auto"
  spot_instance_types = ["t3.large", "t3a.large", "t3.xlarge"]
}

# ==============================================================================
# Docker Builder (for local testing)
# ==============================================================================

source "docker" "erlmcp" {
  image  = "debian:bookworm-slim"
  commit = true

  changes = [
    "ENV OTP_VERSION=${var.otp_version}",
    "ENV REBAR3_VERSION=${var.rebar3_version}",
    "ENV ERLMCP_VERSION=${var.erlmcp_version}",
    "USER erlmcp",
    "WORKDIR /opt/erlmcp",
    "EXPOSE 8080 9090 4369",
    "ENTRYPOINT [\"/opt/erlmcp/bin/erlmcp\"]",
    "CMD [\"foreground\"]"
  ]
}

# ==============================================================================
# Build Definitions
# ==============================================================================

build {
  name = "erlmcp-images"

  sources = [
    "source.googlecompute.erlmcp",
    "source.amazon-ebs.erlmcp",
    "source.docker.erlmcp"
  ]

  # Install OTP and dependencies
  provisioner "shell" {
    inline = [local.install_otp_script]

    # Run as root for installation
    execute_command = "chmod +x {{ .Path }}; sudo -S sh -c '{{ .Vars }} {{ .Path }}'"

    # Environment variables
    environment_vars = [
      "DEBIAN_FRONTEND=noninteractive",
      "OTP_VERSION=${var.otp_version}",
      "REBAR3_VERSION=${var.rebar3_version}"
    ]
  }

  # Verify the build
  provisioner "shell" {
    inline = [
      "echo '=== Verification ==='",
      "erl -eval 'io:format(\"OTP: ~s~n\", [erlang:system_info(otp_release)]), halt().' -noshell",
      "rebar3 --version",
      "cat /etc/systemd/system/erlmcp.service",
      "ulimit -n",
      "echo '=== Build verified ==='",
    ]
  }

  # Post-processors for Docker
  post-processor "docker-tag" {
    only       = ["docker.erlmcp"]
    repository = "erlmcp/erlmcp-base"
    tags       = ["otp-${var.otp_version}", "latest"]
  }

  # Manifest for tracking builds
  post-processor "manifest" {
    output     = "packer-manifest.json"
    strip_path = true
    custom_data = {
      otp_version    = var.otp_version
      rebar3_version = var.rebar3_version
      erlmcp_version = var.erlmcp_version
      build_time     = local.timestamp
    }
  }
}
