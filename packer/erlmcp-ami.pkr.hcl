# ============================================================================
# Packer Configuration for erlmcp Immutable Machine Images
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION - Immutable artifacts with proof
#
# Builds immutable VM images for:
#   - AWS (AMI)
#   - GCP (Compute Image)
#   - Azure (Managed Image)
#
# Usage:
#   packer init packer/
#   packer validate packer/erlmcp-ami.pkr.hcl
#   packer build -var 'erlmcp_version=3.0.0' packer/erlmcp-ami.pkr.hcl
#
# Artifacts:
#   - AMI with erlmcp release pre-installed
#   - systemd service configured
#   - OS-level tuning for BEAM VM
#   - Security hardening applied
# ============================================================================

packer {
  required_version = ">= 1.9.0"

  required_plugins {
    amazon = {
      version = ">= 1.2.0"
      source  = "github.com/hashicorp/amazon"
    }
    googlecompute = {
      version = ">= 1.1.0"
      source  = "github.com/hashicorp/googlecompute"
    }
  }
}

# ============================================================================
# Variables
# ============================================================================

variable "erlmcp_version" {
  type        = string
  default     = "3.0.0"
  description = "erlmcp application version"
}

variable "otp_version" {
  type        = string
  default     = "28.3.1"
  description = "Erlang/OTP version"
}

variable "base_ami" {
  type        = string
  default     = "ami-0c55b159cbfafe1f0" # Amazon Linux 2023
  description = "Base AMI ID for AWS"
}

variable "aws_region" {
  type        = string
  default     = "us-east-1"
  description = "AWS region for AMI"
}

variable "gcp_project" {
  type        = string
  default     = ""
  description = "GCP project ID"
}

variable "gcp_zone" {
  type        = string
  default     = "us-central1-a"
  description = "GCP zone for image"
}

variable "instance_type" {
  type        = string
  default     = "t3.medium"
  description = "Instance type for building"
}

variable "ssh_username" {
  type        = string
  default     = "ec2-user"
  description = "SSH username for provisioning"
}

variable "docker_image" {
  type        = string
  default     = ""
  description = "Docker image to pull (if using container-based deployment)"
}

variable "environment" {
  type        = string
  default     = "production"
  description = "Target environment (dev, staging, production)"
}

# ============================================================================
# Local Variables
# ============================================================================

locals {
  timestamp = regex_replace(timestamp(), "[- TZ:]", "")

  common_tags = {
    Application = "erlmcp"
    Version     = var.erlmcp_version
    OTPVersion  = var.otp_version
    Environment = var.environment
    BuildDate   = timestamp()
    ManagedBy   = "packer"
  }

  ami_name = "erlmcp-${var.erlmcp_version}-otp${var.otp_version}-${local.timestamp}"
}

# ============================================================================
# AWS AMI Builder
# ============================================================================

source "amazon-ebs" "erlmcp" {
  ami_name        = local.ami_name
  ami_description = "erlmcp v${var.erlmcp_version} on OTP ${var.otp_version} - Immutable Production Image"
  instance_type   = var.instance_type
  region          = var.aws_region
  source_ami      = var.base_ami
  ssh_username    = var.ssh_username

  # Use IMDSv2 for security
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }

  # EBS configuration
  launch_block_device_mappings {
    device_name           = "/dev/xvda"
    volume_size           = 20
    volume_type           = "gp3"
    delete_on_termination = true
    encrypted             = true
  }

  # AMI sharing (customize as needed)
  ami_users = []

  # Tags
  tags = merge(local.common_tags, {
    Name = local.ami_name
  })

  run_tags = {
    Name = "packer-builder-erlmcp-${local.timestamp}"
  }

  # Temporary security group
  temporary_security_group_source_cidrs = ["0.0.0.0/0"]
}

# ============================================================================
# GCP Image Builder
# ============================================================================

source "googlecompute" "erlmcp" {
  project_id          = var.gcp_project
  source_image_family = "ubuntu-2204-lts"
  zone                = var.gcp_zone
  machine_type        = "e2-medium"
  ssh_username        = "ubuntu"

  image_name        = local.ami_name
  image_description = "erlmcp v${var.erlmcp_version} on OTP ${var.otp_version} - Immutable Production Image"
  image_family      = "erlmcp"

  disk_size = 20
  disk_type = "pd-ssd"

  image_labels = {
    application = "erlmcp"
    version     = replace(var.erlmcp_version, ".", "-")
    otp_version = replace(var.otp_version, ".", "-")
    environment = var.environment
  }

  # Network tags for firewall rules
  tags = ["packer-builder", "erlmcp"]
}

# ============================================================================
# Build Configuration
# ============================================================================

build {
  name = "erlmcp-immutable-image"

  sources = [
    "source.amazon-ebs.erlmcp",
    # Uncomment for GCP builds:
    # "source.googlecompute.erlmcp",
  ]

  # =========================================================================
  # Provisioner: System Updates and Base Packages
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Installing base packages ==='",
      "sudo yum update -y || sudo apt-get update -y",
      "sudo yum install -y curl wget git make gcc openssl-devel ncurses-devel || sudo apt-get install -y curl wget git make gcc libssl-dev libncurses-dev",
    ]
  }

  # =========================================================================
  # Provisioner: Install Erlang/OTP
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Installing Erlang/OTP ${var.otp_version} ==='",

      # Install Erlang from Erlang Solutions repository
      "curl -fsSL https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb -o /tmp/erlang-solutions.deb || true",
      "sudo dpkg -i /tmp/erlang-solutions.deb 2>/dev/null || true",
      "sudo apt-get update -y || true",
      "sudo apt-get install -y esl-erlang || sudo yum install -y erlang",

      # Verify installation
      "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell",
    ]
  }

  # =========================================================================
  # Provisioner: Create erlmcp User and Directories
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Creating erlmcp user and directories ==='",

      # Create service user
      "sudo useradd -r -s /bin/false -d /opt/erlmcp erlmcp || true",

      # Create directories
      "sudo mkdir -p /opt/erlmcp",
      "sudo mkdir -p /var/log/erlmcp",
      "sudo mkdir -p /var/lib/erlmcp",
      "sudo mkdir -p /etc/erlmcp",

      # Set ownership
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp",
    ]
  }

  # =========================================================================
  # Provisioner: Install erlmcp Release (from Docker image or tarball)
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Installing erlmcp release ==='",

      # Option 1: Pull from Docker and extract
      "if [ -n '${var.docker_image}' ]; then",
      "  sudo yum install -y docker || sudo apt-get install -y docker.io",
      "  sudo systemctl start docker",
      "  sudo docker pull ${var.docker_image}",
      "  CONTAINER_ID=$(sudo docker create ${var.docker_image})",
      "  sudo docker cp $CONTAINER_ID:/opt/erlmcp /opt/",
      "  sudo docker rm $CONTAINER_ID",
      "fi",

      # Set permissions
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp",
      "sudo chmod +x /opt/erlmcp/bin/* 2>/dev/null || true",
    ]
  }

  # =========================================================================
  # Provisioner: OS-Level Tuning for BEAM VM
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Applying BEAM VM OS tuning ==='",

      # Increase file descriptor limits
      "echo 'erlmcp soft nofile 1000000' | sudo tee -a /etc/security/limits.conf",
      "echo 'erlmcp hard nofile 1000000' | sudo tee -a /etc/security/limits.conf",
      "echo 'root soft nofile 1000000' | sudo tee -a /etc/security/limits.conf",
      "echo 'root hard nofile 1000000' | sudo tee -a /etc/security/limits.conf",

      # Kernel parameters for high-performance networking
      "cat <<'EOF' | sudo tee /etc/sysctl.d/99-erlmcp.conf",
      "# erlmcp BEAM VM tuning",
      "",
      "# File descriptors",
      "fs.file-max = 2097152",
      "fs.nr_open = 2097152",
      "",
      "# Network tuning",
      "net.core.somaxconn = 65535",
      "net.core.netdev_max_backlog = 65535",
      "net.core.rmem_max = 16777216",
      "net.core.wmem_max = 16777216",
      "net.core.rmem_default = 1048576",
      "net.core.wmem_default = 1048576",
      "",
      "# TCP tuning",
      "net.ipv4.tcp_max_syn_backlog = 65535",
      "net.ipv4.tcp_fin_timeout = 15",
      "net.ipv4.tcp_keepalive_time = 300",
      "net.ipv4.tcp_keepalive_intvl = 30",
      "net.ipv4.tcp_keepalive_probes = 5",
      "net.ipv4.tcp_tw_reuse = 1",
      "net.ipv4.ip_local_port_range = 1024 65535",
      "",
      "# Memory",
      "vm.swappiness = 1",
      "vm.dirty_ratio = 80",
      "vm.dirty_background_ratio = 5",
      "EOF",

      # Apply sysctl settings
      "sudo sysctl -p /etc/sysctl.d/99-erlmcp.conf",
    ]
  }

  # =========================================================================
  # Provisioner: Create systemd Service
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Creating systemd service ==='",

      "cat <<'EOF' | sudo tee /etc/systemd/system/erlmcp.service",
      "[Unit]",
      "Description=erlmcp - Model Context Protocol SDK",
      "Documentation=https://erlmcp.dev",
      "After=network-online.target",
      "Wants=network-online.target",
      "",
      "[Service]",
      "Type=exec",
      "User=erlmcp",
      "Group=erlmcp",
      "WorkingDirectory=/opt/erlmcp",
      "",
      "# Environment",
      "Environment=ERLMCP_ENV=production",
      "Environment=HOME=/opt/erlmcp",
      "Environment=LANG=C.UTF-8",
      "Environment=ERL_MAX_PORTS=1000000",
      "Environment=ERL_MAX_ETS_TABLES=100000",
      "EnvironmentFile=-/etc/erlmcp/environment",
      "",
      "# Resource limits",
      "LimitNOFILE=1000000",
      "LimitNPROC=65535",
      "LimitCORE=infinity",
      "",
      "# Start command",
      "ExecStart=/opt/erlmcp/bin/erlmcp foreground",
      "ExecStop=/opt/erlmcp/bin/erlmcp stop",
      "",
      "# Restart policy",
      "Restart=on-failure",
      "RestartSec=5",
      "StartLimitInterval=60",
      "StartLimitBurst=3",
      "",
      "# Security hardening",
      "NoNewPrivileges=true",
      "ProtectSystem=strict",
      "ProtectHome=true",
      "ReadWritePaths=/var/log/erlmcp /var/lib/erlmcp /var/run/erlmcp",
      "PrivateTmp=true",
      "ProtectKernelTunables=true",
      "ProtectKernelModules=true",
      "ProtectControlGroups=true",
      "",
      "[Install]",
      "WantedBy=multi-user.target",
      "EOF",

      # Enable service
      "sudo systemctl daemon-reload",
      "sudo systemctl enable erlmcp",
    ]
  }

  # =========================================================================
  # Provisioner: Create Health Check Script
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Creating health check script ==='",

      "cat <<'SCRIPT' | sudo tee /opt/erlmcp/bin/healthcheck.sh",
      "#!/bin/bash",
      "set -euo pipefail",
      "",
      "HEALTH_URL=\"${HEALTH_URL:-http://localhost:8080/health}\"",
      "HEALTH_TIMEOUT=\"${HEALTH_TIMEOUT:-5}\"",
      "",
      "# Level 1: HTTP health endpoint",
      "if curl -f -s --max-time \"$HEALTH_TIMEOUT\" \"$HEALTH_URL\" > /dev/null 2>&1; then",
      "    echo 'healthy: application responding'",
      "    exit 0",
      "fi",
      "",
      "# Level 2: Node ping",
      "if /opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1; then",
      "    echo 'degraded: node responding but HTTP down'",
      "    exit 1",
      "fi",
      "",
      "# Level 3: Process check",
      "if pgrep -f 'beam.smp.*erlmcp' > /dev/null; then",
      "    echo 'degraded: process running but not responding'",
      "    exit 1",
      "fi",
      "",
      "echo 'down: no process found'",
      "exit 2",
      "SCRIPT",

      "sudo chmod +x /opt/erlmcp/bin/healthcheck.sh",
    ]
  }

  # =========================================================================
  # Provisioner: Security Hardening
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Applying security hardening ==='",

      # Remove unnecessary packages
      "sudo yum remove -y telnet ftp || sudo apt-get remove -y telnet ftp || true",

      # Disable root SSH login
      "sudo sed -i 's/PermitRootLogin yes/PermitRootLogin no/' /etc/ssh/sshd_config || true",

      # Set restrictive permissions
      "sudo chmod 700 /etc/erlmcp",
      "sudo chmod 600 /etc/erlmcp/* 2>/dev/null || true",

      # Clean up
      "sudo yum clean all || sudo apt-get clean",
      "sudo rm -rf /tmp/* /var/tmp/*",
    ]
  }

  # =========================================================================
  # Provisioner: Create Build Manifest
  # =========================================================================
  provisioner "shell" {
    inline = [
      "echo '=== Creating build manifest ==='",

      "cat <<EOF | sudo tee /opt/erlmcp/BUILD_MANIFEST",
      "# erlmcp Build Manifest",
      "# Generated by Packer",
      "",
      "ERLMCP_VERSION=${var.erlmcp_version}",
      "OTP_VERSION=${var.otp_version}",
      "BUILD_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ)",
      "BUILD_TIMESTAMP=${local.timestamp}",
      "ENVIRONMENT=${var.environment}",
      "PACKER_VERSION=$(packer version 2>/dev/null || echo 'unknown')",
      "IMAGE_NAME=${local.ami_name}",
      "EOF",

      "sudo chown erlmcp:erlmcp /opt/erlmcp/BUILD_MANIFEST",
    ]
  }

  # =========================================================================
  # Post-Processor: Manifest
  # =========================================================================
  post-processor "manifest" {
    output     = "packer-manifest.json"
    strip_path = true
    custom_data = {
      erlmcp_version = var.erlmcp_version
      otp_version    = var.otp_version
      environment    = var.environment
      build_date     = timestamp()
    }
  }
}
