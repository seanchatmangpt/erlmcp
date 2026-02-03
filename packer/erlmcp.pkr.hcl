# Packer Configuration for erlmcp v3
# Builds machine images for GCP and AWS
#
# Build GCP: packer build -only=googlecompute.erlmcp .
# Build AWS: packer build -only=amazon-ebs.erlmcp .
# Build All: packer build .
#
# Prerequisites:
#   - GOOGLE_APPLICATION_CREDENTIALS for GCP
#   - AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY for AWS

packer {
  required_version = ">= 1.9.0"

  required_plugins {
    googlecompute = {
      version = ">= 1.1.0"
      source  = "github.com/hashicorp/googlecompute"
    }
    amazon = {
      version = ">= 1.2.0"
      source  = "github.com/hashicorp/amazon"
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

variable "gcp_project_id" {
  type        = string
  default     = env("GCP_PROJECT_ID")
  description = "GCP project ID"
}

variable "gcp_zone" {
  type        = string
  default     = "us-central1-a"
  description = "GCP zone for building"
}

variable "aws_region" {
  type        = string
  default     = "us-east-1"
  description = "AWS region for building"
}

variable "aws_instance_type" {
  type        = string
  default     = "t3.medium"
  description = "AWS instance type for building"
}

variable "ssh_username" {
  type        = string
  default     = "packer"
  description = "SSH username for provisioning"
}

# ============================================================================
# Locals
# ============================================================================

locals {
  timestamp  = formatdate("YYYYMMDD-hhmm", timestamp())
  image_name = "erlmcp-${var.erlmcp_version}-otp${var.otp_version}-${local.timestamp}"

  common_labels = {
    application = "erlmcp"
    version     = var.erlmcp_version
    otp_version = var.otp_version
    built_by    = "packer"
    built_at    = local.timestamp
  }
}

# ============================================================================
# GCP Image (Google Compute Engine)
# ============================================================================

source "googlecompute" "erlmcp" {
  project_id          = var.gcp_project_id
  zone                = var.gcp_zone
  source_image_family = "debian-12"
  image_name          = local.image_name
  image_description   = "erlmcp v${var.erlmcp_version} with Erlang/OTP ${var.otp_version}"
  image_family        = "erlmcp"
  image_labels        = local.common_labels

  machine_type = "e2-standard-4"
  disk_size    = 20
  disk_type    = "pd-ssd"

  ssh_username = var.ssh_username

  metadata = {
    enable-oslogin = "false"
  }

  tags = ["packer", "erlmcp"]
}

# ============================================================================
# AWS AMI (Amazon Machine Image)
# ============================================================================

source "amazon-ebs" "erlmcp" {
  region        = var.aws_region
  instance_type = var.aws_instance_type

  source_ami_filter {
    filters = {
      name                = "debian-12-amd64-*"
      root-device-type    = "ebs"
      virtualization-type = "hvm"
    }
    owners      = ["136693071363"] # Debian official
    most_recent = true
  }

  ami_name        = local.image_name
  ami_description = "erlmcp v${var.erlmcp_version} with Erlang/OTP ${var.otp_version}"

  ami_virtualization_type = "hvm"

  ssh_username = "admin"

  tags = merge(local.common_labels, {
    Name = local.image_name
  })

  run_tags = {
    Name = "packer-erlmcp-builder"
  }

  launch_block_device_mappings {
    device_name           = "/dev/xvda"
    volume_size           = 20
    volume_type           = "gp3"
    delete_on_termination = true
  }
}

# ============================================================================
# Build Configuration
# ============================================================================

build {
  name = "erlmcp"

  sources = [
    "source.googlecompute.erlmcp",
    "source.amazon-ebs.erlmcp"
  ]

  # Wait for cloud-init to complete
  provisioner "shell" {
    inline = [
      "echo 'Waiting for cloud-init to complete...'",
      "cloud-init status --wait || true",
      "sleep 10"
    ]
  }

  # Update system packages
  provisioner "shell" {
    inline = [
      "sudo apt-get update -y",
      "sudo apt-get upgrade -y",
      "sudo apt-get install -y curl wget git build-essential libssl-dev libncurses5-dev"
    ]
  }

  # Install Erlang/OTP from Erlang Solutions
  provisioner "shell" {
    inline = [
      "echo 'Installing Erlang/OTP ${var.otp_version}...'",
      "wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb",
      "sudo dpkg -i erlang-solutions_2.0_all.deb",
      "sudo apt-get update -y",
      "sudo apt-get install -y esl-erlang=${var.otp_version}-1 || sudo apt-get install -y esl-erlang",
      "rm erlang-solutions_2.0_all.deb",
      "erl -noshell -eval 'io:format(\"OTP Version: ~s~n\", [erlang:system_info(otp_release)]), halt().'"
    ]
  }

  # Install rebar3
  provisioner "shell" {
    inline = [
      "echo 'Installing rebar3...'",
      "wget https://s3.amazonaws.com/rebar3/rebar3",
      "chmod +x rebar3",
      "sudo mv rebar3 /usr/local/bin/",
      "rebar3 --version"
    ]
  }

  # Create erlmcp user and directories
  provisioner "shell" {
    inline = [
      "echo 'Creating erlmcp user and directories...'",
      "sudo useradd -r -s /bin/false -d /opt/erlmcp erlmcp || true",
      "sudo mkdir -p /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp /etc/erlmcp",
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp /etc/erlmcp"
    ]
  }

  # Create systemd service file
  provisioner "file" {
    content = <<-EOF
      [Unit]
      Description=erlmcp - Erlang MCP SDK Server
      Documentation=https://github.com/banyan-platform/erlmcp
      After=network.target

      [Service]
      Type=simple
      User=erlmcp
      Group=erlmcp
      WorkingDirectory=/opt/erlmcp
      ExecStart=/opt/erlmcp/bin/erlmcp foreground
      ExecStop=/opt/erlmcp/bin/erlmcp stop
      Restart=on-failure
      RestartSec=5
      TimeoutStartSec=60
      TimeoutStopSec=30

      # Security hardening
      NoNewPrivileges=yes
      PrivateTmp=yes
      ProtectSystem=strict
      ProtectHome=yes
      ReadWritePaths=/var/log/erlmcp /var/lib/erlmcp

      # Resource limits
      LimitNOFILE=65536
      LimitNPROC=4096

      # Environment
      Environment=ERLMCP_ENV=production
      Environment=HOME=/opt/erlmcp
      EnvironmentFile=-/etc/erlmcp/environment

      [Install]
      WantedBy=multi-user.target
    EOF
    destination = "/tmp/erlmcp.service"
  }

  provisioner "shell" {
    inline = [
      "sudo mv /tmp/erlmcp.service /etc/systemd/system/erlmcp.service",
      "sudo systemctl daemon-reload",
      "sudo systemctl enable erlmcp"
    ]
  }

  # Create health check script
  provisioner "shell" {
    inline = [
      "cat << 'HEALTHCHECK' | sudo tee /opt/erlmcp/bin/healthcheck.sh",
      "#!/bin/bash",
      "set -euo pipefail",
      "HEALTH_URL=\"http://localhost:8080/health\"",
      "if curl -sf --max-time 5 \"$HEALTH_URL\" > /dev/null 2>&1; then",
      "  exit 0",
      "fi",
      "if /opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1; then",
      "  exit 0",
      "fi",
      "exit 1",
      "HEALTHCHECK",
      "sudo chmod +x /opt/erlmcp/bin/healthcheck.sh || true"
    ]
  }

  # Clean up
  provisioner "shell" {
    inline = [
      "echo 'Cleaning up...'",
      "sudo apt-get autoremove -y",
      "sudo apt-get clean",
      "sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*",
      "sudo truncate -s 0 /var/log/syslog || true",
      "sudo truncate -s 0 /var/log/auth.log || true",
      "history -c"
    ]
  }

  # Verify installation
  provisioner "shell" {
    inline = [
      "echo '=== erlmcp Image Build Complete ==='",
      "echo 'OTP Version:'",
      "erl -noshell -eval 'io:format(\"~s~n\", [erlang:system_info(otp_release)]), halt().'",
      "echo 'rebar3 Version:'",
      "rebar3 --version",
      "echo 'Systemd service:'",
      "systemctl status erlmcp --no-pager || true"
    ]
  }

  post-processor "manifest" {
    output     = "packer-manifest.json"
    strip_path = true
  }
}
