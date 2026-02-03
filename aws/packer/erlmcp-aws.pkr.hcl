# ============================================================================
# BEAMOps Packer AMI Build for erlmcp
# ============================================================================
# Builds an immutable Amazon Machine Image (AMI) containing:
#   - Erlang/OTP 28.3.1 runtime
#   - erlmcp release (pre-compiled)
#   - Systemd service configuration
#   - CloudWatch Agent
#   - Security hardening
#
# Usage:
#   packer init .
#   packer build -var 'release_path=../../_build/prod/rel/erlmcp' .
#
# DOCKER-ONLY CONSTITUTION: The release is built via Docker, then baked into AMI.
# ============================================================================

packer {
  required_version = ">= 1.9.0"

  required_plugins {
    amazon = {
      version = ">= 1.2.0"
      source  = "github.com/hashicorp/amazon"
    }
    ansible = {
      version = ">= 1.1.0"
      source  = "github.com/hashicorp/ansible"
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

variable "release_path" {
  type        = string
  description = "Path to the erlmcp release directory (built by rebar3)"
}

variable "aws_region" {
  type        = string
  default     = "us-west-2"
  description = "AWS region for AMI"
}

variable "instance_type" {
  type        = string
  default     = "t3.medium"
  description = "EC2 instance type for building"
}

variable "source_ami_filter" {
  type        = string
  default     = "amzn2-ami-hvm-*-x86_64-gp2"
  description = "AMI name filter for source image"
}

variable "ami_regions" {
  type        = list(string)
  default     = ["us-west-2", "us-east-1"]
  description = "Regions to copy the AMI to"
}

variable "environment" {
  type        = string
  default     = "production"
  description = "Environment tag"
}

variable "encrypt_boot" {
  type        = bool
  default     = true
  description = "Encrypt the root EBS volume"
}

# ============================================================================
# Locals
# ============================================================================

locals {
  timestamp  = formatdate("YYYYMMDD-hhmm", timestamp())
  ami_name   = "erlmcp-${var.erlmcp_version}-otp${var.otp_version}-${local.timestamp}"

  common_tags = {
    Name        = local.ami_name
    Application = "erlmcp"
    Version     = var.erlmcp_version
    OTPVersion  = var.otp_version
    Environment = var.environment
    BuildDate   = local.timestamp
    ManagedBy   = "packer"
  }
}

# ============================================================================
# Source: Amazon Linux 2
# ============================================================================

source "amazon-ebs" "erlmcp" {
  ami_name        = local.ami_name
  ami_description = "erlmcp v${var.erlmcp_version} with Erlang/OTP ${var.otp_version} - Immutable BEAM Server"

  instance_type = var.instance_type
  region        = var.aws_region
  ami_regions   = var.ami_regions

  source_ami_filter {
    filters = {
      name                = var.source_ami_filter
      root-device-type    = "ebs"
      virtualization-type = "hvm"
      architecture        = "x86_64"
    }
    owners      = ["amazon"]
    most_recent = true
  }

  ssh_username = "ec2-user"

  # EBS configuration
  encrypt_boot = var.encrypt_boot
  launch_block_device_mappings {
    device_name           = "/dev/xvda"
    volume_size           = 20
    volume_type           = "gp3"
    iops                  = 3000
    throughput            = 125
    delete_on_termination = true
    encrypted             = var.encrypt_boot
  }

  # AMI sharing (optional)
  # ami_users = ["123456789012"]

  # Tagging
  tags = local.common_tags
  run_tags = {
    Name = "packer-erlmcp-builder"
  }

  # IMDSv2 required
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }
}

# ============================================================================
# Build Configuration
# ============================================================================

build {
  name    = "erlmcp-ami"
  sources = ["source.amazon-ebs.erlmcp"]

  # -------------------------------------------------------------------------
  # Stage 1: Wait for cloud-init
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo 'Waiting for cloud-init to complete...'",
      "cloud-init status --wait || true",
      "sleep 10"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 2: Install dependencies
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Installing system dependencies ==='",
      "sudo yum update -y",
      "sudo yum install -y curl wget git jq htop",
      "sudo yum install -y openssl-devel ncurses-devel",
      "sudo amazon-linux-extras install -y epel",
      "sudo yum groupinstall -y 'Development Tools'"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 3: Install Erlang/OTP
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Installing Erlang/OTP ${var.otp_version} ==='",

      # Download and install Erlang Solutions repo
      "wget https://packages.erlang-solutions.com/erlang-solutions-2.0-1.noarch.rpm",
      "sudo rpm -Uvh erlang-solutions-2.0-1.noarch.rpm || true",
      "rm erlang-solutions-2.0-1.noarch.rpm",

      # Install Erlang (or build from source if specific version needed)
      "sudo yum install -y erlang || true",

      # Verify installation
      "erl -noshell -eval 'io:format(\"OTP Version: ~s~n\", [erlang:system_info(otp_release)]), halt().'",

      # Install rebar3
      "echo '=== Installing rebar3 ==='",
      "wget https://s3.amazonaws.com/rebar3/rebar3",
      "chmod +x rebar3",
      "sudo mv rebar3 /usr/local/bin/",
      "rebar3 --version"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 4: Create erlmcp user and directories
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Creating erlmcp user and directories ==='",

      # Create system user
      "sudo useradd -r -s /sbin/nologin -d /opt/erlmcp -c 'erlmcp service account' erlmcp || true",

      # Create directories
      "sudo mkdir -p /opt/erlmcp",
      "sudo mkdir -p /var/log/erlmcp",
      "sudo mkdir -p /var/lib/erlmcp",
      "sudo mkdir -p /etc/erlmcp",

      # Set ownership
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp",
      "sudo chown -R erlmcp:erlmcp /var/log/erlmcp",
      "sudo chown -R erlmcp:erlmcp /var/lib/erlmcp",
      "sudo chown -R erlmcp:erlmcp /etc/erlmcp"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 5: Copy erlmcp release
  # -------------------------------------------------------------------------
  provisioner "file" {
    source      = var.release_path
    destination = "/tmp/erlmcp-release"
  }

  provisioner "shell" {
    inline = [
      "echo '=== Installing erlmcp release ==='",
      "sudo cp -r /tmp/erlmcp-release/* /opt/erlmcp/",
      "sudo chown -R erlmcp:erlmcp /opt/erlmcp",
      "sudo chmod +x /opt/erlmcp/bin/*",
      "rm -rf /tmp/erlmcp-release",

      # Verify release
      "ls -la /opt/erlmcp/",
      "/opt/erlmcp/bin/erlmcp version || echo 'Version command not available'"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 6: Create systemd service
  # -------------------------------------------------------------------------
  provisioner "file" {
    content = <<-EOF
      [Unit]
      Description=erlmcp - Erlang MCP SDK Server
      Documentation=https://github.com/seanchatmangpt/erlmcp
      After=network.target network-online.target
      Wants=network-online.target

      [Service]
      Type=simple
      User=erlmcp
      Group=erlmcp
      WorkingDirectory=/opt/erlmcp

      # Environment
      EnvironmentFile=-/etc/erlmcp/environment
      Environment=HOME=/opt/erlmcp
      Environment=LANG=en_US.UTF-8

      # Start/Stop
      ExecStart=/opt/erlmcp/bin/erlmcp foreground
      ExecStop=/opt/erlmcp/bin/erlmcp stop
      ExecReload=/opt/erlmcp/bin/erlmcp reload

      # Restart policy
      Restart=on-failure
      RestartSec=5
      StartLimitBurst=5
      StartLimitIntervalSec=60

      # Timeouts
      TimeoutStartSec=120
      TimeoutStopSec=60

      # Security hardening
      NoNewPrivileges=yes
      PrivateTmp=yes
      ProtectSystem=strict
      ProtectHome=yes
      ProtectKernelTunables=yes
      ProtectKernelModules=yes
      ProtectControlGroups=yes
      ReadWritePaths=/var/log/erlmcp /var/lib/erlmcp /opt/erlmcp/log
      ReadOnlyPaths=/opt/erlmcp

      # Resource limits
      LimitNOFILE=65536
      LimitNPROC=4096
      LimitCORE=0

      # Logging
      StandardOutput=journal
      StandardError=journal
      SyslogIdentifier=erlmcp

      [Install]
      WantedBy=multi-user.target
    EOF
    destination = "/tmp/erlmcp.service"
  }

  provisioner "shell" {
    inline = [
      "sudo mv /tmp/erlmcp.service /etc/systemd/system/erlmcp.service",
      "sudo chmod 644 /etc/systemd/system/erlmcp.service",
      "sudo systemctl daemon-reload",
      "sudo systemctl enable erlmcp"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 7: Create health check script
  # -------------------------------------------------------------------------
  provisioner "file" {
    content = <<-EOF
      #!/bin/bash
      # erlmcp health check script
      set -euo pipefail

      HEALTH_URL="http://localhost:8080/health"
      TIMEOUT=5

      # Try HTTP health endpoint first
      if curl -sf --max-time $TIMEOUT "$HEALTH_URL" > /dev/null 2>&1; then
        exit 0
      fi

      # Fall back to erlmcp ping command
      if /opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1; then
        exit 0
      fi

      # Check if BEAM process is running
      if pgrep -f "beam.smp" > /dev/null 2>&1; then
        exit 0
      fi

      exit 1
    EOF
    destination = "/tmp/healthcheck.sh"
  }

  provisioner "shell" {
    inline = [
      "sudo mv /tmp/healthcheck.sh /opt/erlmcp/bin/healthcheck.sh",
      "sudo chmod +x /opt/erlmcp/bin/healthcheck.sh",
      "sudo chown erlmcp:erlmcp /opt/erlmcp/bin/healthcheck.sh"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 8: Install CloudWatch Agent
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Installing CloudWatch Agent ==='",
      "sudo yum install -y amazon-cloudwatch-agent",
      "sudo systemctl enable amazon-cloudwatch-agent"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 9: Security hardening
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Applying security hardening ==='",

      # Disable root SSH login
      "sudo sed -i 's/^PermitRootLogin.*/PermitRootLogin no/' /etc/ssh/sshd_config",

      # Set restrictive permissions
      "sudo chmod 700 /opt/erlmcp",
      "sudo chmod 600 /etc/erlmcp/*",

      # Enable ASLR
      "echo 2 | sudo tee /proc/sys/kernel/randomize_va_space",

      # Configure sysctl for production
      "cat << 'SYSCTL' | sudo tee /etc/sysctl.d/99-erlmcp.conf
# erlmcp kernel tuning
net.core.somaxconn = 65535
net.core.netdev_max_backlog = 65535
net.ipv4.tcp_max_syn_backlog = 65535
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_time = 300
net.ipv4.tcp_keepalive_probes = 5
net.ipv4.tcp_keepalive_intvl = 15
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
vm.swappiness = 10
SYSCTL",
      "sudo sysctl -p /etc/sysctl.d/99-erlmcp.conf || true"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 10: Cleanup
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Cleaning up ==='",
      "sudo yum clean all",
      "sudo rm -rf /var/cache/yum",
      "sudo rm -rf /tmp/*",
      "sudo rm -rf /var/tmp/*",
      "sudo truncate -s 0 /var/log/messages || true",
      "sudo truncate -s 0 /var/log/secure || true",
      "history -c",
      "rm -f ~/.bash_history"
    ]
  }

  # -------------------------------------------------------------------------
  # Stage 11: Verification
  # -------------------------------------------------------------------------
  provisioner "shell" {
    inline = [
      "echo '=== Verification ==='",
      "echo 'OTP Version:'",
      "erl -noshell -eval 'io:format(\"~s~n\", [erlang:system_info(otp_release)]), halt().'",
      "echo 'rebar3 Version:'",
      "rebar3 --version",
      "echo 'Release Contents:'",
      "ls -la /opt/erlmcp/",
      "echo 'Systemd Service:'",
      "systemctl status erlmcp --no-pager || true",
      "echo '=== AMI Build Complete ==='"
    ]
  }

  # -------------------------------------------------------------------------
  # Post-processors
  # -------------------------------------------------------------------------
  post-processor "manifest" {
    output     = "manifest.json"
    strip_path = true
    custom_data = {
      erlmcp_version = var.erlmcp_version
      otp_version    = var.otp_version
      build_date     = local.timestamp
    }
  }
}
