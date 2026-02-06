#!/bin/bash
set -euxo pipefail

# EKS Node Bootstrap Script for ErlMCP v3
# This script is used to bootstrap EKS worker nodes with proper configuration

# Variables passed from Terraform
CLUSTER_NAME="${cluster_name}"
CLUSTER_ENDPOINT="${cluster_endpoint}"
CLUSTER_CA_DATA="${cluster_ca_data}"
BOOTSTRAP_EXTRA_ARGS="${bootstrap_extra_args}"

# System configuration
echo "Configuring system settings..."
sysctl -w net.ipv4.ip_forward=1
sysctl -w net.bridge.bridge-nf-call-iptables=1
sysctl -w net.ipv4.conf.all.rp_filter=1

# Update system packages
echo "Updating system packages..."
yum update -y

# Install required packages
echo "Installing required packages..."
yum install -y \
    amazon-ssm-agent \
    aws-cli \
    jq \
    htop \
    wget \
    curl

# Enable and start SSM agent for remote management
echo "Enabling SSM agent..."
systemctl enable amazon-ssm-agent
systemctl start amazon-ssm-agent

# Configure Docker daemon
echo "Configuring Docker daemon..."
mkdir -p /etc/docker
cat > /etc/docker/daemon.json <<EOF
{
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "100m",
    "max-file": "5"
  },
  "storage-driver": "overlay2",
  "live-restore": true
}
EOF

# Restart Docker
systemctl restart docker

# Get node IP address
NODE_IP=$(curl -s http://169.254.169.254/latest/meta-data/local-ipv4)
echo "Node IP: $NODE_IP"

# Bootstrap the EKS node
echo "Bootstrapping EKS node..."
/etc/eks/bootstrap.sh "$CLUSTER_NAME" \
  --b64-cluster-ca "$CLUSTER_CA_DATA" \
  --apiserver-endpoint "$CLUSTER_ENDPOINT" \
  --kubelet-extra-args "--node-ip=$NODE_IP $BOOTSTRAP_EXTRA_ARGS"

# Configure kubelet
echo "Configuring kubelet..."
cat > /etc/systemd/system/kubelet.service.d/90-local.conf <<EOF
[Service]
CPUAccounting=true
MemoryAccounting=true
EOF

# Reload systemd and restart kubelet
systemctl daemon-reload
systemctl restart kubelet

# Setup CloudWatch agent (optional)
if command -v amazon-cloudwatch-agent-ctl &> /dev/null; then
    echo "Configuring CloudWatch agent..."
    cat > /opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json <<EOF
{
  "metrics": {
    "namespace": "ErlMCP/EKS",
    "metrics_collected": {
      "cpu": {
        "measurement": [
          {
            "name": "cpu_usage_idle",
            "rename": "CPU_USAGE_IDLE",
            "unit": "Percent"
          }
        ],
        "metrics_collection_interval": 60
      },
      "mem": {
        "measurement": [
          {
            "name": "mem_used_percent",
            "rename": "MEM_USED_PERCENT",
            "unit": "Percent"
          }
        ],
        "metrics_collection_interval": 60
      },
      "disk": {
        "measurement": [
          {
            "name": "used_percent",
            "rename": "DISK_USED_PERCENT",
            "unit": "Percent"
          }
        ],
        "metrics_collection_interval": 60,
        "resources": [
          "*"
        ]
      }
    }
  }
}
EOF
    /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \
        -a fetch-config \
        -m ec2 \
        -c file:/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json \
        -s
fi

# Mark node as ready
echo "EKS node bootstrap completed successfully"
date > /var/log/eks-bootstrap-complete.log
