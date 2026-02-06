#!/bin/bash
# ============================================================================
# erlmcp v3 GCE Startup Script
# Initializes Erlang/OTP environment and starts erlmcp server
# ============================================================================

set -euo pipefail

# Logging setup
exec 1> >(logger -s -t erlmcp-startup) 2>&1

echo "Starting erlmcp initialization..."

# ============================================================================
# Environment Configuration
# ============================================================================
export ERLMCP_ENV="${environment}"
export DEBIAN_FRONTEND=noninteractive

# ============================================================================
# Install Dependencies
# ============================================================================
echo "Installing system dependencies..."
apt-get update -qq
apt-get install -y -qq \
  curl \
  wget \
  gnupg \
  ca-certificates \
  apt-transport-https \
  software-properties-common \
  docker.io \
  docker-compose

# ============================================================================
# Configure Docker
# ============================================================================
echo "Configuring Docker..."
systemctl enable docker
systemctl start docker
usermod -aG docker ubuntu || true

# ============================================================================
# Pull erlmcp Docker Image
# ============================================================================
echo "Configuring Docker authentication for Artifact Registry..."
gcloud auth configure-docker ${region}-docker.pkg.dev --quiet

echo "Pulling erlmcp image..."
docker pull ${region}-docker.pkg.dev/${project_id}/erlmcp/erlmcp-server:${image_tag}

# ============================================================================
# Retrieve Secrets from Secret Manager
# ============================================================================
echo "Retrieving secrets from Secret Manager..."

ERLANG_COOKIE=$(gcloud secrets versions access latest --secret="erlmcp-erlang-cookie" --project="${project_id}")
DB_PASSWORD=$(gcloud secrets versions access latest --secret="erlmcp-db-password" --project="${project_id}")
REDIS_PASSWORD=$(gcloud secrets versions access latest --secret="erlmcp-redis-password" --project="${project_id}")

# ============================================================================
# Create erlmcp Configuration
# ============================================================================
echo "Creating erlmcp configuration..."
mkdir -p /etc/erlmcp

cat > /etc/erlmcp/config.yml <<EOF
environment: ${environment}
transport: stdio
log_level: info
metrics:
  enabled: true
  port: 9090
tracing:
  enabled: true
  sampling_rate: 0.1
EOF

# ============================================================================
# Create systemd Service
# ============================================================================
echo "Creating systemd service..."
cat > /etc/systemd/system/erlmcp.service <<EOF
[Unit]
Description=erlmcp v3 MCP Server
After=docker.service
Requires=docker.service

[Service]
Type=simple
Restart=always
RestartSec=10
Environment="ERLANG_COOKIE=$ERLANG_COOKIE"
Environment="DB_PASSWORD=$DB_PASSWORD"
Environment="REDIS_PASSWORD=$REDIS_PASSWORD"
Environment="ERLMCP_ENV=${environment}"
Environment="ERL_AFLAGS=-proto_dist inet_tls"
Environment="ERLMCP_CONFIG=/etc/erlmcp/config.yml"

ExecStartPre=-/usr/bin/docker stop erlmcp
ExecStartPre=-/usr/bin/docker rm erlmcp
ExecStart=/usr/bin/docker run --name erlmcp \\
  --network host \\
  --restart unless-stopped \\
  -e ERLANG_COOKIE \\
  -e DB_PASSWORD \\
  -e REDIS_PASSWORD \\
  -e ERLMCP_ENV \\
  -e ERL_AFLAGS \\
  -v /etc/erlmcp:/etc/erlmcp:ro \\
  -p 8080:8080 \\
  -p 9090:9090 \\
  ${region}-docker.pkg.dev/${project_id}/erlmcp/erlmcp-server:${image_tag}

ExecStop=/usr/bin/docker stop erlmcp

[Install]
WantedBy=multi-user.target
EOF

# ============================================================================
# Configure Monitoring Agent
# ============================================================================
echo "Installing Cloud Monitoring agent..."
curl -sSO https://dl.google.com/cloudagents/add-google-cloud-ops-agent-repo.sh
bash add-google-cloud-ops-agent-repo.sh --also-install

cat > /etc/google-cloud-ops-agent/config.yaml <<EOF
logging:
  receivers:
    syslog:
      type: files
      include_paths:
        - /var/log/messages
        - /var/log/syslog
    erlmcp:
      type: files
      include_paths:
        - /var/log/erlmcp/*.log
  service:
    pipelines:
      default_pipeline:
        receivers: [syslog, erlmcp]

metrics:
  receivers:
    hostmetrics:
      type: hostmetrics
      collection_interval: 60s
    erlmcp_prometheus:
      type: prometheus
      config:
        scrape_configs:
          - job_name: 'erlmcp'
            scrape_interval: 30s
            static_configs:
              - targets: ['localhost:9090']
  service:
    pipelines:
      default_pipeline:
        receivers: [hostmetrics, erlmcp_prometheus]
EOF

systemctl restart google-cloud-ops-agent

# ============================================================================
# Start erlmcp Service
# ============================================================================
echo "Starting erlmcp service..."
systemctl daemon-reload
systemctl enable erlmcp
systemctl start erlmcp

# ============================================================================
# Health Check
# ============================================================================
echo "Waiting for erlmcp to be healthy..."
for i in {1..30}; do
  if curl -sf http://localhost:8080/health > /dev/null 2>&1; then
    echo "erlmcp is healthy!"
    break
  fi
  echo "Waiting for erlmcp... ($i/30)"
  sleep 5
done

# ============================================================================
# Completion
# ============================================================================
echo "erlmcp startup complete!"
echo "Service status:"
systemctl status erlmcp --no-pager || true

echo "Container status:"
docker ps | grep erlmcp || true

echo "Health check:"
curl -s http://localhost:8080/health || echo "Health check failed"
