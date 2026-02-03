# ============================================================================
# erlmcp Production Dockerfile for GCP Marketplace
# ============================================================================
# Multi-stage build for minimal, secure production image
#
# DOCKER-ONLY CONSTITUTION: All execution via Docker
#
# Build:
#   docker build -t erlmcp:latest -f gcp/marketplace/Dockerfile .
#
# Run:
#   docker run -p 8080:8080 -p 9568:9568 erlmcp:latest
# ============================================================================

# Stage 1: Build environment
FROM erlang:28.3.1-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    make \
    gcc \
    g++ \
    libc-dev \
    openssl-dev \
    ncurses-dev \
    curl

# Set working directory
WORKDIR /build

# Copy rebar config first (for dependency caching)
COPY rebar.config rebar.lock ./

# Fetch dependencies
RUN rebar3 get-deps

# Copy source code
COPY config/ config/
COPY apps/ apps/
COPY src/ src/
COPY include/ include/

# Build release
RUN rebar3 as prod release

# Stage 2: Production image
FROM alpine:3.19 AS production

# Labels for GCP Marketplace
LABEL maintainer="seanchatmangpt" \
      org.opencontainers.image.title="erlmcp" \
      org.opencontainers.image.description="Model Context Protocol SDK for Erlang/OTP" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.vendor="seanchatmangpt" \
      org.opencontainers.image.licenses="Apache-2.0" \
      org.opencontainers.image.source="https://github.com/seanchatmangpt/erlmcp" \
      com.google.cloud.marketplace.solution="erlmcp-mcp-sdk"

# Install runtime dependencies
RUN apk add --no-cache \
    openssl \
    ncurses-libs \
    libstdc++ \
    libgcc \
    ca-certificates \
    curl \
    jq \
    tini

# Create non-root user
RUN addgroup -g 1000 erlmcp && \
    adduser -u 1000 -G erlmcp -s /bin/sh -D erlmcp

# Create directories
RUN mkdir -p /opt/erlmcp /var/log/erlmcp /var/run/erlmcp /etc/erlmcp && \
    chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/run/erlmcp /etc/erlmcp

# Copy release from builder
COPY --from=builder --chown=erlmcp:erlmcp /build/_build/prod/rel/erlmcp/ /opt/erlmcp/

# Copy configuration templates
COPY --chown=erlmcp:erlmcp gcp/marketplace/config/ /etc/erlmcp/

# Health check script
COPY --chown=erlmcp:erlmcp <<'EOF' /opt/erlmcp/bin/health-check.sh
#!/bin/sh
set -e
HEALTH_PORT=${ERLMCP_HEALTH_PORT:-8080}
response=$(curl -sf --max-time 5 http://localhost:$HEALTH_PORT/health 2>/dev/null)
if [ $? -eq 0 ] && echo "$response" | grep -q '"status":"healthy"'; then
    exit 0
else
    exit 1
fi
EOF
RUN chmod +x /opt/erlmcp/bin/health-check.sh

# Entrypoint script
COPY --chown=erlmcp:erlmcp <<'EOF' /opt/erlmcp/bin/docker-entrypoint.sh
#!/bin/sh
set -e

# Configure node name
if [ -z "$ERLMCP_NODE_NAME" ]; then
    if [ -n "$HOSTNAME" ]; then
        export ERLMCP_NODE_NAME="erlmcp@$HOSTNAME"
    else
        export ERLMCP_NODE_NAME="erlmcp@127.0.0.1"
    fi
fi

# Configure cookie from environment or Secret Manager
if [ -z "$ERLMCP_COOKIE" ] && [ -n "$ERLMCP_COOKIE_SECRET" ]; then
    if command -v gcloud > /dev/null 2>&1; then
        ERLMCP_COOKIE=$(gcloud secrets versions access latest --secret="$ERLMCP_COOKIE_SECRET" 2>/dev/null || echo "")
    fi
fi

# Default cookie if still not set
export ERLMCP_COOKIE="${ERLMCP_COOKIE:-erlmcp_default_cookie_change_me}"

# Configure ports
export ERLMCP_HTTP_PORT="${ERLMCP_HTTP_PORT:-8080}"
export ERLMCP_METRICS_PORT="${ERLMCP_METRICS_PORT:-9568}"
export ERLMCP_TCP_PORT="${ERLMCP_TCP_PORT:-9090}"

# Configure logging
export ERLMCP_LOG_LEVEL="${ERLMCP_LOG_LEVEL:-info}"

# Write vm.args
cat > /etc/erlmcp/vm.args <<VMARGS
## erlmcp VM Arguments (generated)
-name $ERLMCP_NODE_NAME
-setcookie $ERLMCP_COOKIE

## Scheduler settings
+S ${ERLMCP_SCHEDULERS:-4}:${ERLMCP_SCHEDULERS:-4}
+SDcpu 75
+sbwt very_long
+swt very_low

## Memory allocators
+MBas aobf
+MBcs 65536

## Kernel settings
+K true
+A ${ERLMCP_ASYNC_THREADS:-64}
+P 1048576
+Q 1048576

## Distribution (disabled in container mode)
-kernel inet_dist_listen_min 9100
-kernel inet_dist_listen_max 9200
VMARGS

# Log startup
echo "$(date -Iseconds) Starting erlmcp..."
echo "  Node: $ERLMCP_NODE_NAME"
echo "  HTTP Port: $ERLMCP_HTTP_PORT"
echo "  Metrics Port: $ERLMCP_METRICS_PORT"
echo "  Log Level: $ERLMCP_LOG_LEVEL"

# Execute erlmcp
exec /opt/erlmcp/bin/erlmcp foreground
EOF
RUN chmod +x /opt/erlmcp/bin/docker-entrypoint.sh

# Set environment
ENV HOME=/opt/erlmcp \
    ERLMCP_HTTP_PORT=8080 \
    ERLMCP_METRICS_PORT=9568 \
    ERLMCP_TCP_PORT=9090 \
    ERLMCP_LOG_LEVEL=info \
    ERLMCP_ENVIRONMENT=production

# Switch to non-root user
USER erlmcp
WORKDIR /opt/erlmcp

# Expose ports
EXPOSE 8080 9090 9568 4369 9100-9200

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD /opt/erlmcp/bin/health-check.sh

# Use tini as init
ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/opt/erlmcp/bin/docker-entrypoint.sh"]
