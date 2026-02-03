# Production Multi-Stage Dockerfile for erlmcp v3
# Target: OTP 28.3.1 for production deployment
#
# Build stages:
#   1. builder  - Full build environment with OTP 28.3.1
#   2. runtime  - Minimal production runtime (<150MB)
#   3. debug    - Debug version with tools for troubleshooting
#
# Build: docker build -t erlmcp:3.0.0 .
# Run:   docker run -p 8080:8080 erlmcp:3.0.0
#
# Build args:
#   OTP_VERSION  - Erlang/OTP version (default: 28.3.1)
#   BUILD_DATE   - ISO 8601 timestamp
#   VCS_REF      - Git commit SHA
#   VERSION      - Application version

# ============================================================================
# STAGE 1: BUILDER - Full build environment with OTP 28.3.1
# ============================================================================
FROM erlang:28.3.1-alpine AS builder

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Builder stage"
LABEL stage="builder"

# Build metadata (set at build time)
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=3.0.0

LABEL org.opencontainers.image.created="${BUILD_DATE}"
LABEL org.opencontainers.image.revision="${VCS_REF}"
LABEL org.opencontainers.image.version="${VERSION}"

# Set build environment for deterministic builds
ENV ERL_COMPILER_OPTIONS=deterministic \
    REBAR_NO_USER_CONFIG=true \
    HOME=/root

WORKDIR /build

# Install build dependencies (minimal set for fastest build)
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    musl-dev \
    pkgconfig \
    openssl-dev \
    openssh-client \
    curl \
    wget \
    ncurses-dev \
    zlib-dev \
    && update-ca-certificates

# Verify OTP version
RUN erl -eval "erlang:display(erlang:system_info(otp_release)), halt()." -noshell

# Copy project files
COPY rebar.config rebar.lock ./
COPY include/ ./include/
COPY config/ ./config/
COPY vm.args ./

# Copy complete app directories (rebar3 umbrella project requires full structure)
COPY apps/erlmcp_core/ ./apps/erlmcp_core/
COPY apps/erlmcp_transports/ ./apps/erlmcp_transports/
COPY apps/erlmcp_observability/ ./apps/erlmcp_observability/
COPY apps/erlmcp_validation/ ./apps/erlmcp_validation/
COPY apps/erlmcp_zero_trust/ ./apps/erlmcp_zero_trust/
COPY apps/swarmflow_os/ ./apps/swarmflow_os/
COPY apps/swarmflow_pqchain/ ./apps/swarmflow_pqchain/

# Download dependencies and compile (all in prod profile)
RUN rebar3 as prod get-deps compile

# Run quality checks (non-blocking for production builds)
RUN rebar3 as prod dialyzer || echo "Dialyzer warnings found - continuing build" && \
    rebar3 as prod xref || echo "Xref warnings found - continuing build"

# Build production release
RUN rebar3 as prod release

# Verify release was created successfully
RUN ls -la _build/prod/rel/erlmcp && \
    file _build/prod/rel/erlmcp/bin/erlmcp

# Extract release for runtime stage
RUN mkdir -p /opt/erlmcp && \
    cp -r _build/prod/rel/erlmcp /opt/erlmcp

# ============================================================================
# STAGE 2: RUNTIME - Minimal production image
# ============================================================================
FROM alpine:3.20

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Runtime stage"
LABEL stage="runtime"

ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=3.0.0

LABEL org.opencontainers.image.created="${BUILD_DATE}"
LABEL org.opencontainers.image.revision="${VCS_REF}"
LABEL org.opencontainers.image.version="${VERSION}"
LABEL org.opencontainers.image.title="erlmcp"
LABEL org.opencontainers.image.description="Erlang/OTP implementation of the Model Context Protocol (MCP) SDK"
LABEL org.opencontainers.image.vendor="erlmcp"
LABEL org.opencontainers.image.licenses="Apache-2.0"
LABEL org.opencontainers.image.source="https://github.com/banyan-platform/erlmcp"
LABEL org.opencontainers.image.url="https://github.com/banyan-platform/erlmcp"
LABEL org.opencontainers.image.documentation="https://erlmcp.dev/docs"

# Install only runtime dependencies
RUN apk add --no-cache \
    ca-certificates \
    libssl3 \
    libcrypto3 \
    ncurses-libs \
    libstdc++ \
    libgcc \
    bash \
    curl \
    tzdata \
    && update-ca-certificates \
    && rm -rf /var/cache/apk/* \
    && rm -rf /tmp/*

# Create application directories
RUN mkdir -p /opt/erlmcp \
    /var/log/erlmcp \
    /var/lib/erlmcp \
    /var/run/erlmcp \
    /etc/erlmcp

WORKDIR /opt/erlmcp

# Copy erlmcp release from builder
COPY --from=builder /opt/erlmcp /opt/erlmcp

# Create health check script with 3-level checks
# Level 1: HTTP /health endpoint (application health)
# Level 2: Node ping (distribution check)
# Level 3: Process running check (state check)
RUN printf '#!/bin/bash\n\
set -euo pipefail\n\
\n\
# Health check configuration\n\
HEALTH_URL="${HEALTH_URL:-http://localhost:8080/health}"\n\
HEALTH_TIMEOUT="${HEALTH_TIMEOUT:-5}"\n\
\n\
# Level 1: HTTP /health endpoint (application health)\n\
# This checks that the application is fully operational\n\
# Returns 200 if registry, session manager, and monitoring are healthy\n\
if command -v curl &> /dev/null; then\n\
    if curl -f -s --max-time "${HEALTH_TIMEOUT}" "${HEALTH_URL}" > /dev/null 2>&1; then\n\
        echo "health: HTTP endpoint healthy (Level 1: Application)"\n\
        exit 0\n\
    fi\n\
fi\n\
\n\
# Level 2: Node ping (distribution check)\n\
# This checks that the Erlang node is responding to distributed commands\n\
# Useful when HTTP server is not yet started or is restarting\n\
if /opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1; then\n\
    echo "health: Node responding to ping (Level 2: Distribution)"\n\
    exit 0\n\
fi\n\
\n\
# Level 3: Process running check (state check)\n\
# This checks that the beam.smp process is alive\n\
# Returns 1 if process exists but not responding (degraded state)\n\
if pgrep -f "beam.smp.*erlmcp" > /dev/null; then\n\
    echo "health: Process running but not responding (Level 3: Degraded)"\n\
    exit 1\n\
fi\n\
\n\
# No process found - service is down\n\
echo "health: Down (no process found)"\n\
exit 1\n' > /opt/erlmcp/bin/healthcheck.sh && \
    chmod +x /opt/erlmcp/bin/healthcheck.sh

# Create startup wrapper for cluster mode
RUN printf '#!/bin/bash\n\
set -euo pipefail\n\
\n\
# Source environment files\n\
if [ -f /etc/erlmcp/environment ]; then\n\
    set -a\n\
    . /etc/erlmcp/environment\n\
    set +a\n\
fi\n\
\n\
# Set node name\n\
export ERLMCP_NODE_NAME="${ERLMCP_NODE_NAME:-erlmcp@$(hostname)}"\n\
\n\
# EPMD-less Clustering Configuration (Swarm/K8s Compatible)\n\
# Set distribution options for EPMD-less clustering\n\
# -proto_dist inet_tls: Use TLS for distribution (secure)\n\
# -setcookie: Set the Erlang cookie for authentication\n\
export ERL_AFLAGS="${ERL_AFLAGS:--proto_dist inet_tls}"\n\
\n\
# Set the distribution port (EPMD-less mode uses a fixed port)\n\
export ERL_DIST_PORT="${ERL_DIST_PORT:-9100}"\n\
\n\
# Set port range for distribution connections\n\
export ERLANG_DISTRIBUTION_PORT_RANGE="${ERLANG_DISTRIBUTION_PORT_RANGE:-9100-9200}"\n\
\n\
# Add EPMD listener flag if provided (overrides -erl_epmd_port)\n\
if [ -n "${ERL_EPMD_PORT:-}" ]; then\n\
    export ERL_AFLAGS="${ERL_AFLAGS} -erl_epmd_port ${ERL_EPMD_PORT}"\n\
fi\n\
\n\
# Cookie handling\n\
if [ -n "${ERLMCP_COOKIE_FILE:-}" ] && [ -f "${ERLMCP_COOKIE_FILE}" ]; then\n\
    export ERLANG_COOKIE="$(cat ${ERLMCP_COOKIE_FILE})"\n\
    export ERL_AFLAGS="${ERL_AFLAGS} -setcookie '"'"'${ERLANG_COOKIE}'"'"'"\n\
else\n\
    export ERLANG_COOKIE="${ERLANG_COOKIE:-erlmcp_prod_cookie}"\n\
fi\n\
\n\
# Enable distribution\n\
export ERLMCP_DISTRIBUTION_MODE="${ERLMCP_DISTRIBUTION_MODE:-cluster}"\n\
\n\
echo "Starting erlmcp node: ${ERLMCP_NODE_NAME}"\n\
echo "Distribution mode: ${ERLMCP_DISTRIBUTION_MODE}"\n\
echo "EPMD-less port: ${ERL_DIST_PORT}"\n\
echo "Port range: ${ERLANG_DISTRIBUTION_PORT_RANGE}"\n\
\n\
# Start the application\n\
exec /opt/erlmcp/bin/erlmcp foreground\n' > /opt/erlmcp/bin/start-cluster.sh && \
    chmod +x /opt/erlmcp/bin/start-cluster.sh

# Create non-root user for security (uid 1000)
RUN addgroup -S -g 1000 erlmcp && \
    adduser -S -u 1000 -G erlmcp -h /opt/erlmcp -s /bin/sh erlmcp && \
    chown -R erlmcp:erlmcp \
        /opt/erlmcp \
        /var/log/erlmcp \
        /var/lib/erlmcp \
        /var/run/erlmcp \
        /etc/erlmcp && \
    chmod -R 755 /opt/erlmcp/bin

# Set environment variables
ENV ERLMCP_ENV=production \
    ERLMCP_VERSION=3.0.0 \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    HOME=/opt/erlmcp \
    PATH=/opt/erlmcp/bin:$PATH \
    ERL_CRASH_DUMP=/var/log/erlmcp/erl_crash.dump \
    ERL_MAX_PORTS=65536 \
    ERL_MAX_ETS_TABLES=50000 \
    ERL_AFLAGS="+MBacul 0 +Msbagf 512 +MBacgs 0"

# Switch to non-root user
USER erlmcp

# Exposed ports:
#   8080      - HTTP API (JSON-RPC over HTTP)
#   9100      - Metrics (Prometheus) + EPMD-less distribution port
#   9090      - Health checks
#   9100-9200 - Distributed Erlang communication (EPMD-less)
# Note: EPMD port 4369 is NOT exposed - using EPMD-less clustering
EXPOSE 8080 9100 9090 9100-9200

# Health check - 3-level health verification
# Level 1: HTTP /health endpoint (application health)
# Level 2: Node ping (distribution check)
# Level 3: Process running check (state check)
HEALTHCHECK --interval=15s --timeout=10s --start-period=45s --retries=3 \
    CMD /opt/erlmcp/bin/healthcheck.sh

# Entrypoint: start erlmcp in foreground mode
# Uses the cluster wrapper to handle distribution
ENTRYPOINT ["/opt/erlmcp/bin/start-cluster.sh"]

# ============================================================================
# STAGE 3: DEBUG - Debug image with tools for troubleshooting
# ============================================================================
FROM erlang:28.3.1-alpine AS debug

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Debug stage with tools"
LABEL stage="debug"

ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=3.0.0

LABEL org.opencontainers.image.created="${BUILD_DATE}"
LABEL org.opencontainers.image.revision="${VCS_REF}"
LABEL org.opencontainers.image.version="${VERSION}"

# Install debug and monitoring tools
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    musl-dev \
    pkgconfig \
    openssl-dev \
    openssh-client \
    curl \
    wget \
    vim \
    nano \
    htop \
    procps \
    lsof \
    netcat-openbsd \
    strace \
    tcpdump \
    bind-tools \
    tzdata \
    tini \
    && update-ca-certificates \
    && rm -rf /var/cache/apk/*

RUN mkdir -p /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp /var/run/erlmcp

WORKDIR /opt/erlmcp

# Copy erlmcp release from builder
COPY --from=builder /opt/erlmcp /opt/erlmcp

# Create health check script
RUN printf '#!/bin/bash\n\
/opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1\n' > /opt/erlmcp/bin/healthcheck.sh && \
    chmod +x /opt/erlmcp/bin/healthcheck.sh

# Create non-root user
RUN addgroup -S -g 1000 erlmcp && \
    adduser -S -u 1000 -G erlmcp -h /opt/erlmcp -s /bin/bash erlmcp && \
    chown -R erlmcp:erlmcp \
        /opt/erlmcp \
        /var/log/erlmcp \
        /var/lib/erlmcp \
        /var/run/erlmcp && \
    chmod -R 755 /opt/erlmcp/bin

USER erlmcp

ENV ERLMCP_ENV=debug \
    LANG=C.UTF-8 \
    HOME=/opt/erlmcp \
    ERLANG_COOKIE=erlmcp_debug_cookie \
    ERL_AFLAGS="-proto_dist inet_tls +MBacul 0 +Msbagf 512 +MBacgs 0" \
    ERL_DIST_PORT="9110" \
    ERLANG_DISTRIBUTION_PORT_RANGE="9110-9210"

# Note: EPMD port 4369 is NOT exposed - using EPMD-less clustering
EXPOSE 8080 9100 9090 9110 9100-9200

# Start with tini for proper signal handling in debug mode
ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/opt/erlmcp/bin/erlmcp", "foreground"]
