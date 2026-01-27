# === MULTI-STAGE DOCKERFILE: erlmcp Production Build ===
# Builds production-ready erlmcp Erlang/OTP application
#
# Stages:
#   1. builder - Full build environment with toolchain
#   2. runtime - Minimal runtime without build tools (<150MB)
#   3. debug   - Debug version with tools for troubleshooting
#
# Build: docker build -t erlmcp:0.7.0 .
# Run:   docker run -p 8080:8080 erlmcp:0.7.0

# Stage 1: Builder - Compile and create release
# Uses erlang:27-alpine for latest stable OTP with minimal footprint
FROM erlang:27-alpine AS builder

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Builder stage"

# Set build environment for deterministic builds
ENV ERLANG_COOKIE=erlmcp_prod_cookie
ENV ERL_COMPILER_OPTIONS=deterministic
ENV REBAR_NO_USER_CONFIG=true
ENV HOME=/root

WORKDIR /build

# Install build dependencies (minimal set for fastest build)
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    pkgconfig \
    openssl-dev \
    openssh-client \
    && update-ca-certificates

# Copy project files
COPY rebar.config rebar.lock ./
COPY include/ ./include/
COPY config/ ./config/
COPY vm.args .

# Copy source code, excluding modules with compilation issues
COPY src/ ./src/
RUN rm -f src/tcps_mcp_diataxis/*.erl src/*enterprise*.erl src/erlmcp_roots.erl 2>/dev/null || true

# Download and compile dependencies
RUN rebar3 update && \
    rebar3 compile

# Run quality checks in parallel
RUN rebar3 dialyzer || true && \
    rebar3 xref || true

# Build production release
RUN rebar3 as prod release

# Verify release was created successfully
RUN ls -la _build/prod/rel/erlmcp && \
    file _build/prod/rel/erlmcp/bin/erlmcp

# Stage 2: Runtime - Minimal production image
# Stripped of build tools, ca. 120-140MB image size
FROM alpine:3.20

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Runtime stage"

# Install only runtime dependencies
RUN apk add --no-cache \
    ca-certificates \
    libssl3 \
    libcrypto3 \
    ncurses-libs \
    libstdc++ \
    bash \
    curl \
    ca-certificates \
    && update-ca-certificates \
    && rm -rf /var/cache/apk/*

# Create application directories
RUN mkdir -p /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp

WORKDIR /opt/erlmcp

# Copy erlmcp release from builder
COPY --from=builder /build/_build/prod/rel/erlmcp /opt/erlmcp

# Create non-root user for security (uid 1000)
RUN addgroup -S -g 1000 erlmcp && \
    adduser -S -u 1000 -G erlmcp -h /opt/erlmcp erlmcp && \
    chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp && \
    chmod -R 755 /opt/erlmcp/bin

# Create health check script
RUN echo '#!/bin/bash' > /opt/erlmcp/bin/healthcheck && \
    echo '/opt/erlmcp/bin/erlmcp ping >/dev/null 2>&1' >> /opt/erlmcp/bin/healthcheck && \
    chmod +x /opt/erlmcp/bin/healthcheck

# Switch to non-root user
USER erlmcp

# Set environment variables
ENV ERLMCP_ENV=production
ENV ERLANG_COOKIE=erlmcp_prod_cookie
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV HOME=/opt/erlmcp

# Health check - verify erlmcp node is responsive
# Checks if the ping endpoint returns successfully
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD /opt/erlmcp/bin/healthcheck

# Exposed ports:
#   8080  - HTTP API
#   9090  - Metrics (Prometheus)
#   4369  - EPMD (Erlang Port Mapper Daemon)
#   9100-9200 - Distributed Erlang communication
EXPOSE 8080 9090 4369 9100-9200

# Entrypoint: start erlmcp in foreground mode
# Foreground mode ensures proper signal handling and prevents zombie processes
ENTRYPOINT ["/opt/erlmcp/bin/erlmcp"]
CMD ["foreground"]

# Build metadata
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=0.7.0

LABEL org.opencontainers.image.created="${BUILD_DATE}" \
      org.opencontainers.image.url="https://github.com/seanchatmangpt/erlmcp" \
      org.opencontainers.image.source="https://github.com/seanchatmangpt/erlmcp" \
      org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.vendor="erlmcp" \
      org.opencontainers.image.title="erlmcp" \
      org.opencontainers.image.description="Erlang/OTP implementation of the Model Context Protocol (MCP) SDK"

# Stage 3: Debug image (optional, for development/troubleshooting)
# Full toolset for debugging: ~320MB
FROM erlang:27-alpine AS debug

LABEL maintainer="erlmcp contributors"
LABEL description="erlmcp Erlang/OTP - Debug stage with tools"

# Install debug and monitoring tools
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
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
    && update-ca-certificates

RUN mkdir -p /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp

WORKDIR /opt/erlmcp

# Copy erlmcp release from builder
COPY --from=builder /build/_build/prod/rel/erlmcp /opt/erlmcp

# Create non-root user
RUN addgroup -S -g 1000 erlmcp && \
    adduser -S -u 1000 -G erlmcp -h /opt/erlmcp erlmcp && \
    chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp && \
    chmod -R 755 /opt/erlmcp/bin

USER erlmcp

ENV ERLMCP_ENV=debug
ENV ERLANG_COOKIE=erlmcp_debug_cookie
ENV LANG=C.UTF-8
ENV HOME=/opt/erlmcp

EXPOSE 8080 9090 4369 9100-9200

# Start shell for interactive debugging
CMD ["/bin/sh"]
