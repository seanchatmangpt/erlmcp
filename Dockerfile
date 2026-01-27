# === MULTI-STAGE DOCKERFILE: TAIEA Production Build ===
# Stage 1: Builder - Compile and create release
# Stage 2: Runtime - Minimal runtime image
# Stage 3: Optional debug image for troubleshooting

# Stage 1: Builder
# Builds on erlang:26-alpine for minimal footprint with full build toolchain
FROM erlang:26-alpine AS builder

LABEL maintainer="TAI Autonomics <engineering@taiea.dev>"
LABEL description="TAIEA Erlang/OTP multi-agent autonomic system - Builder stage"

# Set build environment
ENV ERLANG_COOKIE=taiea_prod_cookie
ENV ERL_COMPILER_OPTIONS=deterministic
ENV REBAR_NO_USER_CONFIG=true

WORKDIR /build

# Install build dependencies
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    openssh-client \
    && update-ca-certificates

# Copy entire workspace (erlmcp + taiea)
COPY . .

# Build erlmcp workspace
RUN rebar3 clean && \
    rebar3 compile && \
    rebar3 as prod release && \
    rebar3 dialyzer || true && \
    rebar3 xref || true

# Build TAIEA application (separate umbrella)
WORKDIR /build/taiea

RUN rebar3 clean && \
    rebar3 compile && \
    rebar3 as prod release

# Verify releases were created
RUN ls -la /build/_build/prod/rel/ && \
    ls -la /build/taiea/_build/prod/rel/

# Stage 2: Runtime
# Minimal alpine runtime without build tools
FROM alpine:3.19

LABEL maintainer="TAI Autonomics <engineering@taiea.dev>"
LABEL description="TAIEA Erlang/OTP multi-agent autonomic system - Runtime stage"

# Install runtime dependencies only
RUN apk add --no-cache \
    ca-certificates \
    libssl3 \
    libcrypto3 \
    ncurses-libs \
    libstdc++ \
    && update-ca-certificates

# Create application directory
WORKDIR /opt/taiea

# Copy TAIEA release from builder
COPY --from=builder /build/taiea/_build/prod/rel/taiea /opt/taiea

# Create non-root user for security
RUN addgroup -S taiea && \
    adduser -S -G taiea -h /opt/taiea taiea && \
    chown -R taiea:taiea /opt/taiea

# Switch to non-root user
USER taiea

# Set environment variables
ENV TAIEA_ENV=production
ENV ERLANG_COOKIE=taiea_prod_cookie
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Health check - verify TAIEA node is responsive
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD /opt/taiea/bin/taiea ping || exit 1

# Default port for TAIEA (configurable via environment)
EXPOSE 8080 9100

# Entrypoint: start TAIEA in foreground mode
ENTRYPOINT ["/opt/taiea/bin/taiea"]
CMD ["foreground"]

# Build metadata
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=1.0.0

LABEL org.opencontainers.image.created="${BUILD_DATE}" \
      org.opencontainers.image.url="https://github.com/seanchatmangpt/erlmcp" \
      org.opencontainers.image.source="https://github.com/seanchatmangpt/erlmcp" \
      org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.vendor="TAI Autonomics" \
      org.opencontainers.image.title="TAIEA" \
      org.opencontainers.image.description="TAIEA Erlang/OTP multi-agent autonomic system"

# Stage 3: Debug image (optional, for development/troubleshooting)
FROM erlang:26-alpine AS debug

LABEL maintainer="TAI Autonomics <engineering@taiea.dev>"
LABEL description="TAIEA Erlang/OTP multi-agent autonomic system - Debug stage with tools"

# Install debug tools
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    openssh-client \
    curl \
    wget \
    vim \
    nano \
    htop \
    procps \
    lsof \
    netcat-openbsd \
    && update-ca-certificates

WORKDIR /opt/taiea

# Copy TAIEA release from builder
COPY --from=builder /build/taiea/_build/prod/rel/taiea /opt/taiea

# Create non-root user
RUN addgroup -S taiea && \
    adduser -S -G taiea -h /opt/taiea taiea && \
    chown -R taiea:taiea /opt/taiea

USER taiea

ENV TAIEA_ENV=debug
ENV ERLANG_COOKIE=taiea_debug_cookie
ENV LANG=C.UTF-8

EXPOSE 8080 9100 4369 9200

# Use remsh for remote debugging
CMD ["/bin/sh"]
