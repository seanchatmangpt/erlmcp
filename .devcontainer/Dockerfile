# ============================================================================
# erlmcp Dev Container Dockerfile
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Provides a complete development environment for erlmcp with:
#   - OTP 28 (configurable via ARG)
#   - rebar3 with hex packages
#   - Development tools (dialyzer, elvis, proper)
#   - Docker CLI for receipt bundle generation
#   - Git, GitHub CLI for workflow integration
#
# Build:
#   docker build -f .devcontainer/Dockerfile -t erlmcp-dev .
# ============================================================================

ARG OTP_VERSION=28.3.1
ARG VARIANT=alpine

# ============================================================================
# Base Image Selection
# ============================================================================
FROM erlang:${OTP_VERSION}-${VARIANT} AS base

# Labels for provenance
LABEL org.opencontainers.image.title="erlmcp-dev"
LABEL org.opencontainers.image.description="Development container for erlmcp"
LABEL org.opencontainers.image.source="https://github.com/seanchatmangpt/erlmcp"
LABEL erlmcp.constitution="docker-only"

# ============================================================================
# System Dependencies
# ============================================================================
RUN apk add --no-cache \
    # Build essentials
    build-base \
    autoconf \
    automake \
    libtool \
    # Git and version control
    git \
    git-lfs \
    openssh-client \
    # Shell utilities
    bash \
    bash-completion \
    coreutils \
    findutils \
    grep \
    sed \
    gawk \
    # Network tools
    curl \
    wget \
    jq \
    # Process management
    procps \
    htop \
    # Docker CLI (for receipt generation)
    docker-cli \
    docker-cli-compose \
    # Security
    ca-certificates \
    gnupg \
    # Editors (minimal)
    vim \
    nano \
    # Documentation
    man-pages \
    mandoc \
    # Timezone data
    tzdata

# ============================================================================
# GitHub CLI Installation
# ============================================================================
RUN wget -qO- https://github.com/cli/cli/releases/download/v2.45.0/gh_2.45.0_linux_amd64.tar.gz | \
    tar xz -C /usr/local --strip-components=1

# ============================================================================
# Rebar3 Setup
# ============================================================================
ENV REBAR3_VERSION=3.24.0

RUN mkdir -p /root/.cache/rebar3 && \
    wget -q "https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" -O /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3 && \
    rebar3 local install

ENV PATH="/root/.cache/rebar3/bin:${PATH}"

# ============================================================================
# Hex Package Manager
# ============================================================================
RUN rebar3 local upgrade && \
    mix local.hex --force 2>/dev/null || true

# ============================================================================
# Erlang LS (Language Server)
# ============================================================================
RUN git clone --depth 1 https://github.com/erlang-ls/erlang_ls.git /tmp/erlang_ls && \
    cd /tmp/erlang_ls && \
    make && \
    make install PREFIX=/usr/local && \
    rm -rf /tmp/erlang_ls

# ============================================================================
# Elvis (Erlang Style Reviewer)
# ============================================================================
RUN wget -q "https://github.com/inaka/elvis/releases/download/3.2.6/elvis" -O /usr/local/bin/elvis && \
    chmod +x /usr/local/bin/elvis

# ============================================================================
# Environment Configuration
# ============================================================================
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV TERM=xterm-256color
ENV COLORTERM=truecolor

# Erlang-specific
ENV ERL_AFLAGS="+pc unicode"
ENV ERL_COMPILER_OPTIONS=deterministic
ENV REBAR_NO_USER_CONFIG=true

# Mark as development container
ENV ERLMCP_ENV=development
ENV ERLMCP_PROFILE=dev
ENV DOCKER_CONTAINER=true

# ============================================================================
# Working Directory
# ============================================================================
WORKDIR /workspace

# ============================================================================
# Shell Configuration
# ============================================================================
RUN echo 'export PS1="\[\033[1;32m\]erlmcp-dev\[\033[0m\]:\[\033[1;34m\]\w\[\033[0m\]\$ "' >> /root/.bashrc && \
    echo 'alias ll="ls -la"' >> /root/.bashrc && \
    echo 'alias la="ls -A"' >> /root/.bashrc && \
    echo 'alias r3="rebar3"' >> /root/.bashrc && \
    echo 'alias compile="rebar3 compile"' >> /root/.bashrc && \
    echo 'alias test="rebar3 eunit && rebar3 ct"' >> /root/.bashrc && \
    echo 'alias dialyze="rebar3 dialyzer"' >> /root/.bashrc && \
    echo '# Source bash completion' >> /root/.bashrc && \
    echo '[ -f /etc/bash/bash_completion.sh ] && . /etc/bash/bash_completion.sh' >> /root/.bashrc

# ============================================================================
# Health Check
# ============================================================================
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD erl -noshell -eval 'io:format("ok~n"), halt(0).' || exit 1

# ============================================================================
# Default Command
# ============================================================================
CMD ["/bin/bash"]
