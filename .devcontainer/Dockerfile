# ==============================================================================
# ERLMCP DEV CONTAINER - Complete Development Environment
# ==============================================================================
# Provides OTP 28+, rebar3, erlang_ls, and all development tools.
# Designed for VS Code Dev Containers and GitHub Codespaces.
#
# Usage:
#   - VS Code: "Remote-Containers: Reopen in Container"
#   - Codespaces: Automatically used when opening in Codespaces
# ==============================================================================

ARG OTP_VERSION=28.3.1
ARG REBAR3_VERSION=3.24.0
ARG ERLANG_LS_VERSION=0.52.0

FROM erlang:${OTP_VERSION} AS base

# ==============================================================================
# System Dependencies
# ==============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    # Build tools
    build-essential \
    autoconf \
    automake \
    libtool \
    pkg-config \
    cmake \
    # Development utilities
    git \
    curl \
    wget \
    vim \
    nano \
    htop \
    tree \
    jq \
    ripgrep \
    fd-find \
    # Network debugging
    netcat-openbsd \
    iproute2 \
    iputils-ping \
    dnsutils \
    tcpdump \
    lsof \
    # Process debugging
    strace \
    gdb \
    # SSL/TLS
    ca-certificates \
    openssl \
    libssl-dev \
    # Documentation
    man-db \
    # Compression
    zip \
    unzip \
    xz-utils \
    # Shell
    zsh \
    # Locales
    locales \
    # sudo for vscode user
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Configure locales
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# ==============================================================================
# Rebar3
# ==============================================================================

ARG REBAR3_VERSION
RUN curl -fsSL https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3 \
    -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

# ==============================================================================
# Erlang LS (Language Server)
# ==============================================================================

ARG ERLANG_LS_VERSION
RUN git clone --depth 1 --branch ${ERLANG_LS_VERSION} \
    https://github.com/erlang-ls/erlang_ls.git /tmp/erlang_ls && \
    cd /tmp/erlang_ls && \
    make && \
    cp _build/default/bin/erlang_ls /usr/local/bin/ && \
    rm -rf /tmp/erlang_ls

# ==============================================================================
# Infrastructure Tools
# ==============================================================================

# Terraform
RUN curl -fsSL https://releases.hashicorp.com/terraform/1.7.0/terraform_1.7.0_linux_amd64.zip \
    -o /tmp/terraform.zip && \
    unzip /tmp/terraform.zip -d /usr/local/bin/ && \
    rm /tmp/terraform.zip

# Packer
RUN curl -fsSL https://releases.hashicorp.com/packer/1.10.0/packer_1.10.0_linux_amd64.zip \
    -o /tmp/packer.zip && \
    unzip /tmp/packer.zip -d /usr/local/bin/ && \
    rm /tmp/packer.zip

# kubectl
RUN curl -fsSL https://dl.k8s.io/release/v1.29.0/bin/linux/amd64/kubectl \
    -o /usr/local/bin/kubectl && \
    chmod +x /usr/local/bin/kubectl

# Helm
RUN curl -fsSL https://get.helm.sh/helm-v3.14.0-linux-amd64.tar.gz | \
    tar -xzf - -C /tmp && \
    mv /tmp/linux-amd64/helm /usr/local/bin/ && \
    rm -rf /tmp/linux-amd64

# ==============================================================================
# User Setup
# ==============================================================================

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME && \
    useradd --uid $USER_UID --gid $USER_GID -m $USERNAME && \
    echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME && \
    chmod 0440 /etc/sudoers.d/$USERNAME

# Create cache directories
RUN mkdir -p /home/$USERNAME/.cache/rebar3 \
             /home/$USERNAME/.cache/hex \
             /home/$USERNAME/.dialyzer_plt \
             /home/$USERNAME/.erlang_ls && \
    chown -R $USERNAME:$USERNAME /home/$USERNAME

# ==============================================================================
# Environment
# ==============================================================================

ENV HOME=/home/$USERNAME
ENV PATH="/home/$USERNAME/.local/bin:$PATH"
ENV TERM=xterm-256color
ENV ERL_AFLAGS="-kernel shell_history enabled"
ENV REBAR_CACHE_DIR=/home/$USERNAME/.cache/rebar3
ENV HEX_HOME=/home/$USERNAME/.cache/hex

WORKDIR /workspace

USER $USERNAME

# ==============================================================================
# Shell Configuration
# ==============================================================================

RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended && \
    echo 'export PATH="/home/vscode/.local/bin:$PATH"' >> ~/.zshrc && \
    echo 'export ERL_AFLAGS="-kernel shell_history enabled"' >> ~/.zshrc && \
    echo 'alias r3="rebar3"' >> ~/.zshrc && \
    echo 'alias mc="make compile"' >> ~/.zshrc && \
    echo 'alias mt="make test"' >> ~/.zshrc && \
    echo 'alias mq="make quick"' >> ~/.zshrc

# ==============================================================================
# Health Check
# ==============================================================================

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell || exit 1

CMD ["sleep", "infinity"]
