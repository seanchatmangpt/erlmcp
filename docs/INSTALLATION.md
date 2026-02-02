# erlmcp Installation Guide

**Version**: 3.0.0 | **OTP Requirement**: Erlang/OTP 28.3.1+ | **MCP Spec**: 2025-11-25

## Table of Contents

- [Requirements](#requirements)
- [Installation Methods](#installation-methods)
  - [Custom OTP Installation](#custom-otp-installation-recommended)
  - [kerl Installation](#kerl-installation-version-manager)
  - [System Package Manager](#system-package-manager)
  - [Docker Installation](#docker-installation)
- [Verification](#verification)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)
- [Cross-Version Distribution](#cross-version-distribution)

## Requirements

### System Requirements

| Component | Minimum | Recommended |
|------------|----------|-------------|
| **Erlang/OTP** | 28.3.1 | 28.3.1 or later |
| **ERTS** | 16.0.3 | 16.0.3 or later |
| **Kernel** | 10.4 | 10.4 or later |
| **STDLIB** | 6.0 | 6.0 or later |
| **Crypto** | 5.3 | 5.3 or later |
| **SSL** | 11.0 | 11.0 or later |
| **Memory** | 512 MB | 2 GB+ |
| **Disk** | 100 MB | 500 MB+ |
| **CPU** | 1 core | 2+ cores |

### Operating Systems

- **Linux**: Ubuntu 20.04+, Debian 11+, RHEL 8+, Arch Linux
- **macOS**: 11+ (Big Sur) or later
- **Windows**: WSL2 with Ubuntu 20.04+ (native Windows not tested)

### Build Tools

- **rebar3**: 3.20.0 or later
- **git**: 2.0+ (for cloning repository)
- **make**: GNU make 4.0+ (optional, for Makefile targets)
- **curl** or **wget**: For downloading dependencies

## Installation Methods

### Custom OTP Installation (Recommended)

This method installs OTP 28.3.1 to a custom location, avoiding conflicts with system-wide Erlang installations.

#### Linux/macOS

```bash
# 1. Download OTP 28.3.1 source
cd /tmp
wget https://github.com/erlang/otp/releases/download/OTP-28.3.1/otp_src_28.3.1.tar.gz
tar -xzf otp_src_28.3.1.tar.gz
cd otp_src_28.3.1

# 2. Configure with custom installation prefix
./configure --prefix=/opt/erlang/otp-28.3.1 \
            --with-ssl \
            --enable-wx \
            --enable-debug \
            --enable-dynamic-ssl-lib

# 3. Compile and install
make -j$(nproc)
sudo make install

# 4. Update PATH
export ERLMCP_OTP_BIN="/opt/erlang/otp-28.3.1/bin"
export PATH="$ERLMCP_OTP_BIN:$PATH"

# 5. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Should output: "28"

# 6. Add to .bashrc or .zshrc for persistence
echo 'export ERLMCP_OTP_BIN="/opt/erlang/otp-28.3.1/bin"' >> ~/.bashrc
echo 'export PATH="$ERLMCP_OTP_BIN:$PATH"' >> ~/.bashrc
```

#### For erlmcp Development (Custom Path)

The erlmcp project uses a custom OTP path at `/Users/sac/.erlmcp/otp-28.3.1/`:

```bash
# Set custom OTP path for erlmcp development
export ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"
export PATH="$ERLMCP_OTP_BIN:$PATH"

# Verify
erl -version
# Should show: Erlang/OTP 28 [erts-16.0.3]
```

### kerl Installation (Version Manager)

[kerl](https://github.com/kerl/kerl) is an Erlang/OTP version manager that simplifies installation and switching between versions.

#### Installing kerl

```bash
# 1. Clone kerl
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
sudo mv kerl /usr/local/bin/

# 2. Update available releases
kerl update releases

# 3. Install OTP 28.3.1
kerl install 28.3.1

# 4. Activate OTP 28.3.1
kerl activate 28.3.1

# 5. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
```

#### Using kerl with erlmcp

```bash
# Activate OTP 28.3.1 for erlmcp development
kerl activate 28.3.1

# Clone and build erlmcp
cd ~/projects
git clone https://github.com/yourusername/erlmcp.git
cd erlmcp
rebar3 compile

# Set OTP 28.3.1 as default for this project
kerl use 28.3.1
```

### System Package Manager

#### Ubuntu/Debian

```bash
# 1. Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
rm erlang-solutions_1.0_all.deb

# 2. Update package list
sudo apt-get update

# 3. Install Erlang/OTP 28+ packages
sudo apt-get install -y \
    erlang-asn1 \
    erlang-base \
    erlang-crypto \
    erlang-eldap \
    erlang-ftp \
    erlang-inets \
    erlang-mnesia \
    erlang-os-mon \
    erlang-parsetools \
    erlang-public-key \
    erlang-runtime-tools \
    erlang-snmp \
    erlang-ssl \
    erlang-syntax-tools \
    erlang-tftp \
    erlang-tools \
    erlang-xmerl \
    erlang-dev

# 4. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Should output: "28"
```

#### macOS (Homebrew)

```bash
# 1. Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 2. Install Erlang/OTP
brew install erlang@28

# 3. Link Erlang 28
brew link erlang@28 --force

# 4. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Should output: "28"
```

#### Fedora/RHEL

```bash
# 1. Enable EPEL repository (if needed)
sudo dnf install epel-release

# 2. Install Erlang/OTP 28+
sudo dnf install -y erlang

# 3. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
```

#### Arch Linux

```bash
# 1. Install Erlang/OTP
sudo pacman -S erlang

# 2. Verify installation
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
```

### Docker Installation

#### Using Official Erlang Docker Image

```dockerfile
# Dockerfile for erlmcp development
FROM erlang:28.3.1

# Install rebar3
RUN curl -O https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Set working directory
WORKDIR /app

# Copy project files
COPY rebar.config .
COPY apps ./apps

# Compile
RUN rebar3 compile

# Default command
CMD ["rebar3", "shell"]
```

#### Building and Running

```bash
# Build Docker image
docker build -t erlmcp:3.0.0 .

# Run container
docker run -it --rm \
  -v $(pwd):/app \
  -p 8080:8080 \
  erlmcp:3.0.0

# Or use docker-compose
docker-compose up -d
```

## Verification

### Verify Erlang Version

```bash
# Check Erlang/OTP version
erl -version
# Expected output: Erlang/OTP 28 [erts-16.0.3]

# Check specific OTP release number
erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Expected output: "28"

# Check ERTS version
erl -noshell -eval 'erlang:display(erlang:system_info(version)), init:stop().'
# Expected output: "8.0.3" (or later)
```

### Verify Required Applications

```bash
# Start Erlang shell and verify required applications
erl

% In the Erlang shell
application:start(kernel).
application:start(stdlib).
application:start(crypto).
application:start(public_key).
application:start(ssl).

% Check SSL version (should be 11.0 or later)
application:load(ssl).
ssl:version().
% Should show: ["11.0", ...]

% Exit
q().
```

### Verify Native JSON Module

```erlang
% In the Erlang shell
% Verify native json module is available
json:encode(#{test => "value"}).
% Should return: <<"{\"test\":\"value\"}">>

json:decode(<<"{\"test\":\"value\"}">>).
% Should return: {ok, #{<<"test">> => <<"value">>}}
```

### Compile and Test erlmcp

```bash
# Clone erlmcp repository
git clone https://github.com/yourusername/erlmcp.git
cd erlmcp

# Compile
rebar3 compile

# Run unit tests
rebar3 eunit

# Run Common Tests
rebar3 ct

# All quality gates
make check
```

## Configuration

### Environment Variables

```bash
# Set custom OTP bin path (if using custom installation)
export ERLMCP_OTP_BIN="/path/to/otp-28.3.1/bin"
export PATH="$ERLMCP_OTP_BIN:$PATH"

# Set erlmcp profile (dev, test, staging, prod)
export ERLMCP_PROFILE=dev

# Enable cloud-specific optimizations
export CLAUDE_CODE_REMOTE=true  # Set when running in Claude Code web
```

### rebar.config Configuration

The `rebar.config` file specifies the minimum OTP version:

```erlang
%% Minimum OTP version - STRICT: Only OTP 28+ allowed
{minimum_otp_vsn, "28"}.

%% Compiler options for OTP 28+
{erl_opts, [
    debug_info,
    {i, "apps/erlmcp_core/include"},
    {platform_define, "^2[8-9]|^[3-9]", 'OTP_MODERN'},  % OTP 28+
    {warn_missing_spec_all, false}
]}.
```

### Application Configuration

#### config/sys.config

```erlang
[
{erlmcp_core, [
    {client_defaults, #{
        timeout => 5000,
        strict_mode => false,
        max_pending_requests => 100
    }},

    %% Session backend with OTP 28 hibernation support
    {session_backend, erlmcp_session_ets},
    {session_hibernation, #{
        hibernate_after => 60000,      % Hibernate after 60s idle
        hibernate_memory_only => true,
        wake_timeout => 30000
    }},

    %% Registry configuration
    {registry_defaults, #{
        health_check_interval => 30000
    }}
]},

{erlmcp_transports, [
    {enabled_transports, [stdio, http, sse]},
    {http_port, 8080},
    {sse_keepalive_interval, 30000}
]},

{erlmcp_observability, [
    {otel_enabled, true},
    {metrics_interval, 60000}
]}
].
```

## Troubleshooting

### Common Installation Issues

#### Issue: "OTP version too old"

**Symptom**: Compilation fails with error about minimum OTP version

**Solution**:
```bash
# Check current version
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'

# If not 28+, install correct version:
# Using kerl:
kerl install 28.3.1
kerl activate 28.3.1

# Or using custom installation:
# Follow "Custom OTP Installation" section above
```

#### Issue: "Native json module not found"

**Symptom**: Runtime error about `json` module not found

**Solution**:
```bash
# This error indicates OTP < 27 was detected
# Ensure OTP 28.3.1+ is installed and active

# Verify:
erl -noshell -eval 'code:ensure_loaded(json), io:format("~p~n", [json:version()]), init:stop().'
# Should show version info

# If fails, reinstall OTP 28.3.1
```

#### Issue: "SSL 11.0 not available"

**Symptom**: Runtime error about SSL version

**Solution**:
```bash
# Check SSL version
erl -noshell -eval 'application:ensure_loaded(ssl), {ok, Vsn} = application:get_key(ssl, version), io:format("~p~n", [Vsn]), init:stop().'
# Should show: "11.0" or higher

# If lower, install OTP 28.3.1 (which includes SSL 11.0)
```

#### Issue: "jsx module not found"

**Symptom**: Compilation error about missing jsx

**Solution**:
```bash
# erlmcp v3.0 uses native JSON, not jsx
# Remove jsx dependency if you have it

# In rebar.config, ensure jsx is NOT in deps list
# The native json module is part of OTP 27+ stdlib

# If you added jsx manually, remove it:
# {deps, [
%   {jsx, "3.1.0"},  <-- REMOVE THIS LINE
#   ...
# ]}.
```

#### Issue: "Permission denied when installing"

**Symptom**: Installation fails due to permissions

**Solution**:
```bash
# Use sudo for system-wide installation
sudo make install

# Or install to user directory
./configure --prefix=$HOME/.local
make install
export PATH="$HOME/.local/bin:$PATH"
```

### Platform-Specific Issues

#### macOS: "Command Line Tools Required"

**Symptom**: Compilation fails with errors about xcode-select

**Solution**:
```bash
# Install Xcode Command Line Tools
xcode-select --install

# Accept license and install
```

#### Windows: "WSL2 Required"

**Symptom**: Native Windows not supported

**Solution**:
```bash
# Install WSL2 with Ubuntu 20.04+
# Follow Microsoft's official guide:
# https://docs.microsoft.com/en-us/windows/wsl/install

# Then follow Linux installation instructions
```

#### Linux: "Dev Tools Missing"

**Symptom**: Compilation fails with missing build tools

**Solution**:
```bash
# Ubuntu/Debian:
sudo apt-get install -y build-essential autoconf libncurses5-dev \
    libssl-dev libwxgtk3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev \
    libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils

# RHEL/CentOS:
sudo dnf groupinstall "Development Tools"
sudo dnf install -y ncurses-devel openssl-devel wxGTK3-devel \
    mesa-libGL-devel mesa-libGLU-devel libpng-devel libssh-devel \
    unixODBC-devel libxslt libxml2
```

## Cross-Version Distribution

OTP 28.3.1 supports cross-version distribution, allowing nodes running different OTP versions to communicate.

### Setting Up Multi-Version Cluster

```bash
# Node 1: OTP 28.3.1
erl -sname node1@127.0.0.1 -setcookie erlmcp_cluster -mnesia extra_db_nodes "['node2@127.0.0.1']"

# Node 2: OTP 27.x (for compatibility testing)
kerl activate 27
erl -sname node2@127.0.0.1 -setcookie erlmcp_cluster -mnesia extra_db_nodes "['node1@127.0.0.1']"
```

### Version Compatibility Matrix

| Feature | OTP 26 | OTP 27 | OTP 28 | Notes |
|---------|--------|--------|--------|-------|
| Message passing | ✅ | ✅ | ✅ | Fully compatible |
| RPC calls | ✅ | ✅ | ✅ | Fully compatible |
| Mnesia replication | ✅ | ✅ | ✅ | Fully compatible |
| Native JSON | ❌ | ✅ | ✅ | OTP 27+ |
| Priority messages | ❌ | ❌ | ✅ | OTP 28 only |
| Process hibernation | ❌ | ❌ | ✅ | OTP 28 only |
| Process iterator | ❌ | ❌ | ✅ | OTP 28 only |
| MLKEM hybrid TLS | ❌ | ❌ | ✅ | OTP 28.3+ only |

### Distribution Configuration

```erlang
% config/sys.config
{erlmcp_core, [
  {cluster_enabled, true},
  {cluster_nodes, [
    'erlmcp1@host1.example.com',
    'erlmcp2@host2.example.com',
    'erlmcp3@host3.example.com'
  ]},
  {cluster_cookie, erlmcp_cluster},
  {cluster_heartbeat_interval, 10000},
  {split_brain_strategy, winner_takes_all}
]}.
```

## Next Steps

After installation:

1. **Clone and Build**:
   ```bash
   git clone https://github.com/yourusername/erlmcp.git
   cd erlmcp
   rebar3 compile
   ```

2. **Run Tests**:
   ```bash
   rebar3 eunit
   rebar3 ct
   make check
   ```

3. **Start Development Shell**:
   ```bash
   rebar3 shell
   # Or:
   make console
   ```

4. **Read Documentation**:
   - [README.md](../README.md) - Quick start guide
   - [CLI_REFERENCE.md](CLI_REFERENCE.md) - CLI commands
   - [OTP28_MIGRATION_USER_GUIDE.md](OTP28_MIGRATION_USER_GUIDE.md) - Migration details

## Additional Resources

- [Erlang/OTP 28.0 Release Notes](https://github.com/erlang/otp/releases/tag/OTP-28.0)
- [Erlang/OTP 28.3 Release Notes](https://github.com/erlang/otp/releases/tag/OTP-28.3)
- [OTP 28 Highlights Blog](https://www.erlang.org/blog/highlights-otp-28/)
- [kerl Documentation](https://github.com/kerl/kerl)
- [rebar3 Documentation](https://rebar3.org/)

## Support

For installation issues:

- Check [Troubleshooting](#troubleshooting) section above
- Review [GitHub Issues](https://github.com/yourusername/erlmcp/issues)
- Consult [OTP 28.3.1 Patch Page](https://www.erlang.org/patches/otp-28.3)
