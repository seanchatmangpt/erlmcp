# Installation Guide

Get erlmcp up and running in minutes. This guide covers installation, setup, and first-time configuration.

## Prerequisites

### System Requirements
- **Erlang/OTP**: 25.0 or higher
- **rebar3**: Latest version (3.20+ recommended)
- **Memory**: Minimum 512MB RAM (1GB+ recommended)
- **Network**: TCP/HTTP access as needed

### Check Requirements
```bash
# Check Erlang version
erl -version
# Should show: Erlang/OTP 25 [erts-13.2] ...

# Check rebar3 version
rebar3 version
# Should show rebar3 3.20.0 or higher
```

## Installation Methods

### Method 1: From Source (Recommended)

#### Clone the Repository
```bash
# Clone erlmcp
git clone https://github.com/your-org/erlmcp.git
cd erlmcp

# Or for GCP integration branch
git clone -b integration/phase1-gcp-ggen https://github.com/your-org/erlmcp.git
cd erlmcp
```

#### Build Dependencies
```bash
# Install rebar3 if needed
./rebar3 local install

# Fetch dependencies
rebar3 deps
```

#### Compile the Project
```bash
# Compile all applications
rebar3 compile

# Run tests to verify installation
rebar3 eunit
```

### Method 2: Using rebar3 Mix

#### Add to Existing Project
```bash
# Add erlmcp to your project deps
echo '{deps, [{erlmcp, "0.6.0"}]}' >> rebar.config

# Fetch and compile
rebar3 compile
```

#### Hex.pm Installation (Future)
```erlang
{deps, [
    {erlmcp, "0.6.0"}
]}.
```

### Method 3: Docker Installation

#### Quick Start with Docker
```bash
# Build the Docker image
docker build -t erlmcp .

# Run the server
docker run -p 8080:8080 erlmcp
```

#### Docker Compose Setup
```yaml
# docker-compose.yml
version: '3.8'
services:
  erlmcp:
    build: .
    ports:
      - "8080:8080"
    environment:
      - ERLMCP_TRANSPORT=tcp
      - ERLMCP_PORT=8080
      - ERLMCP_LOG_LEVEL=info
```

## Configuration

### Basic Configuration

Create a configuration file `config/sys.config`:

```erlang
% sys.config
[
    {erlmcp,
        [
            % Transport configuration
            {transport, tcp},
            {port, 8080},
            {host, "0.0.0.0"},

            % Server configuration
            {server, [
                {tools, [
                    % Built-in tools
                    calculator,
                    weather,
                    file_system
                ]}
            ]},

            % Client configuration
            {client, [
                {timeout, 5000},
                {reconnect_interval, 1000}
            ]},

            % Logging
            {log_level, info},
            {log_dir, "/var/log/erlmcp"},

            % Monitoring
            {metrics_enabled, true},
            {metrics_port, 9090}
        ]
    }
].
```

### Environment Variables

Set configuration via environment variables:

```bash
# TCP transport
export ERLMCP_TRANSPORT=tcp
export ERLMCP_PORT=8080
export ERLMCP_HOST=0.0.0.0

# Server settings
export ERLMCP_SERVER_TOOLS=calculator,weather
export ERLMCP_SERVER_TIMEOUT=5000

# Logging
export ERLMCP_LOG_LEVEL=info
export ERLMCP_LOG_DIR=/var/log/erlmcp

# Monitoring
export ERLMCP_METRICS_ENABLED=true
export ERLMCP_METRICS_PORT=9090
```

## Initial Setup

### 1. Start the Application
```bash
# Start the erlmcp application
rebar3 shell

# Or run as a service
rebar3 release
```

### 2. Verify Installation
```erlang
% In the Erlang shell
application:ensure_all_started(erlmcp).

% Check processes
application:which_processes(erlmcp).
```

### 3. Test Basic Functionality
```erlang
% Test the server is running
erlmcp_server:tools().

% Should return list of available tools
```

## Transport Configuration

### TCP Transport
```erlang
% tcp transport configuration
[
    {erlmcp,
        [
            {transport, tcp},
            {port, 8080},
            {socket_opts, [
                {backlog, 128},
                {nodelay, true},
                {reuseaddr, true}
            ]}
        ]
    }
].
```

### HTTP Transport
```erlang
% http transport configuration
[
    {erlmcp,
        [
            {transport, http},
            {port, 8081},
            {ssl, false},
            {max_connections, 1000},
            {timeout, 30000}
        ]
    }
].
```

### stdio Transport
```erlang
% stdio transport configuration
[
    {erlmcp,
        [
            {transport, stdio},
            {encoding, utf8},
            {mode, binary}
        ]
    }
].
```

## Security Configuration

### Basic Authentication
```erlang
[
    {erlmcp,
        [
            {auth_method, basic},
            {auth_credentials, [
                {"user1", "password1"},
                {"user2", "password2"}
            ]}
        ]
    }
].
```

### JWT Authentication
```erlang
[
    {erlmcp,
        [
            {auth_method, jwt},
            {jwt_secret, "your-secret-key"},
            {jwt_issuer, "erlmcp"},
            {jwt_algorithm, hs256}
        ]
    }
].
```

### SSL/TLS Configuration
```erlang
[
    {erlmcp,
        [
            {transport, tcp},
            {ssl, true},
            {ssl_opts, [
                {certfile, "/path/to/cert.pem"},
                {keyfile, "/path/to/key.pem"},
                {cacertfile, "/path/to/ca.pem"},
                {verify, verify_none},
                {versions, [tlsv1.2, tlsv1.3]}
            ]}
        ]
    }
].
```

## Monitoring Setup

### Prometheus Integration
```erlang
[
    {erlmcp,
        [
            {metrics_backend, prometheus},
            {metrics_port, 9090},
            {metrics_prefix, "erlmcp_"}
        ]
    }
].
```

### Logging Configuration
```erlang
[
    {logger,
        [
            {handler, default, logger_std_h,
                {formatter,
                    logger_formatter,
                    #{single_line => false,
                      template => [time, " ", level, " ", msg, "\n"]}}},
            {handler, file, logger_std_h,
                {file, "/var/log/erlmcp/erlmcp.log"},
                {formatter, logger_formatter, #{}}}
        ]
    }
].
```

## GCP Integration Setup

### GCP Configuration
```erlang
[
    {erlmcp,
        [
            {gcp, [
                {project_id, "your-gcp-project"},
                {credentials_file, "/path/to/credentials.json"},
                {region, "us-central1"},
                {services, [
                    % GCP services to integrate
                    pubsub,
                    storage,
                    compute
                ]}
            ]}
        ]
    }
].
```

### Environment Variables for GCP
```bash
export GCP_PROJECT_ID=your-gcp-project
export GCP_CREDENTIALS_FILE=/path/to/credentials.json
export GCP_REGION=us-central1
```

## Troubleshooting Common Issues

### 1. Compilation Errors
```bash
# Clean and recompile
rebar3 clean
rebar3 compile

# Check Erlang version
erl -version
```

### 2. Port Already in Use
```bash
# Find what's using the port
lsof -i :8080

# Change configuration or kill process
kill -9 <PID>
```

### 3. Dependency Issues
```bash
# Update dependencies
rebar3 update

# Rebuild dependencies
rebar3 deps compile
```

### 4. Memory Issues
```bash
# Check memory usage
recon:get_memory()

# Increase BEAM memory
export ERL_MAX_PORTS=65536
export ERL_AFLAGS="-kernel inet_dist_listen_min 9100 inet_dist_listen_max 9109"
```

## Next Steps

### Verify Installation
```bash
# Run the hello world example
./examples/hello_world.erl

# Check if server is responding
curl -X POST http://localhost:8080 -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":"test","method":"tools/list"}'
```

### Explore Further
- [Configuration](../reference/configuration/schema.md) - Full configuration options
- [Security Setup](security-implementation.md) - Secure your installation
- [Monitoring Setup](monitoring-setup.md) - Add monitoring
- [First Integration](first-integration.md) - Build your first integration

## Quick Reference

### Useful Commands
```bash
# Build
rebar3 compile

# Test
rebar3 eunit
rebar3 ct

# Run
rebar3 shell
rebar3 release

# Clean
rebar3 clean
```

### Files to Check
- `rebar.config` - Project configuration
- `config/sys.config` - Runtime configuration
- `vm.args` - VM arguments
- `start.script` - Release script

---

**Next**: [Your First Integration](first-integration.md) to build your first MCP connection.