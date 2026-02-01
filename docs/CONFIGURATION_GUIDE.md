# ErlMCP Configuration Guide

## Overview

This comprehensive guide covers all configuration aspects of ErlMCP, from profile selection to advanced runtime tuning. It is designed for operators, DevOps engineers, and advanced users.

## Configuration Architecture

ErlMCP uses a **layered configuration system**:

```
Layer 1: ERLMCP_PROFILE (dev, test, staging, prod)
         ↓
Layer 2: config/sys.config.{profile} (Erlang application config)
         ↓
Layer 3: config/*.env (environment variables, secrets)
         ↓
Layer 4: Specialized configs (cluster, dashboard, monitoring)
```

## Profile Selection Decision Tree

```
┌─────────────────────────────────────┐
│ What are you trying to do?         │
└─────────────────────────────────────┘
                │
    ┌───────────┴───────────┐
    │                       │
    ▼                       ▼
┌─────────┐         ┌──────────────┐
│ Develop │         │ Test/Deploy  │
└─────────┘         └──────────────┘
    │                       │
    ▼                       │
ERLMCP_PROFILE=dev          │
(default)                   │
                ┌───────────┴───────────┐
                │                       │
                ▼                       ▼
        ┌──────────────┐        ┌─────────────┐
        │ Run tests?   │        │ Production? │
        └──────────────┘        └─────────────┘
                │                       │
                ▼                       │
        ERLMCP_PROFILE=test             │
                                ┌───────┴────────┐
                                │                │
                                ▼                ▼
                        ┌──────────────┐  ┌──────────┐
                        │ Benchmark?   │  │ Release? │
                        └──────────────┘  └──────────┘
                                │                │
                                ▼                ▼
                        ERLMCP_PROFILE=  ERLMCP_PROFILE=
                        staging          prod
```

## Per-Profile Configuration Details

### Development Profile (dev)

**File**: `config/sys.config.dev`

**Key Settings**:
```erlang
{erlmcp, [
    {log_level, debug},
    {client_defaults, #{
        timeout => 10000,
        strict_mode => false,
        max_pending_requests => 500
    }},
    {cache_enabled, false},
    {features, #{
        rate_limiting => false,
        experimental => true
    }}
]}
```

**Use Cases**:
- Local development iteration
- Feature prototyping
- Bug investigation
- Learning the codebase

**Performance Characteristics**:
- Throughput: Not optimized (verbose logging overhead)
- Latency: Variable (10s timeouts)
- Memory: Higher (debug data retention)

**Logging**:
- Console: All levels (debug, info, warn, error)
- File: `logs/erlmcp-dev.log` (50MB max, 10 files)
- Format: Human-readable, multi-line

---

### Test Profile (test)

**File**: `config/sys.config.test`

**Key Settings**:
```erlang
{erlmcp, [
    {log_level, debug},
    {client_defaults, #{
        timeout => 1000,
        strict_mode => true,
        max_pending_requests => 100
    }},
    {cache_enabled, false},
    {features, #{
        rate_limiting => false,
        async_processing => false
    }}
]}
```

**Use Cases**:
- EUnit test execution
- Common Test suites
- Property-based testing (Proper)
- CI/CD pipelines
- Pre-commit hooks

**Performance Characteristics**:
- Throughput: Fast test execution
- Latency: 1s timeout (fail fast)
- Memory: Minimal (quick cleanup)

**Logging**:
- Console: stderr (for CI capture)
- File: None (deterministic, no I/O overhead)
- Format: Single-line for easy parsing

**Special Features**:
- Random ports (avoid conflicts)
- Synchronous operations (determinism)
- Fast shutdown (quick cleanup)

---

### Staging Profile (staging)

**File**: `config/sys.config.staging`

**Key Settings**:
```erlang
{erlmcp, [
    {log_level, info},
    {client_defaults, #{
        timeout => 5000,
        strict_mode => true,
        max_pending_requests => 250
    }},
    {cache_enabled, true},
    {cache_ttl_seconds, 1800},
    {features, #{
        rate_limiting => true,
        rate_limit_per_second => 500,
        async_processing => true
    }}
]}
```

**Use Cases**:
- Load testing
- Performance benchmarking
- Chaos engineering
- Integration testing with real services
- QA validation

**Performance Characteristics**:
- Throughput: Production-like (rate limited 500 req/s)
- Latency: P99 < 100ms (production-like)
- Memory: Moderate (caching enabled)

**Logging**:
- Console: Info and above
- File: `/var/logs/erlmcp-staging.log` (100MB, compressed rotation)
- Format: Structured, single-line

**Observability**:
- Prometheus metrics enabled
- OpenTelemetry tracing (50% sampling)
- Health checks every 30s

---

### Production Profile (prod)

**File**: `config/sys.config.prod`

**Key Settings**:
```erlang
{erlmcp, [
    {log_level, error},
    {client_defaults, #{
        timeout => 5000,
        strict_mode => true,
        max_pending_requests => 100
    }},
    {cache_enabled, true},
    {cache_ttl_seconds, 3600},
    {features, #{
        rate_limiting => true,
        rate_limit_per_second => 1000,
        async_processing => true,
        experimental => false
    }}
]}
```

**Use Cases**:
- Production releases
- Customer-facing deployments
- Official releases only

**Performance Characteristics**:
- Throughput: Maximum (rate limited 1000 req/s)
- Latency: P99 < 50ms (strict)
- Memory: Optimized (aggressive GC)

**Logging**:
- Console: None (disabled)
- File: `/var/logs/erlmcp-prod.log` (100MB, compressed rotation)
- Format: JSON (structured, machine-readable)

**Security**:
- Debug endpoints: Disabled
- Experimental features: Disabled
- TLS: Required
- Auth: Required

**Observability**:
- Prometheus metrics enabled
- OpenTelemetry tracing (1% sampling)
- Health checks every 10s
- Alerting: Strict thresholds

---

## Advanced Configuration

### Custom Profiles

Create a custom profile for specialized use cases:

```bash
# 1. Copy an existing profile
cp config/sys.config.staging config/sys.config.loadtest

# 2. Edit the new profile
vim config/sys.config.loadtest

# 3. Use the custom profile
export ERLMCP_PROFILE=loadtest
make compile
```

**Example**: High-throughput load test profile:
```erlang
{erlmcp, [
    {log_level, error},  % Minimal logging
    {client_defaults, #{
        timeout => 1000,  % Fast timeout
        max_pending_requests => 10000  % High limit
    }},
    {cache_enabled, true},
    {cache_ttl_seconds, 86400},  % 24-hour cache
    {features, #{
        rate_limiting => false  % No limits for load test
    }}
]}
```

### Per-App Configuration Overrides

Override specific app settings without changing the profile:

```erlang
% config/sys.config.dev
[
    {erlmcp, [
        {log_level, debug}
    ]},
    {erlmcp_core, [
        {custom_setting, value}  % Override for erlmcp_core app only
    ]},
    {erlmcp_transports, [
        {tcp_port, 9999}  % Override TCP port for transports app
    ]}
].
```

### Environment Variable Interpolation

Use environment variables in config files:

```erlang
{erlmcp, [
    {database, #{
        host => {env, "ERLMCP_DB_HOST"},
        port => {env, "ERLMCP_DB_PORT", 5432},  % Default: 5432
        password => {env, "ERLMCP_DB_PASSWORD"}
    }}
]}
```

Load environment variables:
```bash
source config/production.env
export ERLMCP_PROFILE=prod
make compile
```

### Erlang VM Arguments (vm.args)

Tune the Erlang VM for your workload:

```
# vm.args
## Node name
-name erlmcp@127.0.0.1

## Cookie for distributed Erlang
-setcookie erlmcp_cookie

## Scheduler configuration
+S 8:8  # 8 schedulers online and dirty schedulers

## Process limit
+P 262144  # Max 262144 processes

## ETS table limit
+e 100000  # Max 100000 ETS tables

## Memory
+MBas aobf  # Best fit allocator strategy
+MBlmbcs 1024  # Largest multiblock carrier size (KB)

## Distribution
+zdbbl 8192  # Distribution buffer busy limit (KB)

## Tracing
+T 9  # Enable tracing (level 9)
```

### Cluster Configuration (config/cluster.config)

For distributed Erlang clusters:

```erlang
[
    {mnesia, [
        {dir, "/var/lib/erlmcp/mnesia"},
        {schema_location, opt_disc},
        {extra_db_nodes, [
            'erlmcp@node1.example.com',
            'erlmcp@node2.example.com'
        ]},
        {disc_copies, [sessions, resources]}
    ]},
    {kernel, [
        {inet_dist_listen_min, 9100},
        {inet_dist_listen_max, 9200}
    ]}
].
```

### Dashboard Configuration (config/dashboard.config)

For the observability dashboard:

```erlang
[
    {erlmcp_observability, [
        {dashboard, #{
            enabled => true,
            port => 3000,
            host => "0.0.0.0",
            tls => #{
                enabled => true,
                certfile => "/etc/erlmcp/certs/dashboard.crt",
                keyfile => "/etc/erlmcp/certs/dashboard.key"
            },
            auth => #{
                enabled => true,
                type => basic,
                users => [
                    {<<"admin">>, <<"hashed_password">>}
                ]
            }
        }}
    ]}
].
```

## Configuration Validation

### Manual Validation

Check your configuration before deploying:

```bash
# 1. Check syntax
erl -pa _build/default/lib/*/ebin -config config/sys.config -eval 'halt().'

# 2. Validate with dialyzer
rebar3 dialyzer

# 3. Run quality gates
make check
```

### Automated Validation (CI/CD)

```yaml
# .github/workflows/config-validation.yml
name: Config Validation
on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
      - name: Validate dev config
        run: |
          export ERLMCP_PROFILE=dev
          make compile
      - name: Validate test config
        run: |
          export ERLMCP_PROFILE=test
          make compile
      - name: Validate prod config
        run: |
          export ERLMCP_PROFILE=prod
          make compile
```

## Troubleshooting

### Configuration not loading

**Symptom**: Changes to config file have no effect

**Solution**:
```bash
# 1. Check symlink
ls -l config/sys.config

# 2. Force rebuild
make clean compile

# 3. Verify profile
echo $ERLMCP_PROFILE
```

### Profile mismatch in cluster

**Symptom**: Nodes rejecting connections, schema mismatches

**Solution**:
```bash
# Ensure all nodes use the same profile
# On each node:
export ERLMCP_PROFILE=prod
make clean compile
```

### Out of memory errors

**Symptom**: VM crashing with memory exhaustion

**Solution**:
1. Check vm.args memory settings
2. Increase `+MBlmbcs` (multiblock carrier size)
3. Enable aggressive GC in production profile
4. Monitor with `observer:start()`

### Permission denied on log files

**Symptom**: Cannot write to `/var/logs/erlmcp-*.log`

**Solution**:
```bash
# 1. Create log directory
sudo mkdir -p /var/logs
sudo chown erlmcp:erlmcp /var/logs

# 2. Or override in config
{kernel, [
    {logger, [
        {handler, file, logger_std_h, #{
            config => #{
                file => "/tmp/erlmcp.log"  % Temporary fix
            }
        }}
    ]}
]}
```

## Best Practices

### 1. Immutable Profiles

Never modify profile configs directly in production. Instead:
- Create custom profile
- Test in staging
- Deploy immutable release

### 2. Version Control All Configs

```bash
git add config/sys.config.*
git commit -m "config: Update production timeouts"
```

### 3. Secret Management

Never commit secrets:
```bash
# .gitignore
config/*.env
config/secrets/
```

Use environment variables or secret managers:
```erlang
{database_password, {env, "ERLMCP_DB_PASSWORD"}}
```

### 4. Document Custom Settings

Always document why you changed a setting:
```erlang
% Increased timeout from 5s to 10s due to slow downstream API
% See: ISSUE-1234
{client_defaults, #{
    timeout => 10000
}}
```

### 5. Monitor Configuration Changes

Alert on unexpected config changes in production:
- Use checksums
- Enable audit logging
- Require approval for production config changes

## Configuration Reference

### Complete erlmcp App Settings

```erlang
{erlmcp, [
    % Logging
    {log_level, debug | info | warn | error},
    {logger_level, debug | info | warn | error},
    
    % Client settings
    {client_defaults, #{
        timeout => integer(),              % ms
        strict_mode => boolean(),
        max_pending_requests => integer(),
        retry_attempts => integer(),
        retry_backoff_ms => integer()
    }},
    
    % Server settings
    {server_defaults, #{
        max_subscriptions_per_resource => integer(),
        max_progress_tokens => integer(),
        enable_debug_endpoints => boolean()
    }},
    
    % Transport settings
    {transport_defaults, #{
        tcp => #{
            connect_timeout => integer(),
            keepalive => boolean(),
            nodelay => boolean(),
            listen_backlog => integer()
        },
        http => #{
            connect_timeout => integer(),
            request_timeout => integer(),
            max_connections => integer()
        },
        stdio => #{
            enabled => boolean(),
            buffer_size => integer()
        }
    }},
    
    % Connection pooling
    {connection_pooling, #{
        enabled => boolean(),
        pool_size => integer(),
        max_overflow => integer(),
        pool_timeout => integer()
    }},
    
    % Caching
    {cache_enabled, boolean()},
    {cache_ttl_seconds, integer()},
    {cache_max_size, integer()},
    
    % Feature toggles
    {features, #{
        rate_limiting => boolean(),
        rate_limit_per_second => integer(),
        async_processing => boolean(),
        webhook_delivery => boolean(),
        experimental => boolean()
    }}
]}
```

## Related Documentation

- [ENVIRONMENT_GUIDE.md](ENVIRONMENT_GUIDE.md) - Profile selection guide
- [DEVELOPMENT.md](../DEVELOPMENT.md) - Development workflow
- [CLAUDE.md](../CLAUDE.md) - System architecture
- [SESSION_PERSISTENCE.md](SESSION_PERSISTENCE.md) - Session backend configuration
- [SECRETS_MANAGEMENT.md](SECRETS_MANAGEMENT.md) - Secrets configuration

---

**Last Updated**: February 2026 (v2.1.0)
**Maintainer**: erlmcp core team
