# Configuration Profiles v1.4.0

erlmcp v1.4.0 introduces configuration profiles for managing environment-specific settings across Development, Production, and Government/Regulated deployments.

## Overview

Profiles provide pre-configured, validated configurations for different environments:

- **dev**: Fast feedback, relaxed limits, debug logging
- **prod**: Strict enforcement, HTTPS required, circuit breaker ON
- **gov**: Audit logging, deterministic behavior, FIPS-140-2 compatible

## Profile Manager API

The `erlmcp_profile_manager` module provides the core API:

```erlang
%% List all available profiles
erlmcp_profile_manager:list_profiles()
%% => {ok, [{dev, "..."}, {prod, "..."}, {gov, "..."}]}

%% Show profile configuration
erlmcp_profile_manager:show_profile(dev)
%% => {ok, [{erlmcp, [...]}, {kernel, [...]}]}

%% Apply profile to runtime
erlmcp_profile_manager:apply_profile(prod, runtime)
%% => {ok, applied}

%% Apply profile to sys.config file
erlmcp_profile_manager:apply_profile(prod, file)
%% => {ok, applied}

%% Validate profile configuration
erlmcp_profile_manager:validate_profile(gov)
%% => ok | {error, Reason}

%% Get current active profile
erlmcp_profile_manager:get_current_profile()
%% => dev | prod | gov | undefined

%% Merge profile with current config
erlmcp_profile_manager:merge_profile(prod)
%% => {ok, MergedConfig}
```

## CLI Usage

### List Profiles

```bash
erlmcp profile list
```

Output:
```
======================================================================
                         ERLMCP PROFILES
======================================================================

Available profiles:

  ✓ dev
     Development - Fast feedback, relaxed limits, debug logging

  ✓ prod
     Production - Strict limits, HTTPS required, circuit breaker ON

  ✓ gov
     Government - Audit logging, deterministic, FIPS-140-2 compatible

Use: erlmcp profile show <name>
Use: erlmcp profile apply <name>
```

### Show Profile

```bash
erlmcp profile show prod
```

Displays the full configuration for a profile with:
- Log level settings
- Rate limiting configuration
- Circuit breaker settings
- HTTPS/TLS configuration
- Session management options
- And more...

### Apply Profile

```bash
erlmcp profile apply prod
```

Merges the profile configuration into the runtime environment:
- Sets application environment variables
- Marks profile as active
- Returns immediately (no restart required)

### Validate Profile

```bash
erlmcp profile validate gov
```

Validates profile configuration:
- Checks for required settings
- Validates configuration structure
- Reports summary of key settings

## Development Profile (dev)

**Use when**: Local development, rapid iteration, debugging

### Key Characteristics

| Setting | dev | Notes |
|---------|-----|-------|
| Log Level | debug | Full visibility into operations |
| Rate Limiting | disabled | Fast feedback without throttling |
| Circuit Breaker | disabled | Retry quickly without cool-down |
| Client Timeout | 30s | Long timeout for debugging |
| Message Limits | 32 MB | Higher for early issue detection |
| Rate Limits | 10,000+ msgs/sec | Permissive for testing |
| HTTPS | disabled | Use HTTP for local testing |
| Backpressure | relaxed | Minimal restrictions |

### Example Configuration

```erlang
{erlmcp, [
    {log_level, debug},
    {rate_limiting, #{enabled => false}},
    {circuit_breaker, #{enabled => false}},
    {client_defaults, #{timeout => 30000}},
    {https_config, [{enabled, false}]}
]}
```

### When to Use

- Local testing and iteration
- Debugging issues
- Development/integration testing
- Lab environments

## Production Profile (prod)

**Use when**: Live deployments, mission-critical services

### Key Characteristics

| Setting | prod | Notes |
|---------|------|-------|
| Log Level | info | Minimal overhead, no debug logs |
| Rate Limiting | enabled | Strict per-client limits (100 msgs/sec) |
| Circuit Breaker | enabled | Prevent cascading failures |
| Client Timeout | 5s | Standard timeout for reliability |
| Message Limits | 16 MB | MCP compliance standard |
| DDoS Protection | 100 violations/min | Block for 5 minutes if exceeded |
| HTTPS | required | Enforce TLS 1.2+ |
| Backpressure | adaptive | Automatic rate reduction on latency spikes |

### Example Configuration

```erlang
{erlmcp, [
    {log_level, info},
    {rate_limiting, #{
        enabled => true,
        max_messages_per_sec => 100,
        ddos_block_duration_ms => 300000
    }},
    {circuit_breaker, #{
        enabled => true,
        failure_threshold => 5,
        cool_down_time_ms => 30000
    }},
    {https_config, [{enabled, true}]},
    {backpressure, #{adaptive_enabled => true}}
]}
```

### When to Use

- Production deployments
- SaaS platforms
- Public APIs
- Enterprise systems

## Government/Regulated Profile (gov)

**Use when**: Government agencies, compliance requirements, audit trails

### Key Characteristics

| Setting | gov | Notes |
|---------|-----|-------|
| Log Level | debug | Full audit trail for compliance |
| Rate Limiting | strict | 50 msgs/sec, very tight control |
| Circuit Breaker | strict | 3 failures threshold, sensitive |
| Client Timeout | 5s | Standard production timeout |
| Message Limits | 10 MB | Conservative for safety |
| Audit Logging | enabled | Every operation logged |
| HTTPS | required | FIPS-140-2 compatible ciphers |
| Determinism | enforced | No compression, reproducible behavior |
| DDoS Protection | aggressive | Very low threshold (50/min) |

### Example Configuration

```erlang
{erlmcp, [
    {log_level, debug},
    {audit_logging, [
        {enabled, true},
        {log_all_operations, true},
        {log_file, "logs/audit.log"}
    ]},
    {rate_limiting, #{
        enabled => true,
        max_messages_per_sec => 50,
        max_connections_per_sec => 5
    }},
    {circuit_breaker, #{
        enabled => true,
        failure_threshold => 3
    }},
    {https_config, [
        {enabled, true},
        {verify_mode, 'verify_peer'},
        {verify_hostname, true},
        {ciphers, ["ECDHE-RSA-AES256-GCM-SHA384", ...]}
    ]},
    {session_replication, [
        {storage_mode, mnesia},
        {enable_compression, false}
    ]}
]}
```

### When to Use

- Government deployments
- Financial institutions
- Healthcare systems
- Regulated industries
- Systems requiring audit trails
- Deterministic behavior requirements

## Profile Comparison

### Logging & Observability

```
dev  → debug logging, relaxed settings
prod → info logging, production-grade metrics
gov  → debug logging, comprehensive audit trail
```

### Rate Limiting

```
dev  → disabled (fast feedback)
prod → 100 msgs/sec per client, 10K global
gov  → 50 msgs/sec per client, very strict
```

### Circuit Breaker

```
dev  → disabled (retry quickly)
prod → enabled (5 failures → open)
gov  → enabled (3 failures → open, strict)
```

### Message Size Limits

```
dev  → 32 MB (catch issues early)
prod → 16 MB (MCP standard)
gov  → 10 MB (conservative safety)
```

### HTTPS/TLS

```
dev  → disabled (local testing)
prod → enabled, TLS 1.2+
gov  → enabled, FIPS-140-2 ciphers
```

## Profile Switching

You can transition between profiles at runtime without restarting:

```erlang
%% Start in dev
erlmcp_profile_manager:apply_profile(dev, runtime).

%% Switch to prod
erlmcp_profile_manager:apply_profile(prod, runtime).

%% Check current
erlmcp_profile_manager:get_current_profile().
%% => prod
```

## Error Handling

Profile operations return standard Erlang error tuples:

```erlang
{error, {profile_not_found, Profile}}
{error, {invalid_config_format, Reason}}
{error, {failed_to_apply_profile, Reason}}
{error, {failed_to_write_config, Reason}}
{error, {invalid_target, Target}}
```

## Limitations & Known Issues

### File-Based Application

When applying profiles to `sys.config` (file target):
- A backup is created automatically (e.g., `sys.config.backup-20260127-163000`)
- If write fails, the backup is restored
- Server restart required for file-based changes to take effect

### Runtime Application

When applying profiles to runtime:
- Only affects current process/node
- Doesn't persist across restarts
- Use file target for persistence

### Configuration Merging

- Profiles completely replace application config for that app
- Per-environment customizations require manual merge
- Custom configs should be applied after profile application

## Testing

Comprehensive test suite in `test/erlmcp_profile_manager_tests.erl`:

```bash
rebar3 eunit --module=erlmcp_profile_manager_tests
```

Tests cover:
- Profile listing and discovery
- Profile content retrieval and validation
- Runtime and file-based application
- Profile transitions
- Configuration consistency
- Error handling

## Future Enhancements

Planned for v1.5.0+:

- [ ] Custom profile creation API
- [ ] Profile inheritance (extend existing profiles)
- [ ] Environment variable substitution in profiles
- [ ] Per-node profile selection in clusters
- [ ] Profile health checks and self-healing
- [ ] Gradual profile transitions (canary deployments)

## See Also

- `/Users/sac/erlmcp/src/erlmcp_profile_manager.erl` - Implementation
- `/Users/sac/erlmcp/profiles/dev.config` - Development profile
- `/Users/sac/erlmcp/profiles/prod.config` - Production profile
- `/Users/sac/erlmcp/profiles/gov.config` - Government profile
- `/Users/sac/erlmcp/config/sys.config` - System configuration reference
