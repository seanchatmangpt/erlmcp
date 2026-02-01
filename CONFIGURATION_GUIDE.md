# ErlMCP Configuration Guide

This document describes how to configure ErlMCP application parameters externally through configuration files and environment variables.

## Configuration Hierarchy

ErlMCP uses a three-level configuration hierarchy (in order of precedence):

1. **Environment Variables** (highest priority) - Runtime override via OS environment
2. **Application Configuration** (sys.config) - Deployed configuration
3. **Built-in Defaults** (lowest priority) - Code defaults

### Example

```erlang
% Application environment takes precedence over built-in default
Value = application:get_env(erlmcp_core, timeout_ms, 5000).  % Default: 5000ms
```

If `ERL_MCP_TIMEOUT_MS=10000` is set in the environment, the value would be 10000ms.

## Configuration Files

### Development

**File**: `config/sys.config.dev`

Optimized for fast iteration with permissive limits:
- Longer RPC timeouts (10-30s for debugging)
- Disabled CPU quota enforcement
- Relaxed memory thresholds (90% vs 80%)
- Higher alert history limits
- Extensive logging (DEBUG level)

### Production

**File**: `config/sys.config.prod`

Hardened for production with strict enforcement:
- Tight RPC timeouts (5-10s)
- CPU quota enabled and enforced
- Aggressive memory thresholds (80%)
- Audit logging enabled
- Warning-only console output

### Test

**File**: `config/sys.config.test`

Optimized for CI/CD and rapid test execution:
- Fast timeouts (1-5s)
- Disabled monitoring (no interval checks)
- Minimal alert history
- Synchronous operations
- Random ports to avoid conflicts

## Configuration Parameters

### RPC Timeout Configuration

Controls timeouts for gen_server:call operations across the system.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `rpc_call_timeout_ms` | integer | 5000 | 10000 | 5000 | 1000 | Standard RPC timeout (ms) |
| `rpc_long_call_timeout_ms` | integer | 10000 | 30000 | 10000 | 5000 | Long-running operation timeout (ms) |

**Used By**:
- `erlmcp_plugin_registry`
- `erlmcp_apps_server`
- `erlmcp_client_transport`

**Example**:
```erlang
% In sys.config
{erlmcp, [
    {rpc_call_timeout_ms, 5000},
    {rpc_long_call_timeout_ms, 10000}
]}
```

### Apps Server Configuration

Controls application registration and lifecycle management.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `max_registered_apps` | integer | 100 | 500 | 100 | 50 | Maximum applications to register simultaneously |

**Used By**:
- `erlmcp_apps_server` - Application registration limit

**Example**:
```erlang
{erlmcp, [
    {max_registered_apps, 500}  % Allow more apps in dev
]}
```

### Plugin Registry Configuration

Controls plugin discovery and routing.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `plugin_registry_timeout_ms` | integer | 5000 | 10000 | 5000 | 1000 | Plugin registry RPC timeout (ms) |

**Used By**:
- `erlmcp_plugin_registry` - All plugin operations

**Example**:
```erlang
{erlmcp, [
    {plugin_registry_timeout_ms, 10000}  % Longer timeout for debugging
]}
```

### Overload Monitor Configuration

Tracks queue depth and alerts when capacity thresholds are exceeded.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `overload_monitor_enabled` | boolean | true | true | true | false | Enable overload monitoring |
| `overload_check_interval_ms` | integer | 5000 | 5000 | 5000 | 5000 | Health check interval (ms) |
| `overload_max_alert_history` | integer | 100 | 500 | 100 | 50 | Maximum alerts to retain |
| `overload_warning_threshold` | float | 0.80 | 0.80 | 0.80 | 0.80 | Queue capacity threshold for warnings (80%) |
| `overload_critical_threshold` | float | 0.90 | 0.90 | 0.90 | 0.90 | Queue capacity threshold for critical (90%) |
| `overload_threshold` | float | 1.00 | 1.00 | 1.00 | 1.00 | Queue capacity threshold for overload (100%) |

**Used By**:
- `erlmcp_overload_monitor` - Queue depth tracking

**Example**:
```erlang
{erlmcp, [
    {overload_monitor_enabled, true},
    {overload_check_interval_ms, 5000},
    {overload_max_alert_history, 100},
    {overload_warning_threshold, 0.80},
    {overload_critical_threshold, 0.90},
    {overload_threshold, 1.00}
]}
```

### Node Monitor Configuration

Monitors cluster node health and connectivity.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `node_monitor_enabled` | boolean | true | true | true | false | Enable node monitoring |
| `node_check_interval_ms` | integer | 5000 | 5000 | 5000 | 5000 | Node health check interval (ms) |

**Used By**:
- `erlmcp_node_monitor` - Cluster connectivity monitoring

**Example**:
```erlang
{erlmcp, [
    {node_monitor_enabled, true},
    {node_check_interval_ms, 5000}
]}
```

### CPU Quota Configuration

Protects against CPU-intensive DoS attacks through per-client quotas.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `cpu_quota_enabled` | boolean | true | false | true | false | Enable CPU quota enforcement |
| `cpu_quota_max_cpu_time_per_sec_ms` | integer | 100 | 500 | 100 | 100 | Max CPU time per second (ms) |
| `cpu_quota_max_ops_per_sec` | integer | 50 | 1000 | 50 | 50 | Max operations per second |
| `cpu_quota_window_ms` | integer | 1000 | 1000 | 1000 | 1000 | Sliding window size (ms) |
| `cpu_quota_timeout_ms` | integer | 5000 | 30000 | 5000 | 5000 | Quota operation timeout (ms) |
| `cpu_quota_cleanup_interval_ms` | integer | 60000 | 300000 | 60000 | 60000 | Cleanup interval (ms) |

**Used By**:
- `erlmcp_cpu_quota` - CPU usage tracking and limiting

**Example**:
```erlang
{erlmcp, [
    {cpu_quota_enabled, true},
    {cpu_quota_max_cpu_time_per_sec_ms, 100},
    {cpu_quota_max_ops_per_sec, 50},
    {cpu_quota_window_ms, 1000},
    {cpu_quota_timeout_ms, 5000},
    {cpu_quota_cleanup_interval_ms, 60000}
]}
```

### Memory Monitor Configuration

Monitors heap and binary memory usage, triggers GC when thresholds exceeded.

| Parameter | Type | Default | Dev | Prod | Test | Description |
|-----------|------|---------|-----|------|------|-------------|
| `memory_monitor_enabled` | boolean | true | true | true | false | Enable memory monitoring |
| `memory_monitor_check_interval_ms` | integer | 30000 | 60000 | 30000 | 30000 | Memory check interval (ms) |
| `memory_monitor_threshold` | float | 0.80 | 0.90 | 0.80 | 0.80 | Memory threshold (fraction of limit) |
| `memory_monitor_binary_threshold_bytes` | integer | 50000000 | 500000000 | 50000000 | 50000000 | Binary memory limit (bytes) |
| `memory_monitor_auto_gc` | boolean | true | true | true | false | Auto GC on memory pressure |

**Used By**:
- `erlmcp_memory_monitor` - Heap and binary memory management

**Example**:
```erlang
{erlmcp, [
    {memory_monitor_enabled, true},
    {memory_monitor_check_interval_ms, 30000},
    {memory_monitor_threshold, 0.80},
    {memory_monitor_binary_threshold_bytes, 50000000},
    {memory_monitor_auto_gc, true}
]}
```

## Using Environment Variables

Each configuration parameter can be overridden via environment variables using the following pattern:

```bash
export ERL_MCP_<PARAM_NAME>=<VALUE>
```

### Examples

```bash
# Override RPC timeout to 15 seconds
export ERL_MCP_RPC_CALL_TIMEOUT_MS=15000

# Disable CPU quota
export ERL_MCP_CPU_QUOTA_ENABLED=false

# Set custom max apps
export ERL_MCP_MAX_REGISTERED_APPS=1000
```

**Note**: Environment variable support depends on the application implementing it via `os:getenv()` or similar. Check individual module documentation for environment variable support.

## Configuration Profiles

ErlMCP supports environment-specific configurations:

### Loading a Configuration Profile

```bash
# Use development config
erl -config config/sys.config.dev

# Use production config
erl -config config/sys.config.prod

# Use test config
erl -config config/sys.config.test
```

### Creating Custom Profiles

1. Copy an existing config file
2. Adjust parameters for your environment
3. Use `-config` flag when starting Erlang

Example custom production config:

```erlang
% config/sys.config.custom-prod
[
    {erlmcp, [
        % More aggressive limits for large deployments
        {max_registered_apps, 2000},
        {rpc_call_timeout_ms, 3000},  % Tighter timeout
        {cpu_quota_enabled, true},
        {cpu_quota_max_ops_per_sec, 100},  % Higher throughput
        {memory_monitor_threshold, 0.75}  % Earlier GC trigger
    ]}
]
```

## Best Practices

### 1. Use Defaults Unless Override Needed

Keep configuration files minimal - only set parameters that differ from defaults.

### 2. Document Custom Configurations

When overriding defaults, add a comment explaining why:

```erlang
{erlmcp, [
    % Increased to 15s to accommodate slow CI/CD runners
    {rpc_call_timeout_ms, 15000},

    % Disabled to reduce test noise
    {memory_monitor_enabled, false}
]}
```

### 3. Version Control Configurations

Store config files in git, but use environment variables for sensitive values:

```erlang
% In sys.config
{some_app, [
    {api_key, os:getenv("API_KEY", "default-key")}
]}
```

### 4. Test Configuration Changes

Always verify configuration changes in a non-production environment:

```bash
# Load test config and run verification
erl -config config/sys.config.prod -eval 'test_config:verify(), halt().'
```

### 5. Monitor Configuration Impact

- Check logs for configuration warnings
- Monitor metrics for changes in behavior
- Adjust timeouts based on actual latencies

## Troubleshooting

### Configuration Not Taking Effect

1. Verify the config file is being loaded:
   ```bash
   erl -config config/sys.config.dev -eval 'application:get_env(erlmcp_core, rpc_call_timeout_ms), halt().'
   ```

2. Check application startup order - ensure erlmcp_core starts after kernel

3. Verify syntax of sys.config file (must be valid Erlang terms)

### Timeouts Too Aggressive

If you see frequent timeout errors:
1. Increase `rpc_call_timeout_ms` or `rpc_long_call_timeout_ms`
2. Monitor system load and network latency
3. Check if upstream services are slow

### Memory Issues

If memory monitor triggers too often:
1. Increase `memory_monitor_threshold` (e.g., 0.90 for 90%)
2. Increase `memory_monitor_check_interval_ms` (less frequent checks)
3. Review application heap requirements

### CPU Quota Rejections

If legitimate requests are rejected:
1. Increase `cpu_quota_max_ops_per_sec`
2. Increase `cpu_quota_max_cpu_time_per_sec_ms`
3. Consider disabling `cpu_quota_enabled` if not needed

## Related Documentation

- `docs/architecture.md` - System design overview
- `apps/erlmcp_core/src/erlmcp_core.app.src` - Default configuration
- Individual module documentation for module-specific configuration
