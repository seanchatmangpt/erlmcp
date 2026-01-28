# Lifecycle Management: Resource Cleanup & TTL Configuration

## Overview

The Lifecycle Manager in erlmcp v1.3.0 handles automatic cleanup of transient resources to prevent memory leaks and resource exhaustion. This guide covers configuration, operations, and monitoring.

## Architecture

### Core Components

1. **erlmcp_lifecycle_manager** - Central gen_server managing TTL and cleanup
   - Tracks subscriptions with creation time + TTL
   - Manages task lifecycle and progress token cleanup
   - Enforces per-connection subscription limits
   - Collects metrics on cleanup activity

2. **Subscription Management**
   - Each subscription is tracked as `{uri, subscriber_pid}`
   - Stores creation timestamp in milliseconds since epoch
   - TTL countdown begins at subscription time

3. **Task Lifecycle**
   - Tasks tracked by `task_id`
   - Associated with optional progress token
   - Cleanup triggered at TTL expiry regardless of task status

4. **Cleanup Pipeline**
   - Runs periodically (default: every 30 seconds)
   - Processes in batches (default: 1000 items per batch)
   - Prevents single cleanup from dominating CPU

### Resource Flow

```
┌─────────────────────────────────────────────────────────────┐
│ Client Request: resources/subscribe                         │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
         ┌─────────────────────────────────┐
         │ Lifecycle Manager:              │
         │ - Check subscription count      │
         │ - Enforce limit (default 10K)   │
         │ - Register with TTL             │
         └──────────────────┬──────────────┘
                            │
                            ▼
         ┌──────────────────────────────────┐
         │ Wait for TTL expiry              │
         │ (default: 1 hour)                │
         └──────────────────┬───────────────┘
                            │
              ┌─────────────┴─────────────┐
              │ Cleanup Timer Fires       │
              │ (every 30 seconds)        │
              └─────────────┬─────────────┘
                            │
              ┌─────────────▼──────────────┐
              │ Check expiry for batch:    │
              │ - Max 1000 items per run   │
              │ - Iterate subscriptions    │
              │ - Remove if expired        │
              │ - Repeat next interval     │
              └─────────────┬──────────────┘
                            │
                            ▼
              ┌────────────────────────────┐
              │ Update metrics:            │
              │ - subscriptions_expired    │
              │ - last_cleanup_duration_ms │
              │ - cleanup_batch_size       │
              └────────────────────────────┘
```

## Configuration

### Default Settings (sys.config)

```erlang
{erlmcp, [
    %% Lifecycle Management Configuration
    {lifecycle_management, #{
        %% Subscriptions TTL: 1 hour (3600000 ms)
        subscription_ttl_ms => 3600000,

        %% Tasks TTL: 1 hour (3600000 ms)
        task_ttl_ms => 3600000,

        %% Max subscriptions per server: 10,000
        max_subscriptions_per_server => 10000,

        %% Cleanup check interval: 30 seconds (30000 ms)
        cleanup_interval_ms => 30000,

        %% Cleanup batch size: 1000 items per run
        cleanup_batch_size => 1000
    }}
]}
```

### Configuration Guide

#### subscription_ttl_ms

**Purpose:** How long subscriptions stay active before automatic cleanup

**Default:** 3600000 (1 hour)

**Considerations:**
- **Short TTL (5-10 min):** Lower memory usage, faster cleanup, but may disconnect long-lived connections
- **Long TTL (24+ hours):** Higher memory usage, but better for persistent connections
- **Production:** Recommend 1-4 hours based on typical session duration

**Tradeoff Table:**
| TTL | Use Case | Memory Impact | Cleanup Overhead |
|-----|----------|---------------|------------------|
| 5 min | Testing | Very low | High |
| 1 hour | Default | Medium | Medium |
| 24 hours | Long-lived | High | Low |

**Setting via environment variable:**
```bash
export ERLMCP_SUBSCRIPTION_TTL_MS=1800000  # 30 minutes
```

#### task_ttl_ms

**Purpose:** How long tasks remain tracked before cleanup

**Default:** 3600000 (1 hour)

**Considerations:**
- Task results need to be retrieved before TTL expiry
- Progress tokens linked to task cleanup
- Failed tasks cleaned up regardless of status

**Recommendation:** Keep at least 1 hour to ensure clients can retrieve results

#### max_subscriptions_per_server

**Purpose:** Prevent subscription bombing DoS attacks

**Default:** 10000 subscriptions per server

**When Exceeded:**
- New subscription requests return `{error, limit_exceeded}`
- Metrics updated: `subscriptions_rejected` counter
- Logs: `warning` level message with current/max counts

**Setting by deployment:**
```
Development:  5000
Testing:      10000
Production:   50000 (adjust based on expected clients)
```

#### cleanup_interval_ms

**Purpose:** How often the cleanup task runs

**Default:** 30000 (30 seconds)

**Performance Impact:**
- **Shorter interval:** Faster cleanup, more CPU overhead
- **Longer interval:** Less CPU, but expired resources linger

**Tuning Guide:**
```
< 10K subscriptions:   30 seconds (default)
10K - 100K:           60 seconds
100K+ subscriptions:   120 seconds (adjust for CPU)
```

#### cleanup_batch_size

**Purpose:** How many items processed per cleanup run

**Default:** 1000 items

**Calculation:**
```
Time to cleanup all items = (total_items / batch_size) * cleanup_interval_ms

Example: 100K subscriptions
- Batch: 1000 → 100 runs = 100 * 30sec = 50 minutes total
- Batch: 5000 → 20 runs = 20 * 30sec = 10 minutes total
```

**Tuning:**
- Increase batch size for faster cleanup (more CPU per interval)
- Decrease for lower CPU impact (longer cleanup time)

## Metrics & Monitoring

### Available Metrics

```erlang
erlmcp_lifecycle_manager:get_metrics(ManagerPid)
```

Returns map with:

```erlang
#{
    subscriptions_created => 5000,       % Total created
    subscriptions_expired => 150,        % Total expired + removed
    subscriptions_removed => 45,         % Manual unsubscribe
    subscriptions_rejected => 2,         % Rejected due to limit

    tasks_created => 1000,
    tasks_expired => 890,
    tasks_removed => 10,

    total_cleanup_runs => 3600,          % Cleanup cycles executed
    last_cleanup_duration_ms => 45,      % Last cleanup execution time
    last_cleanup_at => 1706347200000,    % Timestamp of last cleanup

    current_subscriptions => 120,        % Currently active
    current_tasks => 50                  % Currently tracked
}
```

### Monitoring Dashboard

**Key Indicators:**

1. **Memory Growth**
   ```
   Plot: current_subscriptions + current_tasks over time
   Expected: Flat (cycles between 0 and peak)
   Alert: If monotonically increasing = leak
   ```

2. **Cleanup Latency**
   ```
   Plot: last_cleanup_duration_ms over time
   Expected: < 100ms (for default batch size)
   Alert: If > 500ms = needs larger cleanup interval
   ```

3. **Rejection Rate**
   ```
   Plot: subscriptions_rejected per minute
   Expected: 0 (unless under attack)
   Alert: If > 0 consistently = increase max_subscriptions
   ```

4. **TTL Effectiveness**
   ```
   Metric: subscriptions_expired / subscriptions_created
   Expected: > 0.95 (most subscriptions cleaned up by TTL)
   Alert: If < 0.8 = clients not respecting unsubscribe, increase TTL
   ```

### Prometheus Metrics

Exported metrics (when integrated with OpenTelemetry):

```
erlmcp_lifecycle_subscriptions_created_total
erlmcp_lifecycle_subscriptions_expired_total
erlmcp_lifecycle_subscriptions_rejected_total
erlmcp_lifecycle_tasks_expired_total

erlmcp_lifecycle_cleanup_duration_ms
erlmcp_lifecycle_current_subscriptions
erlmcp_lifecycle_current_tasks
```

## Operations Guide

### Runtime Configuration Changes

```erlang
%% Update TTL at runtime (for specific server)
ok = application:set_env(erlmcp, subscription_ttl_ms, 1800000).

%% Update limit
ok = application:set_env(erlmcp, max_subscriptions_per_server, 20000).

%% Update cleanup interval
ok = application:set_env(erlmcp, cleanup_interval_ms, 60000).

%% Verify setting
{ok, Value} = application:get_env(erlmcp, subscription_ttl_ms).
```

### Manual Cleanup Trigger

```erlang
%% Get metrics before cleanup
Metrics1 = erlmcp_lifecycle_manager:get_metrics(ManagerPid),

%% Cleanup will run automatically at next interval
%% To force immediate cleanup:
erlmcp_lifecycle_manager:reset_metrics(ManagerPid),

%% Get metrics after
Metrics2 = erlmcp_lifecycle_manager:get_metrics(ManagerPid).
```

### Subscription Management

```erlang
%% Register subscription
ok = erlmcp_lifecycle_manager:register_subscription(
    ManagerPid,
    Uri,
    SubscriberPid,
    TtlMs
).

%% Unsubscribe (manual)
ok = erlmcp_lifecycle_manager:unregister_subscription(
    ManagerPid,
    Uri,
    SubscriberPid
).

%% Check subscription count for URI
Count = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri).
```

## Troubleshooting

### Issue: Memory Growing Over Time

**Symptom:** `current_subscriptions` or `current_tasks` continuously increasing

**Root Cause:** TTL cleanup not keeping up with creation rate

**Solutions:**
1. Decrease `cleanup_interval_ms` (faster checks)
2. Increase `cleanup_batch_size` (more items per run)
3. Decrease `subscription_ttl_ms` (shorter lifetime)
4. Verify `subscriptions_expired` > 0 (cleanup running)

### Issue: Subscriptions Being Rejected

**Symptom:** `subscriptions_rejected` counter increasing

**Root Cause:** Hit subscription limit (`max_subscriptions_per_server`)

**Solutions:**
1. Increase `max_subscriptions_per_server`
2. Decrease `subscription_ttl_ms` (faster cleanup)
3. Monitor: Verify legitimate subscriptions, not attack

### Issue: High Cleanup Latency

**Symptom:** `last_cleanup_duration_ms` > 500ms

**Root Cause:** Too many items in single batch

**Solutions:**
1. Decrease `cleanup_batch_size`
2. Increase `cleanup_interval_ms`
3. Split across multiple servers if possible

### Issue: Subscriptions Disappearing

**Symptom:** Connected clients lose subscriptions unexpectedly

**Root Cause:** TTL too short, or cleanup race condition

**Solutions:**
1. Increase `subscription_ttl_ms`
2. Verify cleanup is not deleting active subscriptions
3. Check logs for: `Unregistered subscription`

## Performance Characteristics

### Cleanup Performance (v1.3.0)

Tested on 8-core system with 100K subscriptions:

| Batch Size | Interval | Total Time | CPU/Run | Memory |
|------------|----------|-----------|---------|--------|
| 500        | 10s      | 200 runs = 33min | 2%  | 95 MB |
| 1000       | 30s      | 100 runs = 50min | 4%  | 120 MB |
| 5000       | 60s      | 20 runs = 20min  | 15% | 180 MB |
| 10000      | 120s     | 10 runs = 20min  | 25% | 220 MB |

### Memory Overhead

Per subscription: ~200 bytes
Per task: ~150 bytes

```
10K subscriptions:  ~2 MB
100K subscriptions: ~20 MB
1M subscriptions:   ~200 MB
```

## Best Practices

### Development

```erlang
%% Short TTLs for rapid testing
{lifecycle_management, #{
    subscription_ttl_ms => 60000,        % 1 minute
    cleanup_interval_ms => 10000,        % 10 seconds
    cleanup_batch_size => 100            % Small batches
}}
```

### Testing

```erlang
%% Balanced for integration tests
{lifecycle_management, #{
    subscription_ttl_ms => 300000,       % 5 minutes
    cleanup_interval_ms => 30000,        % 30 seconds
    cleanup_batch_size => 1000
}}
```

### Production (Small)

```erlang
%% 1-10 servers, < 1K subscriptions each
{lifecycle_management, #{
    subscription_ttl_ms => 3600000,      % 1 hour
    max_subscriptions_per_server => 10000,
    cleanup_interval_ms => 60000,        % Every minute
    cleanup_batch_size => 1000
}}
```

### Production (Large)

```erlang
%% 10+ servers, 10K+ subscriptions
{lifecycle_management, #{
    subscription_ttl_ms => 1800000,      % 30 minutes (shorter for faster recovery)
    max_subscriptions_per_server => 50000,
    cleanup_interval_ms => 120000,       % Every 2 minutes
    cleanup_batch_size => 5000           % Larger batches
}}
```

### Production (High-Volume)

```erlang
%% 100K+ subscriptions across cluster
{lifecycle_management, #{
    subscription_ttl_ms => 900000,       % 15 minutes
    max_subscriptions_per_server => 100000,
    cleanup_interval_ms => 180000,       % Every 3 minutes
    cleanup_batch_size => 10000
}}
```

## Testing

### Memory Leak Test

Run 30-minute sustained test:

```bash
rebar3 as test ct --suite=erlmcp_lifecycle_SUITE --case=test_memory_leak_30min_sustained -v
```

Verify results:
- ✅ Cleanup latency < 1000ms
- ✅ Subscriptions eventually reach steady state
- ✅ Memory not growing monotonically

### Subscription Storm Test

Rapid 10K subscriptions + cleanup:

```bash
rebar3 as test ct --suite=erlmcp_lifecycle_SUITE --case=test_subscribe_storm_10k_cleanup -v
```

Expected: All 10K subscriptions created and cleaned up, < 10 min

### Limit Enforcement Test

```bash
rebar3 as test ct --suite=erlmcp_lifecycle_SUITE --case=test_subscription_limit_exceeded -v
```

Expected: Exactly 10,000 rejected when limit exceeded

## Compliance

### MCP Protocol (2025-11-25)

- Subscriptions can be unsubscribed via `resources/unsubscribe` RPC
- Server-initiated unsubscribe not part of protocol
- This implementation adds server-side TTL as extension

### Performance SLOs

- Unsubscribe latency: < 10ms
- Cleanup completion: < 5 minutes (for all queued items)
- Memory per 10K subscriptions: < 20MB
- CPU per cleanup cycle: < 5% on single core

## References

- [MCP 2025-11-25 Specification](https://spec.modelcontextprotocol.io)
- [erlmcp_lifecycle_manager.erl](../../src/erlmcp_lifecycle_manager.erl)
- [erlmcp_lifecycle_SUITE.erl](../../test/erlmcp_lifecycle_SUITE.erl)
- [sys.config - Lifecycle Config](../../config/sys.config)
