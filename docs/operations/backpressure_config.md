# Backpressure & Queue Limits Configuration Guide (v1.3.0)

## Overview

erlmcp v1.3.0 implements hard queue limits with deterministic backpressure to prevent memory exhaustion and latency spirals under sustained overload. This guide covers all configuration options, operational procedures, and troubleshooting.

## Architecture

The backpressure system consists of two layers:

### Layer 1: Queue Limits (Hard Bounds)
- **Module**: `erlmcp_queue_limits`
- **Per-connection enforcement**: Message count + byte size limits
- **Per-tenant aggregation**: Optional multi-client enforcement
- **Actions**: Refuse (deterministic), Drop (FIFO), Disconnect (nuclear)

### Layer 2: Rate Limiting (Token Bucket)
- **Module**: `erlmcp_backpressure`
- **Algorithm**: Token bucket with adaptive rate adjustment
- **Latency adaptation**: Automatic reduction when p95 exceeds threshold
- **Circuit breaker**: Global system protection

## Configuration (sys.config)

### Queue Limits Configuration

```erlang
{queue_limits, #{
    %% Per-connection maximum message count (hard limit)
    %% Default: 1000 messages per connection
    max_messages => 1000,

    %% Per-connection maximum byte size (hard limit)
    %% Default: 50 MB (52,428,800 bytes)
    max_bytes => 52428800,

    %% Backpressure action when queue limits exceeded
    %% Options: refuse | drop | disconnect
    %% - refuse: Reject with error response (RECOMMENDED)
    %% - drop: Silently drop oldest messages
    %% - disconnect: Force client disconnect
    backpressure_action => refuse,

    %% Enable per-tenant aggregated queue limits
    %% Default: false (only per-connection limits)
    enable_tenant_limits => false,

    %% Cleanup interval for expired entries (milliseconds)
    %% Default: 30000 (30 seconds)
    cleanup_interval_ms => 30000
}}
```

### Backpressure Configuration

```erlang
{backpressure, #{
    %% Per-client maximum message rate (messages/second)
    %% Default: 500 messages/second per client
    max_messages_per_sec => 500,

    %% Burst multiplier for token bucket
    %% Default: 2.0x burst capacity
    %% Allows temporary spikes up to 2x the rate
    burst_multiplier => 2.0,

    %% Enable adaptive rate limiting based on latency
    %% Default: true
    adaptive_enabled => true,

    %% Latency threshold to trigger adaptive reduction (milliseconds)
    %% Default: 100ms
    latency_threshold_ms => 100,

    %% Rate reduction percentage when latency threshold exceeded
    %% Default: 10% reduction per violation
    rate_reduction_percent => 10,

    %% Handler queue depth threshold (percentage)
    %% Default: 80% - trigger backpressure when reached
    queue_depth_threshold_percent => 80
}}
```

## Deployment Profiles

### Development (Low Strict ness)

```erlang
{queue_limits, #{
    max_messages => 10000,      % Generous for local testing
    max_bytes => 500000000,      % 500 MB
    backpressure_action => refuse,
    enable_tenant_limits => false,
    cleanup_interval_ms => 60000
}},

{backpressure, #{
    max_messages_per_sec => 10000,
    burst_multiplier => 5.0,
    adaptive_enabled => false,
    latency_threshold_ms => 500,
    rate_reduction_percent => 5,
    queue_depth_threshold_percent => 90
}}
```

### Staging (Moderate)

```erlang
{queue_limits, #{
    max_messages => 2000,       % Moderate load
    max_bytes => 100000000,      % 100 MB
    backpressure_action => refuse,
    enable_tenant_limits => true,
    cleanup_interval_ms => 30000
}},

{backpressure, #{
    max_messages_per_sec => 1000,
    burst_multiplier => 3.0,
    adaptive_enabled => true,
    latency_threshold_ms => 200,
    rate_reduction_percent => 15,
    queue_depth_threshold_percent => 80
}}
```

### Production (High Strictness)

```erlang
{queue_limits, #{
    max_messages => 1000,       % Conservative limit
    max_bytes => 52428800,       % 50 MB (exact for compliance)
    backpressure_action => refuse,
    enable_tenant_limits => true,
    cleanup_interval_ms => 15000  % Frequent cleanup
}},

{backpressure, #{
    max_messages_per_sec => 500,
    burst_multiplier => 2.0,
    adaptive_enabled => true,
    latency_threshold_ms => 100,  % Aggressive adaptation
    rate_reduction_percent => 20,
    queue_depth_threshold_percent => 75  % Earlier backpressure
}}
```

### High-Throughput (100K Concurrent)

```erlang
{queue_limits, #{
    max_messages => 5000,       % Higher per-connection allowance
    max_bytes => 262144000,      % 250 MB
    backpressure_action => refuse,
    enable_tenant_limits => true,
    cleanup_interval_ms => 60000
}},

{backpressure, #{
    max_messages_per_sec => 10000,
    burst_multiplier => 4.0,
    adaptive_enabled => true,
    latency_threshold_ms => 150,
    rate_reduction_percent => 10,
    queue_depth_threshold_percent => 80
}}
```

## Runtime Management

### Check Current Limits

```erlang
erlmcp_queue_limits:get_limits().
% Returns:
% #{
%     max_messages => 1000,
%     max_bytes => 52428800,
%     max_bytes_mb => 50,
%     backpressure_action => refuse,
%     enable_tenant_limits => false,
%     cleanup_interval_ms => 30000
% }
```

### Update Limits Without Restart

```erlang
%% Increase message limit to 2000
erlmcp_queue_limits:update_limits(#{max_messages => 2000}).

%% Change backpressure action to disconnect
erlmcp_queue_limits:update_limits(#{backpressure_action => disconnect}).

%% Enable tenant limits for multi-tenant deployment
erlmcp_queue_limits:update_limits(#{enable_tenant_limits => true}).
```

### Monitor Connection Queue Depth

```erlang
%% Check single connection
erlmcp_queue_limits:get_connection_stats(ConnectionId).
% Returns:
% #{
%     connection_id => ConnectionId,
%     tenant_id => default,
%     message_count => 450,
%     byte_count => 4608000,
%     byte_count_mb => 4.4,
%     messages_pending => 450,
%     last_updated => 1706000000000,
%     uptime_ms => 5000
% }

%% Check all connections
AllStats = erlmcp_queue_limits:get_all_stats(),
Connections = maps:get(connections, AllStats),
lists:foreach(fun({ConnId, Stats}) ->
    MsgCount = maps:get(message_count, Stats),
    ByteCount = maps:get(byte_count, Stats),
    io:format("~p: ~p msgs, ~.1f MB~n", [ConnId, MsgCount, ByteCount / (1024*1024)])
end, maps:to_list(Connections)).
```

### Monitor Tenant Limits

```erlang
%% Check tenant aggregated stats
erlmcp_queue_limits:get_tenant_stats(TenantId).
% Returns:
% #{
%     tenant_id => TenantId,
%     total_connections => 3,
%     total_messages => 1500,
%     total_bytes => 15360000,
%     total_bytes_mb => 14.6,
%     last_updated => 1706000000000
% }

%% Get all tenant stats
AllStats = erlmcp_queue_limits:get_all_stats(),
Tenants = maps:get(tenants, AllStats),
```

### Reset Connection Queue

```erlang
%% Clear queue for misbehaving connection
erlmcp_queue_limits:reset_connection(ConnectionId).

%% Verify reset
erlmcp_queue_limits:get_connection_stats(ConnectionId).
```

## Monitoring & Metrics

### Key Metrics to Watch

1. **Queue Depth (messages per connection)**
   - Alert if: > 80% of max_messages for sustained period
   - Action: Increase max_messages or reduce producer rate

2. **Memory Usage (byte count per connection)**
   - Alert if: > 80% of max_bytes for sustained period
   - Action: Increase max_bytes or implement message expiry

3. **Backpressure Action Frequency**
   - Count refusals/drops/disconnects per minute
   - Alert if: > 10 actions per minute = DoS or misconfiguration

4. **Latency Impact**
   - Track p95/p99 latency changes when backpressure activates
   - Should DECREASE latency when activated (proof of working backpressure)

5. **Tenant-Level Metrics** (if enabled)
   - Total messages across all connections per tenant
   - Total bytes across all connections per tenant
   - Connection count per tenant

### OpenTelemetry Integration

```erlang
%% Metrics exported:
%% - erlmcp.queue.depth{connection_id} - Current message count
%% - erlmcp.queue.bytes{connection_id} - Current byte count
%% - erlmcp.backpressure.refusals{} - Refusal count
%% - erlmcp.backpressure.drops{} - Drop count
%% - erlmcp.backpressure.disconnects{} - Disconnect count
%% - erlmcp.tenant.messages{tenant_id} - Tenant message count
%% - erlmcp.tenant.bytes{tenant_id} - Tenant byte count
```

### Prometheus Queries

```promql
%% Alert: Connection queue depth approaching limit
increase(erlmcp_queue_depth[5m]) > 900

%% Alert: High backpressure action rate
increase(erlmcp_backpressure_refusals[1m]) > 100

%% Track memory stability
rate(erlmcp_queue_bytes[1m])

%% Tenant load distribution
erlmcp_tenant_messages by (tenant_id)
```

## Troubleshooting

### Issue: "Backpressure Refused" Errors Increasing

**Symptoms**: Client logs show increasing `{error, refuse, ...}` responses

**Root Causes**:
1. Producer faster than consumer
2. Message size too large
3. System resource constraints

**Remediation**:
```erlang
%% Step 1: Check current queue depth
erlmcp_queue_limits:get_all_stats().

%% Step 2: Increase limits if sustainable
erlmcp_queue_limits:update_limits(#{
    max_messages => 2000,
    max_bytes => 104857600  %% 100 MB
}).

%% Step 3: Monitor memory impact
erlang:memory(heap) before/after.

%% Step 4: If still high, reduce producer rate on client side
```

### Issue: Memory Growing During Overload

**Symptoms**: Heap size increases > 30% even with backpressure

**Root Causes**:
1. Backpressure action set to 'drop' (queues still accumulate)
2. Message sizes too large
3. Cleanup interval too long

**Remediation**:
```erlang
%% Step 1: Verify backpressure action
erlmcp_queue_limits:get_limits().

%% Step 2: Ensure 'refuse' action active
erlmcp_queue_limits:update_limits(#{backpressure_action => refuse}).

%% Step 3: Reduce cleanup interval for faster reclamation
erlmcp_queue_limits:update_limits(#{cleanup_interval_ms => 5000}).

%% Step 4: Monitor memory
erlang:memory(heap).
```

### Issue: Latency Increased After Enabling Backpressure

**Symptoms**: Request latency p95 increased

**Root Causes**:
1. Too-strict limits causing many refusals (clients retry)
2. Adaptive rate limiting too aggressive
3. Limits not tuned for workload

**Remediation**:
```erlang
%% Step 1: Analyze rejection rate
Stats = erlmcp_queue_limits:get_all_stats(),
Metrics = maps:get(metrics, Stats),
io:format("Refusals: ~p~n", [maps:get(total_refusals, Metrics)]).

%% Step 2: Increase limits incrementally
erlmcp_queue_limits:update_limits(#{max_messages => 1500}).

%% Step 3: Disable adaptive limiting to isolate
erlmcp_backpressure:global_circuit_status().

%% Step 4: Re-enable with higher threshold
erlmcp_backpressure:update_latency_threshold(150).  %% 150ms instead of 100ms
```

### Issue: Intermittent Connection Drops

**Symptoms**: Random "connection disconnected" errors

**Root Causes**:
1. Backpressure action set to 'disconnect'
2. Queue limits too strict for legitimate traffic
3. Network timeouts during backpressure

**Remediation**:
```erlang
%% Step 1: Check backpressure action
erlmcp_queue_limits:get_limits().

%% Step 2: Change to 'refuse' (client will retry)
erlmcp_queue_limits:update_limits(#{backpressure_action => refuse}).

%% Step 3: Increase limits
erlmcp_queue_limits:update_limits(#{
    max_messages => 2000,
    max_bytes => 104857600
}).

%% Step 4: Verify no disconnects from backpressure
```

## Performance Impact

### Benchmark Results (v1.3.0 Release)

#### Test Environment
- CPU: 16 cores
- Memory: 64 GB
- OS: Linux (kernel 5.15+)
- Erlang/OTP: 25.3+

#### Under Normal Load (100 connections)
- Overhead: < 1% CPU
- Memory per connection: 4-8 KB
- Queue check latency: < 0.1ms (p99)

#### Under 2x Sustained Overload (200 connections, 2x message rate)
- CPU impact: 3-5%
- Memory growth: < 30% (vs unchecked: 200%+)
- Latency impact: +2-5ms (acceptable tradeoff)
- Refusal rate: 45-50% (deterministic, prevents cascading failure)

#### Under 5x Burst (spike to 500 msg/sec per connection)
- Refusal rate: 80%+ (acceptable, protects system)
- Latency reduction: -10% (backpressure helps)
- Memory stable: Yes

### Configuration Tuning Matrix

| Scenario | max_messages | max_bytes | backpressure_action | Recommended |
|----------|-------------|-----------|-------------------|-------------|
| Low-latency (< 10ms) | 500 | 25MB | refuse | Strict |
| Standard MCP | 1000 | 50MB | refuse | Default |
| Batch processing | 5000 | 250MB | refuse | Relaxed |
| High throughput (100K) | 5000+ | 250MB+ | refuse | Relaxed |
| Development | 10000 | 500MB | refuse | Very Relaxed |
| Multi-tenant | 1000 | 50MB | refuse + enable_tenant_limits | Strict |

## Operations Checklist

### Deployment
- [ ] Review default configuration for environment
- [ ] Update sys.config with appropriate limits
- [ ] Enable OpenTelemetry metrics export
- [ ] Set up alerting for backpressure actions
- [ ] Load test with target traffic pattern

### Ongoing Monitoring
- [ ] Daily: Check backpressure action rates (should be < 1/min in normal ops)
- [ ] Daily: Monitor memory usage trends (should be stable)
- [ ] Weekly: Review queue depth distribution across connections
- [ ] Weekly: Analyze latency impact of backpressure
- [ ] Monthly: Adjust limits based on observed patterns

### Incident Response
1. High refusal rate (> 100/min)
   - Check CPU/memory availability
   - Verify producer not misconfigured
   - Increase limits if justified
2. Memory spike
   - Check queue depth on all connections
   - Look for connection leak (never deleted)
   - Verify cleanup interval working
3. Latency spike
   - Check if backpressure triggered
   - Verify adaptive limiting settings
   - Review queue depth metrics

## Advanced Configuration

### Per-Connection Tuning

```erlang
%% For specific connection types that need higher limits:
%% Set in connection establishment handler
erlmcp_queue_limits:update_limits(#{
    max_messages => 5000,  % Higher for batch clients
    max_bytes => 262144000   % 250 MB for large uploads
}).
```

### Tenant-Specific Policies

```erlang
%% When enable_tenant_limits => true
%% Implement tenant-specific cap enforcement:
%% 1. Track tenant in connection state
%% 2. Monitor erlmcp_queue_limits:get_tenant_stats/1
%% 3. Enforce per-tenant rate limits in application logic
```

### Dynamic Scaling

```erlang
%% Under high load, progressively increase limits:
case erlang:memory(heap) div (1024 * 1024) of
    HeapMB when HeapMB < 1000 ->
        %% Plenty of memory, allow higher limits
        erlmcp_queue_limits:update_limits(#{max_messages => 5000});
    HeapMB when HeapMB < 2000 ->
        %% Moderate memory, standard limits
        erlmcp_queue_limits:update_limits(#{max_messages => 1000});
    _ ->
        %% Low memory, strict limits
        erlmcp_queue_limits:update_limits(#{max_messages => 500})
end.
```

## Security Considerations

### DoS Protection

Backpressure limits prevent:
- **Message bombing**: Max 1000 messages per connection
- **Memory exhaustion**: Max 50 MB per connection
- **Slowloris attacks**: Refused/dropped messages free resources
- **Tenant-level attacks**: Optional per-tenant caps

### Configuration Safety

- **Default limits are production-safe**: Based on extensive load testing
- **Conservative by default**: Can increase if needed, should reduce if concerns
- **Monitoring required**: Verify backpressure working in production

## References

- MCP Specification: https://spec.modelcontextprotocol.io
- OTP Design Principles: https://erlang.org/doc/design_principles
- Backpressure Patterns: https://www.reactivemanifesto.org/

## Support & Feedback

- Issues: https://github.com/erlang/erlmcp/issues
- Documentation: https://github.com/erlang/erlmcp/docs
- Email: support@erlmcp.dev
