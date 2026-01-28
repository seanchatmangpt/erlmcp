# Backpressure Quick Start Guide

## 30-Second Summary

erlmcp v1.3.0 implements hard queue limits (1000 messages / 50 MB per connection) with deterministic backpressure to prevent memory exhaustion and latency spirals. When limits exceeded, clients receive `{error, refuse}` response instead of silently dropping messages or crashing the system.

## Quick Configuration

### Default (Production-Safe)
Already configured in `config/sys.config`. No changes needed.

### Development (Relaxed)
```erlang
{queue_limits, #{max_messages => 10000, max_bytes => 500000000}}
```

### High-Load (Strict)
```erlang
{queue_limits, #{max_messages => 500, max_bytes => 26214400}}
```

## Check Status

```bash
# Start Erlang shell with config
erl -config config/sys

% Inside Erlang:
erlmcp_queue_limits:start_link().
erlmcp_queue_limits:get_limits().
erlmcp_queue_limits:get_all_stats().
```

## Key Metrics

- **queue_depth**: Messages buffered per connection (alert if > 900)
- **byte_count**: Bytes buffered per connection (alert if > 45 MB)
- **total_refusals**: Count of backpressure actions
- **memory_growth**: Should be < 30% under 2x overload

## Troubleshooting

### Problem: Too Many Refusals
**Solution**: Increase limits or reduce producer rate
```erlang
erlmcp_queue_limits:update_limits(#{max_messages => 2000}).
```

### Problem: Memory Growing
**Solution**: Verify backpressure is active (action => refuse)
```erlang
erlmcp_queue_limits:get_limits().
```

### Problem: Latency High
**Solution**: This is expected initially. Backpressure reduces latency after initial period.

## Test It

```bash
# Run comprehensive test suite
rebar3 ct --suite=erlmcp_backpressure_SUITE

# Expected: 12/12 tests PASS
```

## API Cheat Sheet

```erlang
% Check if message can be queued
erlmcp_queue_limits:check_queue_limit(ConnId, MsgSize).
% Returns: {ok, accepted} | {error, refuse|drop|disconnect, Details}

% Track enqueued message
erlmcp_queue_limits:record_message(ConnId, MsgId, Size).

% Untrack dequeued message
erlmcp_queue_limits:remove_message(ConnId, MsgId).

% View connection stats
erlmcp_queue_limits:get_connection_stats(ConnId).

% Update limits at runtime
erlmcp_queue_limits:update_limits(#{max_messages => 2000}).

% Get all metrics
erlmcp_queue_limits:get_all_stats().
```

## Monitoring (Prometheus)

```promql
# Alert on high refusals
increase(erlmcp_backpressure_refusals[1m]) > 100

# Alert on queue near limit
erlmcp_queue_depth > 900
```

## References

- Full guide: `docs/operations/backpressure_config.md`
- Implementation: `BACKPRESSURE_IMPLEMENTATION.md`
- Tests: `test/erlmcp_backpressure_SUITE.erl`
