# Simple Metrics Usage Guide

The `erlmcp_simple_metrics` module provides a lightweight, ETS-based metrics system with zero external dependencies.

## Quick Start

```erlang
% Start the metrics system
ok = erlmcp_simple_metrics:start().

% Track requests
erlmcp_simple_metrics:increment(request_count, 1).
erlmcp_simple_metrics:increment(error_count, 1).

% Record latencies
erlmcp_simple_metrics:record_latency(json_decode, 15.3).
erlmcp_simple_metrics:record_latency(transport_send, 42.1).

% Get all statistics
Stats = erlmcp_simple_metrics:get_stats().

% Reset all metrics
erlmcp_simple_metrics:reset().

% Stop the system
erlmcp_simple_metrics:stop().
```

## Core API

### Starting and Stopping

- `start/0` - Initialize the ETS table and counters
- `stop/0` - Clean shutdown and delete the ETS table
- `reset/0` - Clear all metrics but keep the system running

### Recording Metrics

- `increment(Metric, Count)` - Increment a counter by Count
- `record_latency(Operation, Latency)` - Record a latency measurement in milliseconds

### Retrieving Data

- `get_stats/0` - Get all current statistics as a map

## Convenience Functions

For common patterns:

- `request/0` - Increment request_count by 1
- `error/0` - Increment error_count by 1  
- `success/0` - Increment success_count by 1

## Example Output

```erlang
Stats = erlmcp_simple_metrics:get_stats().
% Returns:
#{
    counters => #{
        request_count => 10,
        error_count => 2,
        success_count => 8
    },
    latencies => #{
        json_decode => #{
            count => 5,
            sum_ms => 87,
            min_ms => 12.3,
            max_ms => 22.1,
            avg_ms => 17.4
        }
    },
    system => #{
        uptime_ms => 45231,
        memory_total => 45123456,
        process_count => 42
    },
    timestamp => 1692123456789
}
```

## Key Features

- **Zero Dependencies**: Uses only ETS and standard Erlang
- **Thread Safe**: ETS operations are atomic and concurrent
- **Memory Efficient**: Automatic cleanup of old latency measurements
- **Fast**: Direct ETS operations, no gen_server overhead for reads
- **Simple**: Clean API focusing on the 80/20 rule

## Memory Management

- Latency measurements are automatically cleaned up (keeps last 1000 per operation)
- Counters and aggregates persist until reset or stop
- ETS table uses `{write_concurrency, true}` for performance

## Integration

```erlang
% In your application startup:
erlmcp_simple_metrics:start(),

% In request handlers:
erlmcp_simple_metrics:request(),
Start = erlang:system_time(millisecond),
Result = do_work(),
End = erlang:system_time(millisecond),
erlmcp_simple_metrics:record_latency(work_duration, End - Start),
case Result of
    {ok, _} -> erlmcp_simple_metrics:success();
    {error, _} -> erlmcp_simple_metrics:error()
end.
```

## Demo

Run the included demo:

```bash
escript examples/simple_metrics_demo.erl
```

This will simulate API requests and show real-time metrics output.