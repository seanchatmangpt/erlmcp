# erlmcp POC Master Integration Demo

## Overview

`erlmcp_poc_demo` is the "wow" demo that showcases all POC innovations working together in a realistic MCP workload simulation.

## What It Demonstrates

### 1. Circuit Breaker Pattern
- Aggressive failure detection (3 failures → open)
- Quick recovery (5s timeout)
- Realistic failure simulation (30% failure rate on flaky tool)
- Automatic trip tracking and reporting

### 2. Pub/Sub Resource Subscriptions
- 10 concurrent subscribers
- Dynamic resource updates
- Fan-out notifications (3-5 per subscriber)
- Subscription lifecycle management

### 3. Streaming Results (Simulated)
- Slow streaming tool with chunked delays
- 500ms total latency (5 x 100ms chunks)
- Realistic streaming behavior patterns

### 4. Leader Election (Simulated)
- Coordinated tool requiring single execution
- 5% of workload uses coordination
- Demonstrates distributed consensus need

### 5. Real-time Metrics
- Live metrics every 10 seconds
- Comprehensive final summary
- Per-agent statistics
- Latency percentiles (P50, P95, P99)

### 6. Realistic AI Agent Workload
- 10 concurrent agents
- Weighted tool selection:
  - 60% fast_tool (low latency)
  - 20% slow_stream_tool (streaming)
  - 15% flaky_tool (circuit breaker trigger)
  - 5% coordinated_tool (leader election)
- 2-5 second think time between calls
- Staggered startup (0-2s delay)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  erlmcp_poc_demo (orchestrator)             │
│  - Lifecycle management                                     │
│  - Metrics collection                                       │
│  - Summary reporting                                        │
└────────┬────────────────────────────────────────────────────┘
         │
         ├─► erlmcp_circuit_breaker
         │   - failure_threshold: 3
         │   - success_threshold: 2
         │   - timeout: 5000ms
         │
         ├─► erlmcp_metrics (observability)
         │   - Real-time collection
         │   - Aggregation
         │   - Performance summary
         │
         ├─► erlmcp_server (MCP protocol)
         │   ├── Tools:
         │   │   ├── fast_tool (0-50ms)
         │   │   ├── slow_stream_tool (500ms streaming)
         │   │   ├── flaky_tool (30% failure rate)
         │   │   └── coordinated_tool (200ms + coordination)
         │   │
         │   ├── Resources:
         │   │   ├── resource://static/data
         │   │   └── resource://dynamic/data (pub/sub)
         │   │
         │   └── Prompts:
         │       ├── simple_prompt
         │       └── topic_prompt (parameterized)
         │
         └─► 10 x Agent Processes
             - Subscribe to dynamic resources
             - Call tools with weighted distribution
             - Report latency and errors
             - Simulate realistic think time
```

## Usage

### Basic Run

```erlang
%% Start the demo with default settings (60 seconds)
erlmcp_poc_demo:run_full_demo().
```

### Custom Configuration

```erlang
%% Run with custom duration and metrics interval
erlmcp_poc_demo:run_full_demo(#{
    duration_sec => 120,        % Run for 2 minutes
    metrics_interval_ms => 5000 % Print metrics every 5 seconds
}).
```

### Early Stop

```erlang
%% Stop the demo early
erlmcp_poc_demo:stop().
```

## Sample Output

```
╔════════════════════════════════════════════════════════════════╗
║          erlmcp POC Master Integration Demo                   ║
║  Showcasing: Circuit Breaker, Pub/Sub, Streaming, Metrics    ║
╚════════════════════════════════════════════════════════════════╝

🚀 Starting core components...
  ✓ Circuit breaker started
  ✓ Metrics collector started
  ✓ MCP server started with tools, resources, and prompts

👥 Starting 10 AI agent clients...
  ✓ Agent #1 connected
  ✓ Agent #2 connected
  ...
  ✓ Agent #10 connected

▶ Demo running (60 seconds)...

📊 Metrics at T+10s:
  Tool Calls:           47
  Subscriptions:        10
  Pub/Sub Notifications: 8
  Errors:               7
  Circuit Breaker Trips: 2
  Avg Latency:          156ms

📊 Metrics at T+20s:
  Tool Calls:           94
  Subscriptions:        10
  Pub/Sub Notifications: 18
  Errors:               14
  Circuit Breaker Trips: 4
  Avg Latency:          162ms

...

⏹ Demo stopping, generating final report...

╔════════════════════════════════════════════════════════════════╗
║                    Final Summary Report                       ║
╚════════════════════════════════════════════════════════════════╝

📈 Overall Statistics:
  Duration:             60 seconds
  Agents:               10
  Total Tool Calls:     283
  Total Subscriptions:  10
  Pub/Sub Notifications: 42
  Total Errors:         42
  Circuit Breaker Trips: 12

⏱ Latency Statistics:
  Average:              158ms
  P50:                  102ms
  P95:                  487ms
  P99:                  521ms

👤 Per-Agent Statistics:
  Agent #1: 28 calls, 4 errors, 151ms avg
  Agent #2: 31 calls, 5 errors, 164ms avg
  Agent #3: 27 calls, 3 errors, 142ms avg
  ...
  Agent #10: 29 calls, 4 errors, 159ms avg

📡 Pub/Sub Fan-out:
  Subscribed Agents:    10
  Avg Notifications/Agent: 4.20

✅ Success Rate:        85.16%

🔌 Circuit Breaker:
  Total Trips:          12
  Errors Prevented:     36

✨ Demo showcased:
  ✓ Circuit breaker pattern with failure handling
  ✓ Pub/Sub resource subscriptions with fan-out
  ✓ Streaming tool results (simulated)
  ✓ Leader election for coordinated tools (simulated)
  ✓ Real-time metrics collection and reporting
  ✓ 10 concurrent AI agents with realistic workload

🧹 Cleaning up...
  ✓ All components stopped

Demo completed successfully!
```

## Metrics Explained

### Tool Calls
Total number of tool invocations across all agents. Expected: 250-300 in 60s with 10 agents.

### Subscriptions
Number of active resource subscriptions. Should be 10 (one per agent).

### Pub/Sub Notifications
Total notifications sent to subscribers. Each agent receives 3-5 notifications, so 30-50 total.

### Errors
Failed tool calls (primarily from flaky_tool). Expected: ~15% of total calls.

### Circuit Breaker Trips
Number of times circuit breaker opened due to failures. Expected: 10-15 trips in 60s.

### Latency Statistics
- **Average**: Mean latency across all tool calls
- **P50**: Median latency (50th percentile)
- **P95**: 95th percentile (slower calls)
- **P99**: 99th percentile (slowest calls)

Expected latencies:
- P50: ~100ms (most calls are fast_tool)
- P95: ~500ms (includes slow_stream_tool)
- P99: ~550ms (edge cases)

### Success Rate
Percentage of successful tool calls. Expected: 85-90% (due to 30% failure rate on 15% of calls).

## Code Structure

### Core Functions

#### `run_full_demo/0,1`
Entry point. Starts orchestrator gen_server, initializes components, spawns agents.

#### `start_circuit_breaker/0`
Creates circuit breaker with demo-tuned settings (aggressive tripping).

#### `start_mcp_server/0`
Starts MCP server with rich capabilities and demo tools/resources/prompts.

#### `start_agent/3`
Creates agent state. In production, would spawn erlmcp_client process.

#### `run_agent_workload/2`
Main agent loop. Subscribes to resources, calls tools with realistic patterns.

#### `print_current_metrics/1`
Prints live metrics every 10 seconds during demo execution.

#### `print_final_summary/1`
Comprehensive final report with all statistics and insights.

### Tool Implementations

#### `fast_tool`
- Latency: 0-50ms (random)
- Success rate: 100%
- Weight: 60% of calls

#### `slow_stream_tool`
- Latency: 500ms (simulated streaming: 5 x 100ms chunks)
- Success rate: 100%
- Weight: 20% of calls

#### `flaky_tool`
- Latency: 0-100ms (random)
- Success rate: 70%
- Weight: 15% of calls
- **Triggers circuit breaker**

#### `coordinated_tool`
- Latency: 200ms
- Success rate: 100%
- Weight: 5% of calls
- Simulates leader election requirement

### Resource Implementations

#### `resource://static/data`
Static JSON resource, never changes.

#### `resource://dynamic/data`
Dynamic resource with random values. Changes trigger pub/sub notifications.

## Integration Points

### Circuit Breaker
- Integrated via `erlmcp_circuit_breaker:call/2`
- Protects tool execution
- Reports trips to demo orchestrator

### Metrics
- `erlmcp_metrics:record_server_operation/4`
- Real-time collection
- Performance summary API

### Pub/Sub
- `erlmcp_server:subscribe_resource/3`
- `erlmcp_server:notify_resource_updated/3`
- Fan-out to multiple subscribers

## Extending the Demo

### Add New Tools

```erlang
add_custom_tools(ServerPid) ->
    CustomTool = fun(Args) ->
        % Your tool logic
        {ok, #{content => [...]}}
    end,
    erlmcp_server:add_tool_with_description(ServerPid, <<"custom_tool">>,
        CustomTool, <<"Description">>).
```

### Modify Agent Behavior

```erlang
run_full_demo(#{
    duration_sec => 120,
    agent_count => 20,  % Custom: would need code changes
    call_interval_ms => 1000  % Custom: would need code changes
}).
```

### Add Chaos Engineering

Integrate `erlmcp_chaos` to inject:
- Network delays
- Process kills
- Memory pressure
- Disk failures

## Performance Expectations

### Baseline (60s run, 10 agents)
- **Tool Calls**: 250-300
- **Success Rate**: 85-90%
- **Avg Latency**: 150-200ms
- **Circuit Breaker Trips**: 10-15
- **Pub/Sub Notifications**: 30-50

### Degraded (high failure rate)
If flaky_tool failure rate increased to 50%:
- **Success Rate**: 75-80%
- **Circuit Breaker Trips**: 20-25
- **More errors**: 50-60

### Optimized (no failures)
If flaky_tool removed:
- **Success Rate**: 100%
- **Circuit Breaker Trips**: 0
- **Avg Latency**: 120-150ms (no recovery overhead)

## Testing

```erlang
%% Run demo as smoke test
1> erlmcp_poc_demo:run_full_demo(#{duration_sec => 10}).
%% Should complete without crashes

%% Verify metrics collection
2> erlmcp_metrics:get_performance_summary().
#{...}

%% Check circuit breaker stats
3> erlmcp_circuit_breaker:get_all_stats().
[...]
```

## Future Enhancements

1. **Real Transport Integration**: Replace mock clients with real erlmcp_client over stdio/tcp/http
2. **Leader Election**: Implement real distributed consensus for coordinated_tool
3. **Streaming**: Integrate actual streaming result protocol
4. **Chaos Engineering**: Add fault injection during demo
5. **Grafana Dashboards**: Export metrics to Prometheus/Grafana
6. **Load Testing**: Scale to 100+ agents for stress testing
7. **Multi-Node**: Demonstrate clustering with multiple erlmcp nodes

## Troubleshooting

### Demo Hangs
- Check if erlmcp_metrics is running: `whereis(erlmcp_metrics)`
- Verify circuit breaker: `whereis(erlmcp_circuit_breaker)`
- Stop and restart: `erlmcp_poc_demo:stop()`

### Low Tool Call Count
- Agents may not be spawning: check logs
- Server may have crashed: check `erlmcp_server` process
- Increase duration: `run_full_demo(#{duration_sec => 120})`

### No Metrics Printed
- Observability app not started: `application:ensure_all_started(erlmcp_observability)`
- Metrics interval too long: reduce `metrics_interval_ms`

### Circuit Breaker Not Tripping
- Failure rate too low: increase flaky_tool percentage
- Threshold too high: lower `failure_threshold` in config
- Not enough calls: increase agent count or duration

## License

Same as erlmcp (Apache 2.0)

## See Also

- `/docs/CIRCUIT_BREAKER.md` - Circuit breaker pattern details
- `/docs/PUBSUB.md` - Pub/sub architecture
- `/docs/protocol.md` - MCP protocol specification
- `/apps/erlmcp_observability/` - Metrics and monitoring
