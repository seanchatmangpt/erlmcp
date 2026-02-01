# MCP Features Benchmark

## Overview

The `erlmcp_bench_mcp_features` module provides comprehensive performance benchmarks for MCP protocol features:

1. **Tool Call Latency** - Measure tool invocation pipeline performance
2. **Resource Subscription Overhead** - Measure subscription management and notification latency
3. **Prompt Template Rendering** - Measure template compilation and rendering performance
4. **Sampling Operations** - Measure LLM sampling request handling

## Quick Start

```erlang
%% Run all benchmarks
erlmcp_bench_mcp_features:run_all().

%% Run specific workload
erlmcp_bench_mcp_features:run(<<"tool_call_simple_1k">>).

%% List available workloads
erlmcp_bench_mcp_features:workloads().
```

## Benchmark Categories

### 1. Tool Call Latency

Measures the end-to-end latency of tool invocations including JSON-RPC encoding/decoding overhead.

**Complexity Levels:**
- **Simple**: 1 parameter (echo tool)
- **Medium**: 5 parameters (data processing)
- **Complex**: 20 parameters (complex data processing)

**Workloads:**
- `tool_call_simple_100` - 100 simple tool calls
- `tool_call_simple_1k` - 1,000 simple tool calls
- `tool_call_simple_10k` - 10,000 simple tool calls
- `tool_call_medium_100` - 100 medium complexity tool calls
- `tool_call_medium_1k` - 1,000 medium complexity tool calls
- `tool_call_complex_100` - 100 complex tool calls

**Metrics:**
- Latency (p50, p95, p99) in microseconds
- Throughput (operations/sec)
- Memory delta (MiB)
- Tool parameter count

**Example:**
```erlang
erlmcp_bench_mcp_features:run(<<"tool_call_simple_1k">>).
```

### 2. Resource Subscription Overhead

Measures the overhead of managing resource subscriptions and delivering notifications.

**Configuration:**
- **Subscribers**: 1, 10, 100, 1000 concurrent subscribers
- **Notification Rate**: 1Hz, 10Hz (notifications per second)
- **Duration**: 10 seconds

**Workloads:**
- `subscription_1sub_1hz` - 1 subscriber, 1 notification/sec, 10 seconds
- `subscription_10sub_10hz` - 10 subscribers, 10 notifications/sec, 10 seconds
- `subscription_100sub_10hz` - 100 subscribers, 10 notifications/sec, 10 seconds
- `subscription_1000sub_1hz` - 1000 subscribers, 1 notification/sec, 10 seconds

**Metrics:**
- Notification latency (p50, p95, p99) in microseconds
- Subscribe/unsubscribe latency
- CPU overhead percentage
- Memory per subscriber

**Example:**
```erlang
erlmcp_bench_mcp_features:run(<<"subscription_100sub_10hz">>).
```

### 3. Prompt Template Rendering

Measures template compilation and rendering performance using Mustache syntax.

**Complexity Levels:**
- **Simple**: Static template, no variables
- **Medium**: 10 template variables
- **Complex**: 50 template variables

**Workloads:**
- `prompt_simple_100` - 100 renders, no variables
- `prompt_simple_1k` - 1,000 renders, no variables
- `prompt_simple_10k` - 10,000 renders, no variables
- `prompt_medium_100` - 100 renders, 10 variables
- `prompt_medium_1k` - 1,000 renders, 10 variables
- `prompt_complex_100` - 100 renders, 50 variables

**Metrics:**
- Render latency (p50, p95, p99) in microseconds
- Template size (bytes)
- Variable count
- Throughput (renders/sec)

**Example:**
```erlang
erlmcp_bench_mcp_features:run(<<"prompt_medium_1k">>).
```

### 4. Sampling Operations

Measures LLM sampling request handling performance using mock provider (no network calls).

**Strategies:**
- **Random**: Basic random sampling
- **Temperature**: Temperature-based sampling (temperature=0.7)
- **Top-K**: Top-K sampling (k=50)

**Workloads:**
- `sampling_random_100` - 100 random sampling requests
- `sampling_random_1k` - 1,000 random sampling requests
- `sampling_temperature_100` - 100 temperature sampling requests
- `sampling_top_k_100` - 100 top-k sampling requests

**Metrics:**
- Request latency (p50, p95, p99) in microseconds
- Success rate
- Throughput (requests/sec)
- Strategy parameters

**Example:**
```erlang
erlmcp_bench_mcp_features:run(<<"sampling_random_1k">>).
```

## Output Format

All benchmarks produce metrology-compliant JSON output in `bench/results/`:

```json
{
  "workload_id": "tool_call_simple_1k",
  "benchmark": "mcp_features",
  "category": "tool_calls",
  "timestamp": 1738304400,
  "environment": {
    "hostname": "benchmark-host",
    "erlang_version": "OTP-27",
    "os": "linux"
  },
  "operations": 1000,
  "duration_s": 0.52,
  "throughput_msg_per_s": 1923.08,
  "latency_p50_us": 45.2,
  "latency_p95_us": 92.1,
  "latency_p99_us": 156.7,
  "precision": "microsecond",
  "memory_start_mib": 48.2,
  "memory_end_mib": 48.5,
  "memory_delta_mib": 0.3,
  "cpu_percent_avg": 52.3,
  "scope": "per_node",
  "details": {
    "complexity": "simple",
    "tool_name": "echo_simple",
    "param_count": 1
  }
}
```

## Performance Baselines

Expected performance on a typical development machine (OTP 27, 8 cores):

### Tool Calls
- Simple (1 param): p50=40-60µs, throughput=15-20K ops/sec
- Medium (5 params): p50=100-150µs, throughput=6-8K ops/sec
- Complex (20 params): p50=250-350µs, throughput=2-3K ops/sec

### Resource Subscriptions
- 1 subscriber: p50=20-30µs per notification
- 100 subscribers: p50=50-80µs per notification
- 1000 subscribers: p50=200-300µs per notification

### Prompt Rendering
- Simple (static): p50=5-10µs, throughput=100-200K renders/sec
- Medium (10 vars): p50=30-50µs, throughput=20-30K renders/sec
- Complex (50 vars): p50=150-250µs, throughput=4-6K renders/sec

### Sampling Operations
- Mock provider: p50=100-200µs, throughput=5-10K req/sec
- (Real providers will be 100-1000x slower due to network latency)

## Usage Patterns

### Continuous Integration

```bash
# Run all benchmarks and check for regressions
make benchmark

# Run strict benchmark with regression detection
make benchmark-strict
```

### Development Workflow

```erlang
%% 1. Start Erlang shell
make console

%% 2. Run specific benchmark
erlmcp_bench_mcp_features:run(<<"tool_call_simple_1k">>).

%% 3. Check results
% Results written to: bench/results/mcp_features_tool_call_simple_1k_<timestamp>.json

%% 4. Compare with baseline
% Use scripts/bench/compare_results.sh to compare with previous runs
```

### Regression Detection

```bash
# Compare current run with baseline
./scripts/bench/compare_results.sh \
  bench/results/mcp_features_tool_call_simple_1k_<baseline>.json \
  bench/results/mcp_features_tool_call_simple_1k_<current>.json
```

## Implementation Details

### Real MCP Processes

All benchmarks use **real** erlmcp processes:
- `erlmcp_server` for tool calls
- `erlmcp_resource_subscriptions` for subscriptions
- `erlmcp_prompt_template` for rendering
- `erlmcp_sampling` with mock provider for sampling

**No mocks, fakes, or placeholder implementations** (Chicago TDD compliance).

### Metrology Compliance

All output follows the erlmcp metrology standard:
- Canonical units: `throughput_msg_per_s`, `latency_p50_us`, `memory_delta_mib`
- Required fields: workload_id, timestamp, environment, operations, duration_s
- Precision tracking: microsecond precision for all latency measurements
- Scope indicators: `per_node`, `per_connection`, etc.

### Memory Measurement

Memory is measured before and after each benchmark:
- `memory_start_mib` - Total memory before benchmark
- `memory_end_mib` - Total memory after benchmark
- `memory_delta_mib` - Memory growth during benchmark

### Percentile Calculation

Latencies are sorted and percentiles calculated:
- p50 (median) - 50th percentile
- p95 - 95th percentile
- p99 - 99th percentile

## Troubleshooting

### Benchmark Fails to Start

```erlang
%% Ensure erlmcp application is started
application:ensure_all_started(erlmcp).

%% Then run benchmark
erlmcp_bench_mcp_features:run(<<"tool_call_simple_1k">>).
```

### High Latency in Tool Calls

Tool call latency includes JSON-RPC encoding/decoding overhead. Expected overhead is 5-10% of total latency.

### Resource Subscription Notifications Not Received

Ensure `erlmcp_resource_subscriptions` server is running:
```erlang
whereis(erlmcp_resource_subscriptions).
%% If undefined, start it:
erlmcp_resource_subscriptions:start_link().
```

### Sampling Benchmark Fails

Ensure mock LLM provider is available:
```erlang
%% Check if mock provider module exists
code:which(erlmcp_mock_llm).

%% Manually set provider
erlmcp_sampling:set_model_provider(erlmcp_sampling, erlmcp_mock_llm).
```

## Contributing

When adding new benchmarks:

1. Follow existing workload naming: `<category>_<config>_<scale>`
2. Ensure metrology compliance (validate_report/1)
3. Use real MCP processes (no mocks)
4. Document expected performance baselines
5. Add workload to workloads/0 function

## References

- Main benchmark suite: `apps/erlmcp_core/test/erlmcp_bench_core_ops.erl`
- Integration benchmarks: `apps/erlmcp_core/test/erlmcp_bench_integration.erl`
- Metrology guide: `docs/metrology/METRICS_GLOSSARY.md`
- CLAUDE.md: Project development guide
