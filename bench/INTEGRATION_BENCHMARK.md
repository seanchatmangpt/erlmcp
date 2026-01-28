# ERLMCP Integration Benchmark Documentation

## Overview

The integration benchmark (`erlmcp_bench_integration.erl`) measures end-to-end MCP protocol workflow performance. It benchmarks complete workflows from initialization through tool calls, prompts, resources, and shutdown.

## Purpose

- **Measure realistic workflow latency**: Complete MCP protocol interactions
- **Protocol overhead analysis**: JSON-RPC encoding/decoding costs
- **Workflow success rates**: Validate protocol compliance
- **Throughput measurement**: Workflows per second capacity
- **Step-by-step breakdown**: Identify bottlenecks in workflow sequences

## Quick Start

### Run All Benchmarks

```bash
cd /Users/sac/erlmcp
bash bench/run_integration_bench.sh
```

**Duration**: ~2-3 minutes for all workflows

### Run Specific Workflow

```erlang
%% Start Erlang shell
cd /Users/sac/erlmcp
rebar3 shell

%% Run single workflow
Workflow = #{
    id => <<"my_test">>,
    steps => [initialize, list_tools, call_tool, shutdown],
    iterations => 100
},
erlmcp_bench_integration:benchmark_workflow(Workflow).
```

## Workflow Definitions

### 1. Basic Initialize (`mcp_basic_initialize`)

```erlang
Steps: [initialize, list_tools, shutdown]
Iterations: 100
Purpose: Minimal MCP protocol overhead measurement
```

**What it tests**:
- Initialization handshake
- Capability negotiation
- Tool discovery
- Clean shutdown

**Expected metrics**:
- E2E latency (p50): < 5ms
- E2E latency (p99): < 15ms
- Success rate: 100%
- Throughput: > 1000 workflows/s

### 2. Tool Sequence (`mcp_tool_sequence`)

```erlang
Steps: [initialize, list_tools, call_tool, call_tool, call_tool, shutdown]
Iterations: 100
Purpose: Measure tool call overhead
```

**What it tests**:
- Multiple tool calls in sequence
- JSON-RPC encoding/decoding for tool arguments
- Tool handler execution time
- Request correlation

**Expected metrics**:
- E2E latency (p50): < 10ms
- E2E latency (p99): < 30ms
- Per-tool-call overhead: ~2-3ms

### 3. Prompts Workflow (`mcp_prompts_workflow`)

```erlang
Steps: [initialize, list_prompts, get_prompt, shutdown]
Iterations: 100
Purpose: Prompt capability testing
```

**What it tests**:
- Prompt discovery
- Prompt argument handling
- Prompt message generation
- Response encoding

**Expected metrics**:
- E2E latency (p50): < 8ms
- E2E latency (p99): < 25ms

### 4. Resources Workflow (`mcp_resources_workflow`)

```erlang
Steps: [initialize, list_resources, read_resource, shutdown]
Iterations: 100
Purpose: Resource capability testing
```

**What it tests**:
- Resource discovery
- Resource content retrieval
- URI handling
- Content encoding

**Expected metrics**:
- E2E latency (p50): < 8ms
- E2E latency (p99): < 25ms

### 5. Complete Workflow (`mcp_complete_workflow`)

```erlang
Steps: [initialize, list_tools, call_tool, list_prompts, get_prompt,
        list_resources, read_resource, shutdown]
Iterations: 50
Purpose: Full MCP capability exercise
```

**What it tests**:
- All MCP capabilities in sequence
- Protocol state management
- Capability interaction
- End-to-end integration

**Expected metrics**:
- E2E latency (p50): < 20ms
- E2E latency (p99): < 50ms
- Success rate: 100%

## Metrics Explained

### Workflow-Level Metrics

| Metric | Description | Interpretation |
|--------|-------------|----------------|
| `iterations` | Number of workflow runs | Higher = more confident percentiles |
| `success_rate_percent` | % of successful completions | Should be 100% for stable protocols |
| `failures` | Count of failed workflows | Protocol errors, timeouts |
| `timeouts` | Count of timeout errors | Indicates slow operations |
| `total_duration_s` | Wall-clock time | Includes all overhead |
| `throughput_workflows_per_s` | Workflows per second | System capacity indicator |
| `latency_e2e_p50_ms` | Median end-to-end time | Typical workflow duration |
| `latency_e2e_p95_ms` | 95th percentile latency | Most workflows under this |
| `latency_e2e_p99_ms` | 99th percentile latency | Tail latency indicator |

### Step-Level Metrics

Each workflow step reports:

```json
{
  "initialize": {
    "p50_ms": 1.2,
    "p95_ms": 2.5,
    "p99_ms": 4.0,
    "mean_ms": 1.5,
    "min_ms": 0.8,
    "max_ms": 6.2
  }
}
```

| Metric | Description |
|--------|-------------|
| `p50_ms` | Median step duration |
| `p95_ms` | 95th percentile step duration |
| `p99_ms` | 99th percentile step duration |
| `mean_ms` | Average step duration |
| `min_ms` | Fastest observed step duration |
| `max_ms` | Slowest observed step duration |

### Protocol Overhead

`protocol_overhead_percent`: Estimated JSON-RPC encoding/decoding overhead

- **5-8%**: Excellent (optimized JSON encoding)
- **8-12%**: Good (standard JSON-RPC)
- **>15%**: Investigate encoding performance

## Output Format

### JSON Export

Results are saved to `bench/results/integration_<timestamp>.json`:

```json
[
  {
    "workload_id": "mcp_tool_sequence",
    "benchmark": "integration",
    "workflow": "mcp_tool_sequence",
    "iterations": 100,
    "success_rate_percent": 100.0,
    "failures": 0,
    "timeouts": 0,
    "total_duration_s": 1.234,
    "throughput_workflows_per_s": 810.3,
    "latency_e2e_p50_ms": 9.8,
    "latency_e2e_p95_ms": 18.5,
    "latency_e2e_p99_ms": 25.3,
    "steps": {
      "initialize": {"p50_ms": 1.2, "p95_ms": 2.1, ...},
      "list_tools": {"p50_ms": 0.8, "p95_ms": 1.5, ...},
      "call_tool": {"p50_ms": 3.2, "p95_ms": 5.8, ...},
      "shutdown": {"p50_ms": 0.3, "p95_ms": 0.6, ...}
    },
    "protocol_overhead_percent": 8.5,
    "scope": "per_node",
    "precision": "microsecond"
  }
]
```

### Console Output

```
=== ERLMCP INTEGRATION BENCHMARK SUITE ===

Starting comprehensive MCP protocol workflow benchmarks...

--- Benchmarking workflow: mcp_basic_initialize ---
Steps: [initialize,list_tools,shutdown]
Iterations: 100
Warming up...
Running benchmark...

--- Benchmarking workflow: mcp_tool_sequence ---
...

=== BENCHMARK SUMMARY ===

Workflow: mcp_basic_initialize
  Iterations: 100
  Success Rate: 100.00%
  Throughput: 1234.56 workflows/s
  E2E Latency (p50): 4.23 ms
  E2E Latency (p99): 12.45 ms

...

Results exported to: bench/results/integration_1738012345.json
```

## Implementation Details

### Benchmark Approach

The integration benchmark uses a **protocol-focused** approach:

1. **Setup Phase**: Create MCP server with test tools/prompts/resources
2. **Warmup**: Run 10 iterations to warm up the system
3. **Measurement**: Execute workflow steps and measure latency
4. **Cleanup**: Stop server and calculate metrics
5. **Export**: Save results to JSON

### What is Measured

Each workflow step measures:
- **JSON-RPC encoding**: Time to encode request
- **JSON-RPC decoding**: Time to decode request
- **Protocol validation**: Schema validation overhead
- **Handler execution**: (Future: actual server calls)

### What is NOT Measured

- Network transport overhead (use transport benchmarks for this)
- Database access (not included in test tools)
- External service calls (test tools are self-contained)
- Concurrent client overhead (single-threaded execution)

## Customization

### Add Custom Workflow

```erlang
%% Edit bench/erlmcp_bench_integration.erl

workflows() ->
    [
        ... existing workflows ...,

        #{
            id => <<"my_custom_workflow">>,
            description => <<"Custom workflow description">>,
            steps => [initialize, custom_step, shutdown],
            iterations => 100
        }
    ].

%% Add custom step handler
execute_step(custom_step, ServerPid) ->
    %% Your custom logic here
    {ok, result}.
```

### Adjust Iterations

For longer/shorter benchmarks:

```erlang
%% Quick test (10 iterations per workflow)
workflows() ->
    [
        #{id => <<"mcp_basic_initialize">>, steps => [...], iterations => 10},
        ...
    ].

%% Production benchmark (1000 iterations)
workflows() ->
    [
        #{id => <<"mcp_basic_initialize">>, steps => [...], iterations => 1000},
        ...
    ].
```

## Performance Targets

### Acceptable Performance

| Metric | Target | Notes |
|--------|--------|-------|
| Success Rate | 100% | No protocol errors |
| P50 Latency | < 10ms | Half of workflows |
| P99 Latency | < 30ms | 99% of workflows |
| Throughput | > 500 workflows/s | Per core |
| Protocol Overhead | < 10% | JSON-RPC efficiency |

### Warning Thresholds

| Metric | Threshold | Action |
|--------|-----------|--------|
| Success Rate | < 99% | Investigate protocol errors |
| P99 Latency | > 50ms | Check step-level metrics |
| Throughput | < 100 workflows/s | Profile CPU/memory |
| Protocol Overhead | > 15% | Optimize JSON encoding |

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Run Integration Benchmarks
  run: |
    bash bench/run_integration_bench.sh

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: integration-benchmark-results
    path: bench/results/integration_*.json
```

### Regression Detection

Compare against baseline:

```bash
# Save baseline
cp bench/results/integration_latest.json bench/baseline/integration_baseline.json

# Compare after changes
diff -u <(jq '.[] | {workflow, latency_e2e_p50_ms}' bench/baseline/integration_baseline.json) \
        <(jq '.[] | {workflow, latency_e2e_p50_ms}' bench/results/integration_latest.json)
```

## Troubleshooting

### Low Success Rate

**Symptoms**: `success_rate_percent < 100%`

**Causes**:
- Protocol validation errors
- Tool handler exceptions
- Timeout issues

**Debug**:
```erlang
%% Enable debug logging
logger:set_primary_config(level, debug).
erlmcp_bench_integration:benchmark_workflow(Workflow).
```

### High Latency

**Symptoms**: `latency_e2e_p99_ms > 50ms`

**Causes**:
- Step-level bottlenecks
- Memory pressure
- CPU contention

**Debug**:
```erlang
%% Check step-level metrics
Result = erlmcp_bench_integration:benchmark_workflow(Workflow),
maps:get(steps, Result).
```

### Low Throughput

**Symptoms**: `throughput_workflows_per_s < 100`

**Causes**:
- Server setup overhead
- JSON encoding slowness
- Memory allocation

**Debug**:
```erlang
%% Profile single workflow
{Time, _Result} = timer:tc(fun() ->
    erlmcp_bench_integration:run_single_workflow(Workflow)
end),
io:format("Single workflow time: ~p us~n", [Time]).
```

## Related Benchmarks

- **Latency Suite** (`bench/latency_SUITE.erl`): Detailed latency distribution analysis
- **Throughput Suite** (`bench/throughput_SUITE.erl`): High-throughput stress testing
- **Transport Benchmarks** (`bench/transport_real/`): Network transport overhead
- **Registry Benchmarks** (`bench/erlmcp_registry_contention.erl`): Message routing performance

## References

- MCP Protocol Specification: https://modelcontextprotocol.io/specification/
- JSON-RPC 2.0 Spec: https://www.jsonrpc.org/specification
- erlmcp Architecture: `docs/architecture.md`
- OTP Patterns: `docs/otp-patterns.md`
