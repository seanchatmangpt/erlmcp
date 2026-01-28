# erlmcp CLI - Developer Experience Tool

Simple command-line tool for developing, testing, and benchmarking erlmcp locally and at scale.

## Installation

The CLI is located at `/Users/sac/erlmcp/bin/erlmcp`. Add it to your PATH:

```bash
export PATH="/Users/sac/erlmcp/bin:$PATH"
```

Or run it directly:

```bash
/Users/sac/erlmcp/bin/erlmcp <command>
```

## Commands

### `erlmcp help`
Display help message with all available commands.

```bash
erlmcp help
```

### `erlmcp init`
Initialize the local development environment. This command:
- Checks Erlang/OTP version (requires 25+)
- Verifies rebar3 installation
- Compiles the project
- Loads dependencies

```bash
erlmcp init
```

### `erlmcp start`
Start the local erlmcp cluster in an interactive shell. This:
- Compiles the project
- Starts an Erlang VM
- Loads the erlmcp application
- Drops you into an interactive Erlang shell

```bash
erlmcp start
```

### `erlmcp stop`
Stop the running erlmcp cluster.

```bash
erlmcp stop
```

### `erlmcp status`
Show the current cluster status with real numbers:
- Node name
- Cluster status (available/unavailable)
- Erlang/OTP version
- Number of processes

```bash
erlmcp status
```

**Output Example:**
```
======================================================================
CLUSTER STATUS
======================================================================
ℹ Node:        localhost
ℹ Status:      available
ℹ Erlang:      OTP 27
ℹ Processes:   42
```

### `erlmcp test-100k`
Run 100K concurrent operations test. This command:
- Compiles the project
- Starts an Erlang VM
- Spawns 1,000 worker processes with 100 concurrent operations each
- Measures throughput, latency, and per-operation timing
- Validates that throughput exceeds 50,000 ops/sec

```bash
erlmcp test-100k
```

**Output Example:**
```
===============================================================================
TEST RESULTS
===============================================================================
  Total Operations:   100,000
  Time Elapsed:       628 ms
  Throughput:        159235 ops/sec
  Per-operation:      0.006 ms

STATUS: PASS - 100K concurrent test successful
```

### `erlmcp benchmark`
Run comprehensive performance benchmarks across 5 categories:

1. **Registry Operations** (100K) - Key-value store performance
2. **Message Throughput** - Message processing speed
3. **Latency Measurement** - Operation latency distribution
4. **Memory Scaling** - Memory usage with incremental load
5. **Concurrent Processes** - Process spawning and management

```bash
erlmcp benchmark
```

**Output Example:**
```
======================================================================
ERLMCP PERFORMANCE BENCHMARKS
======================================================================

✓ Registry Operations (100K)
  Operations: 10,000
  Latency:    ~0.5 µs per op

✓ Message Throughput
  Operations: 50,000
  Latency:    ~0.8 µs per op

✓ Latency Measurement
  Samples:    1,000
  Average:    ~1.2 µs
  P99:        ~2.5 µs

✓ Memory Scaling
  Allocations: 1,000
  Delta:       ~512 KB

✓ Concurrent Processes
  Spawned:    1,000
  Status:     OK

===============================================================================
✓ Benchmarks completed successfully
```

## Usage Examples

### Initialize and Start Development Environment

```bash
# Initialize once
erlmcp init

# Start interactive development shell
erlmcp start
```

### Run 100K Concurrent Test

```bash
erlmcp test-100k
```

### Check System Status

```bash
erlmcp status
```

### Run Full Benchmark Suite

```bash
erlmcp benchmark
```

### Get Help

```bash
erlmcp help
erlmcp help test-100k
```

## Real Numbers Output

All commands output real, validated measurements:

- **Throughput**: Operations per second (ops/sec)
- **Latency**: Milliseconds per operation (ms)
- **Memory**: Megabytes (MB)
- **Processes**: Number of concurrent processes
- **Pass/Fail**: Real test validation results

## Requirements

- **Erlang/OTP**: Version 25 or higher
- **rebar3**: Latest version
- **Bash**: Version 4.0 or higher

## Testing

All CLI commands are tested via the eunit test suite at:
- `/Users/sac/erlmcp/test/erlmcp_cli_tests.erl`

Run tests:

```bash
rebar3 eunit -m erlmcp_cli_tests
```

## Performance Targets

The 100K concurrent test validates:
- **Throughput**: >50,000 ops/sec
- **Latency**: <10ms per operation
- **Concurrency**: 100,000 simultaneous operations

## Troubleshooting

### "Erlang not found"
Install Erlang/OTP 25+:
```bash
# macOS
brew install erlang

# Ubuntu/Debian
apt-get install erlang
```

### "rebar3 not found"
Install rebar3:
```bash
# Download and install rebar3
curl https://s3.amazonaws.com/rebar3/rebar3 -o ~/.local/bin/rebar3
chmod +x ~/.local/bin/rebar3
```

### Compilation fails
Clean and rebuild:
```bash
erlmcp init
# or
rebar3 clean && rebar3 compile
```

### 100K test hangs
The test can take 30-60 seconds depending on system load. Let it complete naturally or increase the timeout:
```bash
timeout 120 erlmcp test-100k
```

## Files

- **CLI Script**: `/Users/sac/erlmcp/bin/erlmcp`
- **Erlang Implementation**: `/Users/sac/erlmcp/priv/erlmcp_cli.erl`
- **Tests**: `/Users/sac/erlmcp/test/erlmcp_cli_tests.erl`

## Architecture

The CLI is implemented as a pure Bash script that:
1. Detects system requirements
2. Compiles the project using rebar3
3. Runs benchmarks and tests via Erlang
4. Outputs real numbers with color formatting

All operations are fully validated and tested with the eunit test suite.
