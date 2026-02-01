# erlmcp CLI Reference Guide

Complete command reference for the erlmcp command-line interface. Learn how to initialize environments, start servers, run tests, and diagnose issues.

**Table of Contents**:
- [Quick Start](#quick-start)
- [Command Summary](#command-summary)
- [Detailed Commands](#detailed-commands)
- [Exit Codes](#exit-codes)
- [Performance Characteristics](#performance-characteristics)
- [Troubleshooting](#troubleshooting)

## Quick Start

```bash
# Installation: Add to PATH
export PATH="/path/to/erlmcp/bin:$PATH"

# Or run directly
/path/to/erlmcp/bin/erlmcp <command>

# First time setup
erlmcp init
erlmcp doctor
erlmcp start
```

## Command Summary

| Command | Purpose | Time | Output |
|---------|---------|------|--------|
| `init` | Initialize environment | 10-30s | Configuration status |
| `start` | Start interactive server | interactive | Erlang shell |
| `stop` | Stop running server | <1s | Confirmation |
| `status` | Check cluster status | <5s | Node/version/process info |
| `doctor` | Diagnostic readiness check | 5-15s | System capability report |
| `test-100k` | Run 100K concurrent test | 30-60s | Throughput/latency metrics |
| `benchmark` | Full performance suite | 2-5m | 5 benchmark categories |
| `help` | Display help | <1s | Command list |

## Detailed Commands

### `erlmcp init`

Initialize the local development environment.

**Usage**:
```bash
erlmcp init
```

**What it does**:
1. Checks Erlang/OTP version (requires OTP 28.3.1+)
2. Verifies rebar3 installation
3. Compiles the project and all dependencies
4. Loads and initializes applications
5. Validates system configuration

**Output**:
```
======================================================================
INITIALIZING ERLMCP ENVIRONMENT
======================================================================
⚙ Checking Erlang version... OK
🔨 Compiling project... OK
✓ Dependencies fetched
✓ Configuration loaded

🚀 Environment ready for development!
```

**Exit codes**:
- `0`: Success
- `1`: Erlang not found
- `2`: rebar3 not found
- `3`: Erlang version too old
- `4`: Compilation failed

**When to use**:
- First time setup
- After pulling new changes
- After modifying rebar.config
- After dependency updates

**Time**: 10-30 seconds (depends on cached artifacts)

---

### `erlmcp start`

Start the erlmcp application in an interactive Erlang shell.

**Usage**:
```bash
erlmcp start
```

**What it does**:
1. Compiles the project
2. Starts Erlang/OTP VM
3. Loads erlmcp application and all dependencies
4. Launches interactive shell
5. Provides access to Erlang REPL

**Output**:
```
======================================================================
STARTING ERLMCP CLUSTER
======================================================================
🔨 Compiling... OK
🚀 Starting Erlang shell... OK
📡 Launching erlmcp application...

Erlang/OTP 28 [erts-14.0]

1>
```

**Interactive shell commands** (once started):
```erlang
% Check application status
1> application:which_applications().

% Start a server
2> {ok, ServerPid} = erlmcp_server:start_link(my_server, #{}).

% List processes
3> i().

% Get help
4> help().

% Exit
5> q().
```

**Exit codes**:
- `0`: Graceful exit (user quit)
- `1`: Compilation failed
- `2`: Erlang VM failed to start

**Performance**:
- Startup: 5-10 seconds
- REPL response: <1ms

---

### `erlmcp stop`

Stop the running erlmcp cluster.

**Usage**:
```bash
erlmcp stop
```

**What it does**:
1. Sends shutdown signal to running erlmcp node
2. Gracefully closes all connections
3. Flushes pending messages
4. Terminates VM

**Output**:
```
======================================================================
STOPPING ERLMCP CLUSTER
======================================================================
⏹ Stopping erlmcp application... OK
✓ Cluster stopped
```

**Exit codes**:
- `0`: Success
- `1`: No running cluster found
- `2`: Shutdown timeout

**Note**: If using `erlmcp start` interactively, just type `q().` in the REPL instead.

---

### `erlmcp status`

Check the status of the erlmcp cluster.

**Usage**:
```bash
erlmcp status
```

**What it does**:
1. Queries local Erlang node
2. Retrieves system information
3. Counts running processes
4. Reports cluster connectivity

**Output**:
```
======================================================================
CLUSTER STATUS
======================================================================
ℹ Node:        localhost
ℹ Status:      available
ℹ Erlang:      OTP 28
ℹ Processes:   42
```

**Output fields**:
- **Node**: Node name (usually localhost for single-node)
- **Status**: available/unavailable
- **Erlang**: OTP release version
- **Processes**: Total process count

**Exit codes**:
- `0`: Node available
- `1`: Node unavailable
- `2`: Connection timeout

**Time**: <5 seconds

---

### `erlmcp doctor`

Run host readiness diagnostics.

**Usage**:
```bash
# Human-readable output
erlmcp doctor

# JSON output (for scripts)
erlmcp doctor-json
```

**What it does**:
1. Checks Erlang/OTP version and installation
2. Verifies rebar3 is available
3. Tests compilation pipeline
4. Validates system resources
5. Checks transport availability
6. Verifies security features

**Output**:
```
======================================================================
ERLMCP DOCTOR - Host Readiness Check
======================================================================
ℹ Running diagnostics...

✓ Erlang/OTP 28.3.1 - OK
✓ rebar3 3.22.0 - OK
✓ Compilation - OK
✓ Memory (16GB) - OK
✓ Disk space (250GB) - OK
✓ STDIO transport - OK
✓ HTTP transport - OK
✓ SSL/TLS - OK (OpenSSL 3.0)
✓ IPv6 - OK

Status: READY - All checks passed
```

**Doctor checks**:

| Check | Purpose | Pass Criteria |
|-------|---------|---------------|
| Erlang version | OTP 28.3.1+ installed | version >= 28.3.1 |
| rebar3 | Build tool available | executable found in PATH |
| Compilation | Can build project | compile returns 0 |
| Memory | Sufficient RAM available | >= 1GB free |
| Disk | Sufficient storage | >= 5GB free |
| STDIO | STDIO transport works | can bind to stdin/stdout |
| HTTP | HTTP server functional | can bind to port 8080 |
| SSL/TLS | TLS libraries present | OpenSSL/LibreSSL available |
| IPv6 | IPv6 support available | socket can bind to ::1 |

**Exit codes**:
- `0`: All checks passed
- `1`: Warning (non-critical failure)
- `2`: Failure (critical issue)

**Interpreting results**:

**READY** - Proceed with `erlmcp start`
```
Status: READY - All checks passed
```

**WARNING** - Some optional features unavailable
```
Status: WARNING - IPv6 not available (optional)
```

**FAILED** - Cannot proceed
```
Status: FAILED - Erlang 28.3.1+ required, found 27
```

**JSON output**:
```bash
erlmcp doctor-json
```

```json
{
  "status": "ready",
  "checks": [
    {
      "name": "erlang_version",
      "status": "pass",
      "details": "OTP 28.3.1"
    },
    {
      "name": "rebar3",
      "status": "pass",
      "details": "3.22.0"
    }
  ],
  "timestamp": "2026-02-01T12:00:00Z"
}
```

**Time**: 5-15 seconds

---

### `erlmcp test-100k`

Run 100K concurrent operations performance test.

**Usage**:
```bash
erlmcp test-100k
```

**What it does**:
1. Compiles project
2. Starts Erlang VM
3. Spawns 1000 worker processes
4. Each worker handles 100 concurrent operations
5. Measures throughput and latency
6. Validates performance thresholds

**Output**:
```
======================================================================
ERLMCP 100K CONCURRENT TEST
======================================================================
🔨 Compiling... OK
🚀 Starting Erlang VM... OK
📡 Loading erlmcp application... OK

🧪 Running 100K concurrent operations test...

===============================================================================
TEST RESULTS
===============================================================================
  Total Operations:   100,000
  Time Elapsed:       628 ms
  Throughput:        159235 ops/sec
  Per-operation:      0.006 ms

STATUS: PASS - 100K concurrent test successful

✓ 100K test complete
```

**Performance targets**:
- **Throughput**: >50,000 ops/sec (PASS/FAIL threshold)
- **Latency**: <10ms per operation
- **Concurrency**: 100,000 simultaneous operations

**Interpreting results**:

| Metric | Excellent | Good | Acceptable | Poor |
|--------|-----------|------|------------|------|
| Throughput | >150K ops/s | >100K ops/s | >50K ops/s | <50K ops/s |
| Per-op latency | <0.01ms | <0.02ms | <0.1ms | >0.1ms |

**Exit codes**:
- `0`: PASS (throughput >50K ops/s)
- `1`: WARNING (throughput 30K-50K ops/s)
- `2`: FAIL (throughput <30K ops/s)

**Troubleshooting**:

**Test hangs (no output for 30+ seconds)**:
```bash
# Increase timeout and try again
timeout 120 erlmcp test-100k
```

**Low throughput**:
1. Check system load: `top`, `htop`
2. Close other applications
3. Ensure sufficient RAM
4. Restart and try again

**Time**: 30-60 seconds (depends on system load)

---

### `erlmcp benchmark`

Run comprehensive performance benchmarks (5 categories, 2-5 minutes).

**Usage**:
```bash
erlmcp benchmark
```

**What it does**:
1. Compiles project
2. Starts Erlang VM
3. Runs 5 benchmark suites
4. Measures latency, throughput, memory
5. Compares against baselines
6. Reports regression analysis

**Output**:
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

**Benchmark categories**:

1. **Registry Operations** - Message passing performance
   - Throughput: 550K+ msg/s
   - Latency: <1µs p99

2. **Message Throughput** - Queue performance
   - Throughput: 970K+ msg/s
   - Latency: <1µs p99

3. **Latency Measurement** - Percentile distribution
   - P50 (median): ~0.5µs
   - P95: ~2µs
   - P99: ~3µs

4. **Memory Scaling** - Allocation efficiency
   - Per-operation: ~50 bytes
   - Garbage collection: <1ms pause

5. **Concurrent Processes** - Spawning and management
   - Spawn rate: 10K/second
   - Memory per process: ~2.5KB

**Baseline values** (OTP 28.3.1, Jan 2026):

| Metric | Baseline | Status |
|--------|----------|--------|
| Registry msg/s | 553K | Reference |
| Queue msg/s | 971K | Reference |
| Pool msg/s | 149K | Reference |
| Session msg/s | 242K | Reference |
| Network I/O | 43K | Reference |
| Sustained | 372K | Reference |

**Exit codes**:
- `0`: All benchmarks pass (regression <10%)
- `1`: Warning (regression 10-20%)
- `2`: Failure (regression >20%)

**Analyzing output**:

Look for **regression** - if your numbers are significantly lower than baseline:
1. System load issues
2. Insufficient resources
3. Network congestion
4. Hardware performance variance

**Time**: 2-5 minutes (all 5 benchmarks)

---

### `erlmcp help`

Display help information.

**Usage**:
```bash
erlmcp help
erlmcp help <command>
erlmcp --help
erlmcp -h
```

**Output**:
```
======================================================================
ERLMCP CLI - Developer Experience Tool
======================================================================

Usage: erlmcp <command>

Commands:
  ⚙ init              Initialize local development environment
  🚀 start            Start local erlmcp cluster
  ⏹ stop              Stop local erlmcp cluster
  ℹ status            Show cluster status
  ℹ doctor            Run host readiness diagnostics
  🧪 test-100k        Run 100K concurrent test
  📊 benchmark        Run performance benchmarks
  ❓ help             Show this help message

Examples:
  erlmcp init
  erlmcp doctor
  erlmcp start
  erlmcp test-100k
  erlmcp benchmark
```

---

## Exit Codes

Standard exit codes across all commands:

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success | Command completed successfully |
| 1 | General error | Check error message in output |
| 2 | Compilation error | Run `rebar3 compile` for details |
| 3 | Configuration error | Check system configuration |
| 4 | Runtime error | Check logs for details |
| 5 | Timeout | Increase timeout or check system load |

**Scripting with exit codes**:

```bash
#!/bin/bash
set -e  # Exit on any error

# Only proceed if erlmcp init succeeds
if ! erlmcp init; then
    echo "Failed to initialize erlmcp"
    exit 1
fi

# Run tests
erlmcp test-100k || {
    echo "Performance test failed"
    exit 2
}

# Everything succeeded
echo "All checks passed"
```

---

## Performance Characteristics

### Expected runtimes

| Command | Min | Typical | Max | Notes |
|---------|-----|---------|-----|-------|
| init | 5s | 15s | 30s | First run slower (deps download) |
| start | 3s | 5s | 10s | Time to REPL prompt |
| stop | <1s | <1s | 5s | Graceful shutdown |
| status | 1s | 2s | 5s | Network roundtrip |
| doctor | 3s | 8s | 15s | Runs all checks |
| test-100k | 20s | 45s | 60s | System load dependent |
| benchmark | 2m | 3m | 5m | All 5 suites run |

### System resource usage

| Command | CPU | Memory | Disk |
|---------|-----|--------|------|
| init | High (30-50%) | 500MB-1GB | 100MB writes |
| start | Idle-High | 200-400MB | No writes |
| test-100k | Very High (100%) | 1-2GB spike | No writes |
| benchmark | Very High (100%) | 1-2GB spike | No writes |

### Network usage

- **init**: Downloads dependencies once (20-50MB)
- **start**: No network
- **doctor**: <1MB (version checks)
- **test-100k**: No network
- **benchmark**: No network

---

## Troubleshooting

### Common issues and solutions

#### "Erlang not found"

**Problem**: Command fails with "Erlang not found"

**Solution**:
```bash
# Check if Erlang is installed
erl -version

# Install if missing
# macOS
brew install erlang

# Ubuntu/Debian
apt-get install erlang

# Verify version
erl -version  # Must be OTP 28.3.1+
```

#### "rebar3 not found"

**Problem**: Command fails with "rebar3 not found"

**Solution**:
```bash
# Install rebar3
curl https://s3.amazonaws.com/rebar3/rebar3 -o ~/.local/bin/rebar3
chmod +x ~/.local/bin/rebar3

# Verify
rebar3 version
```

#### "Erlang version too old"

**Problem**: "Erlang 28.3.1+ required, found X.Y.Z"

**Solution**:
```bash
# Upgrade Erlang
# macOS
brew upgrade erlang

# Ubuntu/Debian
apt-get install erlang-base erlang-dev erlang-tools

# Verify
erl -version
```

#### Compilation fails

**Problem**: `erlmcp init` fails at compilation step

**Solution**:
```bash
# Clean and rebuild
rebar3 clean
rebar3 compile

# Check for specific error
rebar3 compile 2>&1 | tail -20

# If stuck, reset completely
rm -rf _build
rebar3 compile
```

#### test-100k hangs

**Problem**: `erlmcp test-100k` runs for >90 seconds or seems stuck

**Solution**:
```bash
# Interrupt with Ctrl+C and try with timeout
timeout 120 erlmcp test-100k

# Check system load
top  # or htop
# If load is very high, close other applications

# Try again
erlmcp test-100k
```

#### Low performance benchmarks

**Problem**: Throughput significantly lower than baseline

**Solutions** (in order):
1. Check system load: `uptime`, `top`
2. Close other applications
3. Ensure sufficient free RAM: `free -h` or `vm_stat` (macOS)
4. Disable CPU frequency scaling if possible
5. Run on a quieter system
6. Check thermal conditions: `sensors` (Linux) or Activity Monitor (macOS)

#### "Status: available" but can't connect

**Problem**: `erlmcp status` reports available but REPL unreachable

**Solution**:
```bash
# Try starting fresh
erlmcp stop
sleep 2
erlmcp start

# Or check logs
tail -f /tmp/erlmcp.log
```

#### doctor reports failures

**Problem**: `erlmcp doctor` shows FAILED status

**Solution**:
Follow the specific check failure in doctor output. Examples:

```
✗ Memory - FAILED (only 512MB free, need 1GB)
  → Close applications or add RAM

✗ SSL/TLS - WARNING (OpenSSL not found, optional)
  → Install OpenSSL if TLS is needed (usually optional)
```

---

## Integration Examples

### Shell scripts

```bash
#!/bin/bash
# Continuous monitoring

while true; do
    erlmcp status
    sleep 5
done
```

### Cron jobs

```bash
# Run benchmark daily
0 3 * * * cd /path/to/erlmcp && erlmcp benchmark >> /var/log/erlmcp-bench.log
```

### Docker

```dockerfile
FROM erlang:28.3.1
RUN git clone https://github.com/user/erlmcp.git /erlmcp
WORKDIR /erlmcp
RUN erlmcp init
CMD ["erlmcp", "start"]
```

---

## Advanced Topics

### Custom configuration

Environment variables:
```bash
# Override config file location
export ERLMCP_CONFIG=/path/to/config.erlang

# Set Erlang VM parameters
export ERL_FLAGS="+P 256000 +K true"

erlmcp start
```

### Performance tuning

In Erlang shell:
```erlang
% Increase process limit
erl +P 256000

% Enable HiPE compilation
hipe:c(erlmcp_server)

% Check scheduler count
erlang:system_info(schedulers)
```

### Debugging

```bash
# Enable debug logging
export DEBUG=1
erlmcp start

# Verbose output
erlmcp doctor --verbose
```

---

## See Also

- [CLI Interactive Guide](CLI_INTERACTIVE_GUIDE.md) - Interactive REPL documentation
- [Diagnostics Guide](DIAGNOSTICS_GUIDE.md) - Advanced debugging tools
- [Shell Completions Guide](SHELL_COMPLETIONS_GUIDE.md) - Tab completion setup
- [README](../README.md) - Project overview
