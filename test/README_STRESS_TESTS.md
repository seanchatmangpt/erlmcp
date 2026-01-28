# ERLMCP Simple Stress Tests

This directory contains stress tests for the ERLMCP (Erlang Model Context Protocol) implementation.

## Quick Start

Run the simple, fast stress tests:

```bash
# Run all quick tests (takes ~6 seconds)
escript quick_stress_test.erl

# Results will show:
# - Flood test: Messages generated per second
# - Connection test: Success/failure rates
# - Memory test: Memory usage patterns
```

## Available Tests

### 1. Quick Stress Test (`quick_stress_test.erl`)
**Recommended for immediate testing**

- **Flood Test**: Generates messages as fast as possible for 5 seconds
- **Connection Test**: Simulates 100 rapid connection attempts
- **Memory Test**: Creates and processes 100 large messages (10KB each)

**Sample Output:**
```
=== ERLMCP Quick Stress Test ===

1. Flood Test (5 seconds of maximum message generation)
   Result: 126145138 messages in 5.00 seconds = 25232227 msg/sec

2. Connection Test (100 rapid connection attempts)
   Result: 97 successful, 3 errors in 0.30 seconds

3. Memory Test (create and process 100 large messages)
   Result: 100 messages processed in 0.01 seconds
   Memory: 54623 KB -> 54633 KB (delta: 9.15 KB)

=== Summary ===
Flood: 25232227 msg/sec
Connections: 97 successful, 3 failed
Memory: 9.15 KB growth
Total test time: 5.31 seconds
```

### 2. Comprehensive Stress Test (`erlmcp_simple_stress.erl`)
**For detailed analysis (may take longer)**

More thorough tests with configurable parameters:

```bash
# Run individual tests
escript run_stress_test.erl flood       # 10-second flood test
escript run_stress_test.erl connection  # Connection cycling test
escript run_stress_test.erl memory      # Large message memory test

# Run all tests (may take 2+ minutes)
escript run_stress_test.erl all
```

**Features:**
- **Flood Test**: 10 seconds of maximum message generation with multiple workers
- **Connection Test**: 50 connections × 10 cycles with rapid open/close
- **Memory Test**: 1000 large messages (100KB each) with memory monitoring

## Test Parameters

You can modify these constants in the test files:

**Quick Test (`quick_stress_test.erl`):**
- Flood duration: 5 seconds
- Connection attempts: 100
- Message size: 10KB × 100 messages

**Comprehensive Test (`erlmcp_simple_stress.erl`):**
- `FLOOD_DURATION_SECONDS`: 10 seconds (line 23)
- `CONNECTION_COUNT`: 50 connections (line 24)
- `CONNECTION_CYCLES`: 10 cycles (line 25)
- `LARGE_MESSAGE_SIZE_KB`: 100 KB per message (line 26)
- `MEMORY_MESSAGE_COUNT`: 1000 messages (line 27)

## What the Tests Measure

### Flood Test
- **Messages per second**: Raw message generation capability
- **Error rates**: Simulated failure rates under load
- **Worker scalability**: Performance across multiple processes

### Connection Test
- **Connection throughput**: Connections established per second
- **Failure rates**: Error handling under rapid connection cycling
- **Resource cleanup**: Memory usage during connection churn

### Memory Test
- **Memory growth**: Heap usage with large message processing
- **Garbage collection**: Memory reclamation patterns
- **Peak usage**: Maximum memory consumption
- **Memory leaks**: Detection of unreclaimed memory

## Running Requirements

- Erlang/OTP 24+ recommended
- No external dependencies required
- Tests are self-contained and safe to run

## Understanding Results

**Good performance indicators:**
- Flood: >1M messages/sec
- Connections: >90% success rate, <5ms average
- Memory: <10% growth after garbage collection

**Potential issues to investigate:**
- Low message rates (<100K/sec)
- High error rates (>10%)
- Excessive memory growth (>100MB)
- Long test completion times

## Troubleshooting

**Tests timeout or fail:**
- Check system resources (CPU, memory)
- Reduce test parameters for slower systems
- Ensure Erlang VM has sufficient heap space

**Compilation errors:**
- Verify `include/erlmcp.hrl` exists
- Check Erlang version compatibility
- Ensure test directory has write permissions

**Memory errors in comprehensive test:**
- Use the quick test instead for compatibility
- The comprehensive test requires newer Erlang versions with map support in `erlang:memory()`
