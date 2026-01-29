# Benchmark Results

This directory contains the results of destructive stress testing and performance benchmarks for the erlmcp project.

## Connection Flood Stress Test (2026-01-29)

### Files
- `CONNECTION_FLOOD_ANALYSIS.md` - Comprehensive technical analysis
- `FLOOD_TEST_SUMMARY.txt` - Executive summary

### Key Findings

**BREAKING POINT: 12,261 concurrent TCP connections**

The MCP server fails at 12,261 connections due to Erlang VM file descriptor table exhaustion. This is a HARD LIMIT in the TCP inet driver layer.

### Root Cause

```
fd=24576 is larger than the largest allowed fd=24575
```

The Erlang VM TCP driver has a hardcoded file descriptor limit of 24,575. Each connection uses 2 FDs (socket + internal), giving a maximum of ~12,287 connections.

### Connection Sequence

- 1,000 conns: PASS (0.4s, 176 MB memory)
- 5,000 conns: PASS (2.0s, 880 MB memory)
- 10,000 conns: PASS (4.0s, 1.76 GB memory)
- **12,261 conns: BREAK (5.0s, 2.16 GB memory)**

### Production Readiness

**Status: NOT PRODUCTION READY** ⚠️

Critical failures:
- No connection limits enforced
- No graceful degradation under flood
- VM crashes instead of refusing connections
- No monitoring for resource exhaustion

### Recommendations

1. Set max_connections to 10,000 (80% of breaking point)
2. Add monitoring/alerting at 7,000 connections (70%)
3. Implement bounded refusal BEFORE resource exhaustion
4. Increase VM port limit (+P 262144)
5. Add connection cleanup for idle connections

### System Limits

Pre-Test:
- OS FD Limit: unlimited
- Kernel maxfiles: 368,640
- Erlang Port Limit: 65,536

At Breaking Point:
- Memory: 2,160.68 MiB
- Process Count: 12,306
- Port Count: 24,527
- Ports Available: 41,009

### Test Environment

- Platform: macOS (Darwin 25.2.0)
- Erlang: OTP 25+
- Transport: TCP (gen_tcp)
- Test Duration: 5 seconds
- Connection Rate: Maximum (no artificial delays)

---

## Running the Flood Test

To reproduce this test:

```bash
# Run the destructive flood test
escript /tmp/run_flood_test.erl

# View results
cat bench/results/CONNECTION_FLOOD_ANALYSIS.md
cat bench/results/FLOOD_TEST_SUMMARY.txt
```

**WARNING:** This test is designed to BREAK the system. Do NOT run in production environments.

---

## Other Benchmarks

Additional benchmark results will be added here as they are completed:

- Core operations benchmark
- Network transport benchmark
- Stress testing
- Chaos engineering
- Integration testing

See `/bench/` directory for benchmark source code.
