# Timeout Storm Test Suite - Documentation

## Overview

This test suite validates the timeout handling mechanism under extreme concurrency by sending 100,000 requests with 1ms timeout simultaneously.

## Test Files

### Quick Start
```bash
# Run 100K timeout storm test
./test/chaos/timeout_storm_standalone.erl

# Find exact breaking point
./test/chaos/find_breaking_point.sh

# Test specific timeout count
./test/chaos/timeout_incremental.erl 100
```

### Test Scripts

| Script | Purpose | Duration |
|--------|---------|----------|
| `timeout_storm_standalone.erl` | Full 100K timeout storm | 60s |
| `timeout_incremental.erl` | Test specific timeout count | 30s |
| `find_breaking_point.sh` | Incremental breaking point finder | 5-15min |
| `erlmcp_timeout_storm_SUITE.erl` | Common Test suite | 10-30min |

## Test Results Summary

### Breaking Point: **62-68 concurrent timeouts**

| Test Count | Success Rate | Duration | Status |
|------------|--------------|----------|--------|
| 50 | 100.00% | 1.12s | ✅ PASSED |
| 75 | 82.67% | 5.00s | ❌ FAILED |
| 100 | 68.00% | 5.00s | ❌ FAILED |
| 100,000 | 13.19% | 60.77s | ❌ FAILED |

**Conclusion**: System is 1,428x below target capacity (100K concurrent timeouts).

## Root Cause

**TCP Connection Bottleneck**: Connection-per-request architecture cannot scale.

**Solution**: Implement connection pooling (expected 10-100x improvement).

## Running the Tests

### Prerequisites
- Erlang/OTP 25+
- Rebar3
- Port 10010 available

### Quick Test (60 seconds)
```bash
./test/chaos/timeout_storm_standalone.erl
```

### Find Breaking Point (5-15 minutes)
```bash
./test/chaos/find_breaking_point.sh
```

### Specific Timeout Count
```bash
./test/chaos/timeout_incremental.erl 1000
```

### Full Common Test Suite
```bash
rebar3 ct --suite=erlmcp_timeout_storm
```

## Interpreting Results

### Success Criteria
- ✅ **PASSED**: >99% timeouts fired, no deadlock, server responsive
- ⚠️ **WARNING**: 90-99% timeouts fired, minor degradation
- ❌ **FAILED**: <90% timeouts fired, or deadlock detected

### Key Metrics

**Timeouts Fired**: Number of successful timeout operations

**Cleanup Success %**: (Timeouts Fired / Total) × 100

**Memory Growth**: (Memory After - Memory Before) / Memory Before × 100

**Deadlock Detected**: true if clients stuck after timeout

**Server Responsive**: true if server accepts new connections after test

## Expected Output

### Successful Test
```
=== Testing 50 concurrent timeouts ===
  Timeouts Fired: 50/50 (100.00%)
  Duration: 1.12 seconds
  Memory: 39.35 -> 39.44 MB (0.24%)
  Server Responsive: true
  Result: ✅ PASSED
```

### Failed Test
```
=== Testing 75 concurrent timeouts ===
  Timeouts Fired: 62/75 (82.67%)
  Duration: 5.00 seconds
  Memory: 39.20 -> 39.36 MB (0.39%)
  Server Responsive: true
  Result: ❌ FAILED (breaking point: 62)
```

## Troubleshooting

### Port Already in Use
```bash
lsof -ti:10010 | xargs kill -9
```

### Permission Denied
```bash
chmod +x ./test/chaos/*.erl
chmod +x ./test/chaos/*.sh
```

### Erlang VM Limits
```bash
# Increase process limit
erl +P 1048576 +Q 65536

# Increase file descriptor limit
ulimit -n 65536
```

## Documentation

- **Final Report**: `TIMEOUT_STORM_FINAL_REPORT.md` (comprehensive analysis)
- **Analysis**: `TIMEOUT_STORM_ANALYSIS.md` (executive summary)
- **Test Protocol**: `TIMEOUT_STORM_REPORT.md` (detailed methodology)

## Next Steps

1. ✅ Review test results in `TIMEOUT_STORM_FINAL_REPORT.md`
2. ⚠️ Implement connection pooling (P0 - critical)
3. ⚠️ Re-test after pooling implementation
4. ⚠️ Validate production readiness

## Contact

For questions or issues, refer to the main project documentation at `/Users/sac/erlmcp/docs/`.
