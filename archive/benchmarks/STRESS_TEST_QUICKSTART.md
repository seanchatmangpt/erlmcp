# erlmcp Stress Testing - Quick Start

## 30-Second Setup

```bash
# 1. Navigate to project
cd /Users/sac/erlmcp

# 2. Compile
rebar3 compile

# 3. Run cascading failure test (1-2 hours)
rebar3 ct --suite erlmcp_stress_cascading_tests
```

## One-Command Test Execution

```bash
# Quick demo (10 minutes)
STRESS_TEST_MODE=demo rebar3 ct --suite erlmcp_stress_cascading_tests

# Production validation (requires multiple hours)
rebar3 do clean, compile, \
    ct --suite erlmcp_stress_baseline_tests, \
    ct --suite erlmcp_stress_scale_tests, \
    ct --suite erlmcp_stress_cascading_tests
```

## What Gets Tested

| Test | Duration | Validates | Command |
|------|----------|-----------|---------|
| **Cascading** | 1-2 hrs | Multiple simultaneous failures | `rebar3 ct --suite erlmcp_stress_cascading_tests` |
| **Baseline** | 10 min | 150 connections @ 5K msg/sec | `rebar3 ct --suite erlmcp_stress_baseline_tests` |
| **Scaling** | 60 min | 100x growth (150→15K conns) | `rebar3 ct --suite erlmcp_stress_scale_tests` |
| **Chaos** | 3-5 hrs | Failure resilience | `rebar3 ct --suite erlmcp_stress_chaos_tests` |
| **Sustained** | 24 hrs | Long-term stability | `STRESS_TEST_MODE=extended rebar3 ct --suite erlmcp_stress_sustained_tests` |

## Monitoring During Test

```bash
# Watch metrics in real-time (in another terminal)
watch -n 1 'curl -s http://localhost:9090/metrics | grep erlmcp_throughput'

# Or check all metrics endpoints
for port in 9090 9091 9092 9093 9094; do
  echo "=== Port $port ===" && \
  curl -s http://localhost:$port/metrics | head -5
done
```

## Expected Results

### Success (Green Zone)
```
✅ Throughput: 90%+ of target
✅ Latency: P95 within ±20% of baseline
✅ Error Rate: <0.1%
✅ Memory: Stable, <100MB growth/hour
✅ Recovery: <30 seconds from failures
```

### Issues Found (Yellow/Red Zone)
```
⚠️ Throughput: 80-90% of target
⚠️ Latency: P95 off by >20%
❌ Error Rate: >1%
❌ Memory: Growing >200MB/hour (leak?)
❌ Recovery: >120 seconds
```

## Reading Test Output

```
=== Test Results ===
Throughput: 500000 msg/sec (target: 500000)
Latency P95: 50 ms (target: 50 ms ±20%)
Error Rate: 0.05% (target: <0.1%)
Memory: 1800 MB (target: <2000 MB)

Status: ✓ PASS - All metrics within acceptable range
```

## Troubleshooting Quick Fixes

### Timeout Error
```bash
# System under stress - reduce load or increase timeout
ulimit -n 65536  # Increase file descriptors
rebar3 ct --suite erlmcp_stress_cascading_tests --timeout 3600
```

### Connection Refused
```bash
# Check server is running
lsof -i :9001

# Restart if needed
pkill -9 erl
rebar3 ct --suite erlmcp_stress_cascading_tests
```

### Memory Warnings
```bash
# Monitor with observer
make observer

# Or run quick baseline instead of full suite
STRESS_TEST_MODE=demo rebar3 ct --suite erlmcp_stress_baseline_tests
```

## Files Generated

After test completion:
```
_build/test/cover/          # Code coverage reports
ct_run.test@HOSTNAME.*/     # Test logs
YYYY_MM_DD_HH_MM_SS/        # Test results directory
```

View HTML report:
```bash
open "_build/test/cover/index.html"
```

## Performance Targets (Quick Reference)

| Load | Connections | Throughput | Latency P95 | Error Rate |
|------|-------------|-----------|------------|-----------|
| Baseline | 150 | 5K/sec | 85ms | <0.1% |
| Moderate | 500 | 15K/sec | 80ms | <0.1% |
| Medium | 1K | 30K/sec | 75ms | <0.1% |
| Heavy | 5K | 150K/sec | 65ms | <0.1% |
| Peak | 10K | 300K/sec | 58ms | <0.1% |
| Maximum | 15K | 500K/sec | 50ms | <0.1% |

## CI/CD Integration

```yaml
# Add to GitHub Actions (.github/workflows/stress-tests.yml)
name: Stress Tests
on: [push, pull_request]
jobs:
  stress:
    runs-on: ubuntu-latest
    timeout-minutes: 120
    steps:
      - uses: actions/checkout@v2
      - name: Run stress tests
        run: rebar3 ct --suite erlmcp_stress_cascading_tests
```

## Get Help

- **Full Guide**: See `/Users/sac/erlmcp/docs/STRESS_TEST_GUIDE.md`
- **Implementation Details**: See `/Users/sac/erlmcp/STRESS_TEST_IMPLEMENTATION_SUMMARY.md`
- **Test Code**: See `/Users/sac/erlmcp/test/erlmcp_stress_cascading_tests.erl`

## Key Metrics Explained

- **Throughput**: Messages per second (target: 500K at 15K connections = 100x baseline)
- **Latency P95**: 95th percentile response time (target: bounded below 100ms even at 100x load)
- **Error Rate**: % of failed messages (target: <0.1% under normal load, <5% during failures)
- **Memory**: RAM usage in MB (target: <2GB at 15K connections, stable over 24 hours)
- **Recovery**: Time to return to baseline after failure (target: <30 seconds)

## Example Full Test Run

```bash
# 1. Start fresh
rebar3 clean

# 2. Compile everything
rebar3 compile

# 3. Run cascading test (core functionality - 1-2 hours)
time rebar3 ct --suite erlmcp_stress_cascading_tests

# 4. View results
cat ct_run.test@*/test_server@*/run.summary

# 5. Success if you see:
# ====== Test Results ======
# Passed: 5 Failed: 0 Errors: 0
# Status: PASS - Cascading Failure Tests Complete
```

## System Requirements

For full stress testing:
- **RAM**: 4GB+ (for 15K connections)
- **CPU**: 8+ cores (for simulating load)
- **Network**: 1Gbps+ (for high throughput)
- **File Descriptors**: `ulimit -n` >= 20000
- **OS**: Linux/macOS/Unix with Erlang OTP 25+

Check requirements:
```bash
ulimit -n          # Should be > 20000
erlang --version   # Should be OTP 25+
nproc --all        # CPU cores
free -h            # Available RAM
```

## Next Steps After Testing

1. **If All Tests Pass** ✅
   - Integration into CI/CD complete
   - 100x scalability validated
   - Ready for production deployment

2. **If Issues Found** ⚠️
   - Check STRESS_TEST_GUIDE.md troubleshooting section
   - Profile bottlenecks: `make profile`
   - Optimize and re-run

3. **Performance Optimization** 🚀
   - Tune GC settings in vm.args
   - Profile hot paths
   - Re-run tests for validation
   - Document improvements

---

**Ready to validate 100x scalability?** Run this:
```bash
rebar3 ct --suite erlmcp_stress_cascading_tests
```

**Want full validation?** Run this:
```bash
time rebar3 do clean, compile, \
    ct --suite erlmcp_stress_baseline_tests, \
    ct --suite erlmcp_stress_scale_tests, \
    ct --suite erlmcp_stress_cascading_tests
```

Good luck! 🚀
