# Chaos Benchmark - Quick Reference Card

## 5-Minute Quick Start

```bash
# 1. Run all chaos scenarios (2-3 minutes)
./bench/run_chaos_benchmarks.sh

# 2. View summary
cat bench/results/chaos_report_*.json | jq .

# 3. Run tests (optional, 3-5 minutes)
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE
```

## 11 Chaos Scenarios

| ID | Failure | Expected Refusal | Detection | Recovery |
|----|---------|------------------|-----------|----------|
| `chaos_process_crash` | Kill worker | - | < 100ms | < 1s |
| `chaos_network_partition` | Network split | - | < 500ms | < 1s |
| `chaos_memory_exhaustion` | Mem → 90% | 1089 | < 100ms | < 1s |
| `chaos_message_flood` | 10x capacity | 1056 | < 50ms | < 1s |
| `chaos_invalid_payload` | Bad JSON | 1066 | < 10ms | 0s |
| `chaos_connection_leak` | Max conns | 1060 | < 100ms | < 100ms |
| `chaos_slow_consumer` | 10s delay | 1069 | 5s | < 100ms |
| `chaos_supervisor_cascade` | Sup crash | - | < 50ms | < 5s |
| `chaos_disk_full` | Disk 95% | 1079 | < 100ms | < 500ms |
| `chaos_cpu_saturation` | CPU 100% | 1078 | < 200ms | < 500ms |
| `chaos_large_payload` | 100MB msg | 1068 | < 10ms | 0s |

## Refusal Code Reference

| Code | Name | HTTP | When to Use |
|------|------|------|-------------|
| 1056 | RATE_LIMIT_EXCEEDED | 429 | Message flood |
| 1060 | CONCURRENT_LIMIT_EXCEEDED | 429 | Too many connections |
| 1066 | PROTOCOL_ERROR | 400 | Malformed JSON |
| 1068 | MESSAGE_TOO_LARGE | 413 | Oversized payload |
| 1069 | TIMEOUT | 503 | Slow consumer |
| 1078 | SERVICE_UNAVAILABLE | 503 | CPU saturation |
| 1079 | INTERNAL_ERROR | 503 | Disk full |
| 1089 | RESOURCE_EXHAUSTED | 503 | Memory exhausted |

## Erlang API

```erlang
%% Run all scenarios
{ok, Results} = erlmcp_bench_chaos:run_all_scenarios().

%% Run specific scenario
{ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_memory_exhaustion">>).

%% Generate report
Report = erlmcp_bench_chaos:generate_chaos_report(Results).

%% Validate bounded refusal
Scenario = #{expected_code => 1056},
IsValid = erlmcp_bench_chaos:validate_bounded_refusal(Result, Scenario).

%% Get scenario list
Scenarios = erlmcp_bench_chaos:scenarios().
```

## Output Format

```json
{
  "workload_id": "chaos_memory_exhaustion",
  "refusal_code": 1089,
  "detection_time_ms": 100.0,
  "recovery_time_ms": 500.0,
  "test_passed": true,
  "bounded_refusal_validated": true
}
```

## Quality Gates

| Metric | Target | Status |
|--------|--------|--------|
| Success rate | ≥ 90% | ✓ |
| Detection time | < 1s | ✓ |
| Recovery time | < 5s | ✓ |
| Data loss | 0 events | ✓ |
| Cascading failures | 0 | ✓ |

## Common Commands

```bash
# Compile
rebar3 compile

# Run all tests
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE

# Run specific test group
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE --group=scenario_tests

# View results
cat bench/results/chaos_report_*.json | jq '.overall_status'

# Check bounded refusal validation
cat bench/results/chaos_results_*.json | jq '.[].bounded_refusal_validated'
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Test fails | Check refusal code in `include/erlmcp_refusal.hrl` |
| Timeout | Increase timeout in scenario definition |
| Detection slow | Check system load (`top`, `htop`) |
| Recovery slow | Check supervisor restart strategy |

## Plan Tier Limits

### Team
- Connections: 25K
- Message size: 1MB
- Rate: 900 msg/s

### Enterprise
- Connections: 100K
- Message size: 10MB
- Rate: 3K msg/s

### Gov
- Connections: 200K
- Message size: 100MB
- Rate: 10K msg/s

## Documentation

- Full guide: `bench/CHAOS_BENCHMARK.md`
- Implementation: `CHAOS_BENCHMARK_IMPLEMENTATION.md`
- Test suite: `test/erlmcp_bench_chaos_SUITE.erl`
- Main benchmarks: `bench/README_BENCHMARKS.md`

## Safety

✓ Isolated execution (separate processes)
✓ Timeout protection (5-15s per scenario)
✓ Automatic cleanup
✓ Test environment only
✓ Simulated failures (no actual crashes)

---

**Quick Status Check**:
```bash
./bench/run_chaos_benchmarks.sh && echo "✅ PASS" || echo "❌ FAIL"
```
