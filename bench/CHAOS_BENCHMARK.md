# Chaos/Adversarial Benchmark - erlmcp

## Overview

The `erlmcp_bench_chaos` module provides comprehensive chaos engineering and adversarial testing for the erlmcp system. It validates system behavior under failure conditions, ensuring bounded refusal, graceful degradation, and recovery within SLA.

## Purpose

Chaos testing validates that erlmcp:
1. **Refuses correctly** - Returns proper refusal codes BEFORE resource exhaustion
2. **Degrades gracefully** - Maintains service for unaffected operations
3. **Recovers automatically** - Returns to normal operation without manual intervention
4. **Preserves data** - No data loss during failures
5. **Prevents cascades** - Failures don't propagate across system

## Architecture

### Chaos Scenarios (11 Total)

| Scenario | Failure Type | Expected Behavior | Refusal Code |
|----------|--------------|-------------------|--------------|
| `chaos_process_crash` | Worker process killed | Recovery < 1s | - |
| `chaos_network_partition` | Network split | Graceful degradation | - |
| `chaos_memory_exhaustion` | Memory → 90% | Refusal before OOM | 1089 |
| `chaos_message_flood` | 10x capacity | Rate limiting | 1056 |
| `chaos_invalid_payload` | Malformed JSON | Protocol error | 1066 |
| `chaos_connection_leak` | Max connections | Connection limit | 1060 |
| `chaos_slow_consumer` | 10s delay | Timeout | 1069 |
| `chaos_supervisor_cascade` | Supervisor crash | Restart < 5s | - |
| `chaos_disk_full` | Disk 95% full | Logging stops gracefully | 1079 |
| `chaos_cpu_saturation` | CPU 100% | Scheduler backpressure | 1078 |
| `chaos_large_payload` | 100MB message | Size limit | 1068 |

### Bounded Refusal Validation

Each scenario validates **bounded refusal** - the system must refuse BEFORE actual resource exhaustion:

```erlang
validate_bounded_refusal(Result, Scenario) ->
    %% 1. Correct refusal code
    CodeMatches = ActualCode =:= ExpectedCode,

    %% 2. Fast detection (< 1s)
    FastDetection = DetectionTime < 1000.0,

    %% 3. Automatic recovery (< 5s)
    AutoRecovery = RecoveryTime < 5000.0,

    %% 4. No data loss
    NoDataLoss = DataLoss =:= false,

    %% 5. No cascading failures
    NoCascade = CascadingFailures =:= 0,

    CodeMatches andalso FastDetection andalso AutoRecovery
        andalso NoDataLoss andalso NoCascade.
```

## Usage

### Running All Scenarios

```bash
# Via script (recommended)
./bench/run_chaos_benchmarks.sh bench/results

# Via rebar3 shell
rebar3 shell
> {ok, Results} = erlmcp_bench_chaos:run_all_scenarios().
> Report = erlmcp_bench_chaos:generate_chaos_report(Results).
```

### Running Individual Scenarios

```erlang
%% Erlang shell
{ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_memory_exhaustion">>).

%% With custom config
{ok, Result} = erlmcp_bench_chaos:run_scenario(
    <<"chaos_message_flood">>,
    #{flood_multiplier => 20}
).
```

### Available Functions

```erlang
%% Run all scenarios
run_all_scenarios() -> {ok, [chaos_result()]} | {error, term()}.
run_all_scenarios(Config) -> {ok, [chaos_result()]} | {error, term()}.

%% Run specific scenario
run_scenario(ScenarioId) -> {ok, chaos_result()} | {error, term()}.
run_scenario(ScenarioId, Config) -> {ok, chaos_result()} | {error, term()}.

%% Get scenario definitions
scenarios() -> [map()].

%% Validate bounded refusal
validate_bounded_refusal(Result, Scenario) -> boolean().

%% Generate report
generate_chaos_report([chaos_result()]) -> map().
```

## Output Format

### Chaos Result (per scenario)

```json
{
  "workload_id": "chaos_memory_exhaustion",
  "benchmark": "chaos",
  "scenario": "memory_exhaustion",
  "injection_time_s": 0.15,
  "detection_time_ms": 100.0,
  "refusal_code": 1089,
  "refusal_message": "Resource exhausted",
  "recovery_time_ms": 500.0,
  "data_loss": false,
  "cascading_failures": 0,
  "logs_captured": true,
  "bounded_refusal_validated": true,
  "expected_behavior": "refusal_code_1089",
  "actual_behavior": "refusal_code_1089",
  "test_passed": true,
  "scope": "per_node",
  "timestamp": 1706380800
}
```

### Chaos Report (aggregate)

```json
{
  "benchmark": "chaos",
  "total_scenarios": 11,
  "passed": 11,
  "failed": 0,
  "success_rate_percent": 100.0,
  "avg_recovery_time_ms": 342.5,
  "avg_detection_time_ms": 85.2,
  "total_data_loss_events": 0,
  "total_cascading_failures": 0,
  "timestamp": 1706380800,
  "results": [...],
  "overall_status": "PASS"
}
```

## Metrology Compliance

All chaos results conform to the `shapes/metrology.schema.json` v1.5.0 standard:

### Required Fields
- `workload_id` - Unique scenario identifier
- `scope` - `per_process`, `per_connection`, `per_node`, `per_cluster`
- `timestamp` - Unix epoch seconds
- `test_passed` - Boolean pass/fail

### Measurements
Each scenario produces measurements compatible with metrology schema:
- Detection time (time dimension, ms)
- Recovery time (time dimension, ms)
- Injection duration (time dimension, s)

### Quality Gates
Default quality gates for chaos benchmarks:
```json
{
  "success_rate_min_percent": 90.0,
  "avg_detection_time_max_ms": 1000.0,
  "avg_recovery_time_max_ms": 5000.0,
  "data_loss_max_events": 0,
  "cascading_failures_max": 0
}
```

## Integration with Plan Tiers

Chaos scenarios validate refusal codes defined in `plans/*.json`:

### Team Tier (plans/team.plan.json)
- Max connections: 25,000 → validates `chaos_connection_leak`
- Max message size: 1MB → validates `chaos_large_payload`
- Rate limit: 900 msg/s → validates `chaos_message_flood`

### Enterprise Tier (plans/enterprise.plan.json)
- Max connections: 100,000
- Max message size: 10MB
- Rate limit: 3000 msg/s
- Failover SLA: 2s

### Gov Tier (plans/gov.plan.json)
- Max connections: 200,000
- Max message size: 100MB
- Rate limit: 10,000 msg/s
- Failover SLA: 1s

## Refusal Code Reference

From `include/erlmcp_refusal.hrl`:

| Code | Name | HTTP Status | Category |
|------|------|-------------|----------|
| 1056 | `REFUSAL_RATE_LIMIT_EXCEEDED` | 429 | Rate Limiting |
| 1060 | `REFUSAL_CONCURRENT_LIMIT_EXCEEDED` | 429 | Rate Limiting |
| 1066 | `REFUSAL_PROTOCOL_ERROR` | 400 | Protocol |
| 1068 | `REFUSAL_MESSAGE_TOO_LARGE` | 413 | Protocol |
| 1069 | `REFUSAL_TIMEOUT` | 503 | Protocol |
| 1078 | `REFUSAL_SERVICE_UNAVAILABLE` | 503 | Server State |
| 1079 | `REFUSAL_INTERNAL_ERROR` | 503 | Server State |
| 1089 | `REFUSAL_RESOURCE_EXHAUSTED` | 503 | Circuit Breaker |

## Testing

Common Test suite: `test/erlmcp_bench_chaos_SUITE.erl`

### Run Tests

```bash
# Full test suite
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE

# Specific test group
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE --group=scenario_tests

# Single test case
rebar3 ct --suite=test/erlmcp_bench_chaos_SUITE --case=test_memory_exhaustion_scenario
```

### Test Coverage
- 16 test cases
- 3 test groups:
  - `scenario_tests` - Individual scenario validation (12 tests, parallel)
  - `integration_tests` - Full suite execution (2 tests, sequence)
  - `validation_tests` - Bounded refusal and metrology (2 tests, parallel)

## Safety Mechanisms

Chaos testing includes safety guardrails:

1. **Isolated execution** - Each scenario runs in separate process
2. **Timeout protection** - Per-scenario timeout (5-15s)
3. **Cleanup functions** - Automatic resource cleanup after each scenario
4. **Test environment** - Never runs in production (checks `environment` config)
5. **Simulated failures** - Most scenarios simulate failures without actually crashing

## Extending

### Adding New Scenarios

1. Define scenario in `scenarios/0`:
```erlang
#{
    id => <<"chaos_new_scenario">>,
    description => <<"Description of failure">>,
    inject => fun chaos_new_scenario/1,
    expected => expected_behavior,
    expected_code => ?REFUSAL_CODE,
    timeout_ms => 10000
}
```

2. Implement injection function:
```erlang
-spec chaos_new_scenario(map()) -> chaos_result().
chaos_new_scenario(_Config) ->
    %% 1. Log start
    logger:info("Starting chaos scenario: ~s", [<<"chaos_new_scenario">>]),

    %% 2. Inject failure
    InjectionStart = erlang:monotonic_time(millisecond),
    %% ... inject failure ...

    %% 3. Measure detection
    DetectionTime = %% ... ,

    %% 4. Validate refusal
    RefusalCode = %% ... ,

    %% 5. Measure recovery
    RecoveryTime = %% ... ,

    %% 6. Return result
    #{
        <<"workload_id">> => <<"chaos_new_scenario">>,
        <<"benchmark">> => <<"chaos">>,
        <<"scenario">> => <<"new_scenario">>,
        <<"injection_time_s">> => InjectionTime,
        <<"detection_time_ms">> => DetectionTime,
        <<"refusal_code">> => RefusalCode,
        <<"refusal_message">> => RefusalMsg,
        <<"recovery_time_ms">> => RecoveryTime,
        <<"data_loss">> => false,
        <<"cascading_failures">> => 0,
        <<"logs_captured">> => true,
        <<"bounded_refusal_validated">> => true,
        <<"expected_behavior">> => <<"expected">>,
        <<"actual_behavior">> => <<"actual">>,
        <<"test_passed">> => true,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    }.
```

3. Add test case in `test/erlmcp_bench_chaos_SUITE.erl`

## References

- [Chaos Engineering Principles](https://principlesofchaos.org/)
- [Netflix Chaos Monkey](https://netflix.github.io/chaosmonkey/)
- [erlmcp Refusal Taxonomy](../docs/reference/refusals.md)
- [Metrology Standard v1.5.0](../shapes/metrology.schema.json)
- [Plan Tier Specifications](../plans/)

## License

Same as erlmcp project license.
