# Chaos Engineering Test Suite - Implementation Summary

## Overview

A comprehensive chaos engineering framework has been implemented for erlmcp v3. The framework provides fault injection capabilities, steady state monitoring, MTTR benchmarking, and complete test suites for validating system resilience.

## Components Delivered

### 1. Core Modules

#### erlmcp_chaos_steady_state.erl
**Location:** `apps/erlmcp_observability/src/erlmcp_chaos_steady_state.erl`

**Features:**
- Steady state baseline capture and validation
- System-level metrics (processes, memory, I/O, GC)
- Component-level metrics (latency, throughput, errors)
- Deviation analysis with severity levels (ok/warning/critical)
- Configurable thresholds per metric type
- History tracking for trend analysis

**API:**
```erlang
erlmcp_chaos_steady_state:capture_steady_state() -> {ok, Snapshot}
erlmcp_chaos_steady_state:validate_steady_state(ThresholdPct) -> {ok, [Deviations]}
erlmcp_chaos_steady_state:set_baseline(Snapshot) -> ok
erlmcp_chaos_steady_state:get_baseline() -> {ok, Baseline}
```

#### erlmcp_chaos_fault_injector.erl
**Location:** `apps/erlmcp_observability/src/erlmcp_chaos_fault_injector.erl`

**Features:**
- Deterministic fault injection with seed control
- Message delay and drop injection
- Network partition simulation
- Process kill with monitoring
- Memory and CPU exhaustion
- Clock skew injection
- Scenario-based multi-fault execution
- Injection statistics tracking

**API:**
```erlang
erlmcp_chaos_fault_injector:inject_message_delay(Target, DelayMs) -> {ok, FaultId}
erlmcp_chaos_fault_injector:inject_network_partition(Nodes) -> {ok, FaultId}
erlmcp_chaos_fault_injector:inject_process_kill(Target) -> {ok, FaultId}
erlmcp_chaos_fault_injector:inject_memory_pressure(MemoryMB) -> {ok, FaultId}
erlmcp_chaos_fault_injector:inject_cpu_spin(DurationMs) -> {ok, FaultId}
erlmcp_chaos_fault_injector:start_scenario(Scenario) -> {ok, ScenarioId}
```

### 2. Test Suites

#### erlmcp_chaos_engineering_SUITE.erl
**Location:** `apps/erlmcp_validation/test/erlmcp_chaos_engineering_SUITE.erl`

**Test Categories (34 test cases):**

| Category | Tests | Purpose |
|----------|-------|---------|
| Fault Injection | 7 | Process kill, network latency, packet loss, jitter |
| Network Partitions | 3 | Split-brain, partial isolation, multi-node failure |
| Resource Exhaustion | 4 | Memory OOM, CPU spin, file descriptors, port limits |
| Cascade Failures | 4 | Supervisor tree, dependency chain, queue overflow, ETS |
| Recovery | 4 | Supervisor restart, state restoration, connections, data integrity |
| Steady State | 4 | Baseline, validation, post-chaos, deviation analysis |
| Game Days | 4 | System outage, region failure, DB disconnect, LB failure |

**Key Features:**
- Automatic baseline capture before each test
- Steady state validation after each test
- Comprehensive fault injection scenarios
- Game day scenario execution

#### erlmcp_chaos_mttr_SUITE.erl
**Location:** `apps/erlmcp_validation/test/erlmcp_chaos_mttr_SUITE.erl`

**MTTR Measurements (14 test cases):**

| Metric | Description |
|--------|-------------|
| TTD | Time To Detect - fault injection to detection |
| TTM | Time To Mitigate - detection to mitigation start |
| TTR | Time To Recover - mitigation to steady state |
| MTTR | Mean TTR across multiple samples |

**Component-Specific MTTR Tests:**
- Registry recovery
- Server recovery
- Client recovery
- Session recovery
- Transport recovery

**Analysis Tests:**
- Trend analysis (degrading/improving MTTR)
- Percentile analysis (p50, p90, p95, p99)
- SLA compliance by tier

### 3. Documentation

#### CHAOS_EXPERIMENT_GUIDE.md
**Location:** `docs/chaos_engineering/CHAOS_EXPERIMENT_GUIDE.md`

**Contents:**
- Chaos engineering principles
- Experiment design templates
- Fault type reference
- Steady state definitions
- Metrics guide
- Runbook procedures
- Game day scenarios
- Safety controls

#### CHAOS_RUNBOOK.md
**Location:** `docs/chaos_engineering/CHAOS_RUNBOOK.md`

**Contents:**
- Emergency procedures
- Rollback decision tree
- Pre/post experiment checklists
- Incident response procedures
- Monitoring commands
- Common scenario responses
- Recovery procedures
- Documentation templates

## Architecture

### Supervision Tree Integration

```
erlmcp_sup
  └── erlmcp_observability_sup
      ├── erlmcp_chaos (gen_server)
      ├── erlmcp_chaos_steady_state (gen_server)
      ├── erlmcp_chaos_fault_injector (gen_server)
      └── erlmcp_chaos_metrics (gen_server)
```

### Fault Injection Flow

```
Experiment Config
      ↓
erlmcp_chaos:run()
      ↓
Validation & Safety Checks
      ↓
erlmcp_chaos_worker_sup:start_worker()
      ↓
Fault Injection (via fault_injector)
      ↓
Metrics Collection (chaos_metrics)
      ↓
Steady State Validation
      ↓
Report Generation
```

## Running the Tests

### Run All Chaos Tests

```bash
# Run specific chaos test suite
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_chaos_engineering_SUITE

# Run MTTR benchmarks
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_chaos_mttr_SUITE

# Run all chaos-related tests
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_chaos_*_SUITE
```

### Manual Experiment Execution

```erlang
%% Start the shell
make console

%% Capture baseline
{ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state().

%% Run experiment
{ok, ExpId} = erlmcp_chaos:run(#{
    experiment => network_latency,
    latency => 1000,
    rate => 0.2,
    duration => 30000
}).

%% Monitor status
{ok, Status} = erlmcp_chaos:get_experiment_status(ExpId).

%% Validate steady state
{ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.1).

%% Stop and verify
erlmcp_chaos:stop_experiment(ExpId).
```

## Safety Features

### Blast Radius Limits
- Maximum concurrent experiments: 5
- Global blast radius: 50%
- Minimum healthy components: 70%
- Per-experiment blast radius: configurable

### Auto-Rollback Triggers
- Error rate > 25%
- Process count < 70%
- Memory usage > 90%
- Incident count > 5

### Safety Checks
- Pre-experiment validation
- Real-time monitoring
- Automatic experiment termination
- Graceful shutdown

## Metrics Collected

### System Metrics
- Total processes
- Run queue length
- Memory (total, processes, system, atom, binary, ETS)
- I/O (input/output bytes)
- GC (count, words reclaimed, pause time)

### Component Metrics
- Latency percentiles (p50, p95, p99, p999)
- Throughput (messages/second)
- Error rate and count
- Process count and heap size
- Message queue depth

### Chaos-Specific Metrics
- TTD (Time To Detect)
- TTM (Time To Mitigate)
- TTR (Time To Recover)
- MTTR (Mean Time To Recovery)
- Deviation severity
- Blast radius

## MTTR Targets

| Component | Target TTD | Target TTM | Target TTR |
|-----------|-----------|-----------|-----------|
| Registry | < 5s | < 2s | < 10s |
| Server | < 5s | < 2s | < 15s |
| Client | < 5s | < 2s | < 15s |
| Session | < 5s | < 3s | < 20s |
| Transport | < 5s | < 2s | < 15s |

## Game Day Scenarios

### Included Scenarios

1. **Full System Outage**
   - All services go down
   - Validate complete recovery
   - Target MTTR: < 30s

2. **Region Failure**
   - Simulate region isolation
   - Test failover mechanisms
   - Target MTTR: < 60s

3. **Database Disconnect**
   - Database becomes unavailable
   - Validate graceful degradation
   - Target MTTR: < 45s

4. **Load Balancer Failure**
   - LB stops distributing traffic
   - Test circuit breaker behavior
   - Target MTTR: < 20s

## Next Steps

### Recommended Usage

1. **Development Phase**
   - Run single fault injection tests
   - Validate component resilience
   - Establish baseline MTTR

2. **Staging Phase**
   - Run cascade failure tests
   - Execute game day scenarios
   - Validate runbooks

3. **Production Phase**
   - Start with 10% blast radius
   - Gradually increase to 20%
   - Continuous MTTR tracking

### Continuous Improvement

1. Track MTTR trends over time
2. Update runbooks based on findings
3. Add new fault types as needed
4. Refine thresholds based on production data

## File Reference

| File | Purpose |
|------|---------|
| `apps/erlmcp_observability/src/erlmcp_chaos.erl` | Main chaos orchestration |
| `apps/erlmcp_observability/src/erlmcp_chaos_process.erl` | Process fault injection |
| `apps/erlmcp_observability/src/erlmcp_chaos_network.erl` | Network fault injection |
| `apps/erlmcp_observability/src/erlmcp_chaos_resource.erl` | Resource fault injection |
| `apps/erlmcp_observability/src/erlmcp_chaos_metrics.erl` | Metrics collection |
| `apps/erlmcp_observability/src/erlmcp_chaos_steady_state.erl` | **NEW** - Steady state definitions |
| `apps/erlmcp_observability/src/erlmcp_chaos_fault_injector.erl` | **NEW** - Enhanced fault injection |
| `apps/erlmcp_validation/test/erlmcp_chaos_engineering_SUITE.erl` | **NEW** - Comprehensive test suite |
| `apps/erlmcp_validation/test/erlmcp_chaos_mttr_SUITE.erl` | **NEW** - MTTR benchmarks |
| `docs/chaos_engineering/CHAOS_EXPERIMENT_GUIDE.md` | **NEW** - Experiment guide |
| `docs/chaos_engineering/CHAOS_RUNBOOK.md` | **NEW** - Runbook procedures |
