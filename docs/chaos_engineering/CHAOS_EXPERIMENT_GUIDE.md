# Chaos Engineering Experiment Guide

## Overview

This guide provides comprehensive documentation for designing, executing, and analyzing chaos experiments in erlmcp v3. The framework follows the Principles of Chaos Engineering to validate system resilience.

## Table of Contents

1. [Principles](#principles)
2. [Experiment Design](#experiment-design)
3. [Fault Types](#fault-types)
4. [Steady State](#steady-state)
5. [Metrics](#metrics)
6. [Runbooks](#runbooks)
7. [Game Days](#game-days)

## Principles

### The Four Principles of Chaos Engineering

1. **Define Steady State**: Measure system behavior under normal conditions
2. **Hypothesize**: Assume steady state continues in both control and experiment
3. **Inject Variables**: Introduce real-world events (faults)
4. **Disprove**: Look for differences between control and experiment

### erlmcp Chaos Engineering Principles

- **Let It Crash**: Process failures are expected and handled by supervisors
- **Isolation**: Faults are contained to prevent cascade failures
- **Observability**: All faults are measured and tracked
- **Recovery**: System must automatically return to steady state
- **Safety**: Blast radius limits prevent total system failure

## Experiment Design

### Experiment Template

```erlang
%% 1. Define steady state
{ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state().

%% 2. Configure experiment
Config = #{
    experiment => fault_type,
    target => component,
    rate => 0.1,              % 10% impact
    duration => 30000,         % 30 seconds
    max_blast_radius => 0.3,   % 30% max impact
    auto_rollback => true
}.

%% 3. Run experiment
{ok, ExpId} = erlmcp_chaos:run(ExpId, Config).

%% 4. Monitor deviation
{ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state().

%% 5. Verify recovery
ok = erlmcp_chaos:stop_experiment(ExpId).
{ok, FinalState} = erlmcp_chaos_steady_state:capture_steady_state().
```

### Blast Radius Calculation

```
Blast Radius = (Affected Components / Total Components) * Impact Factor

Safe Levels:
- Development: 0.5 (50%)
- Testing: 0.3 (30%)
- Staging: 0.2 (20%)
- Production: 0.1 (10%)
```

## Fault Types

### Process Faults

| Fault | Description | Use Case |
|-------|-------------|----------|
| `kill_random` | Kill random processes | Validate supervision |
| `kill_servers` | Kill specific servers | Component resilience |
| `kill_supervisor_tree` | Kill supervisor | Recovery hierarchy |

### Network Faults

| Fault | Description | Use Case |
|-------|-------------|----------|
| `network_latency` | Add delay to messages | Timeout handling |
| `packet_loss` | Drop messages | Retry logic |
| `network_partition` | Simulate split-brain | Distributed state |

### Resource Faults

| Fault | Description | Use Case |
|-------|-------------|----------|
| `resource_memory` | Memory pressure | GC behavior |
| `resource_cpu` | CPU saturation | Scheduler stress |
| `resource_disk` | Disk full | Error handling |

## Steady State

### System Metrics

```erlang
%% Capture baseline
{ok, Snapshot} = erlmcp_chaos_steady_state:capture_steady_state(5).

%% System metrics include:
- total_processes: Number of active processes
- run_queue: Process scheduler queue length
- memory: Memory breakdown (total, processes, system, atom, binary, ets)
- io: Input/output byte counts
- gc: Garbage collection stats
```

### Component Metrics

```erlang
%% Per-component metrics include:
- latency: p50, p95, p99, p999 percentiles
- throughput: Messages per second
- errors: Error rate and count
- processes: Count, heap size, reductions
- queues: Message queue statistics
```

### Validation Thresholds

```erlang
%% Default thresholds (percentage deviation)
- Latency: 20% warning, 50% critical
- Throughput: 15% warning, 30% critical
- Error Rate: 10% warning, 25% critical
- Memory: 30% warning, 50% critical
- Process Count: 20% warning, 40% critical
```

## Metrics

### MTTR (Mean Time To Recovery)

```erlang
%% MTTR is measured as:
MTTR = Time To Detect + Time To Mitigate + Time To Recover

%% Breakdown:
- TTD: Time from fault to detection
- TTM: Time from detection to mitigation start
- TTR: Time from mitigation to steady state
```

### MTTR Targets

| Component | TTD | TTM | TTR (Total) |
|-----------|-----|-----|-------------|
| Registry | < 5s | < 2s | < 10s |
| Server | < 5s | < 2s | < 15s |
| Client | < 5s | < 2s | < 15s |
| Session | < 5s | < 3s | < 20s |
| Transport | < 5s | < 2s | < 15s |

### Chaos Metrics Collection

```erlang
%% Record metrics during experiment
erlmcp_chaos_metrics:record_latency(server, request, DurationUs).
erlmcp_chaos_metrics:record_throughput(client, MsgCount, Timestamp).
erlmcp_chaos_metrics:record_memory(Pid, #{heap_mib => 10.5, rss_mib => 15.2}).
erlmcp_chaos_metrics:record_gc_pause(PauseUs).

%% Get summary
Metrics = erlmcp_chaos_metrics:get_all_metrics().
```

## Runbooks

### Pre-Experiment Checklist

- [ ] Capture baseline steady state
- [ ] Configure blast radius limits
- [ ] Set auto-rollback enabled
- [ ] Verify monitoring is active
- [ ] Document rollback procedure
- [ ] Notify stakeholders

### During Experiment

- [ ] Monitor steady state deviation
- [ ] Watch for cascading failures
- [ ] Track incident count
- [ ] Log all anomalies
- [ ] Be ready to abort

### Post-Experiment

- [ ] Verify steady state restored
- [ ] Collect all metrics
- [ ] Generate incident report
- [ ] Update runbooks
- [ ] Share findings

### Rollback Procedure

```erlang
%% Emergency rollback
erlmcp_chaos:stop_all_experiments().
erlmcp_chaos_fault_injector:clear_all_faults().

%% Verify recovery
{ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.1).
```

## Game Days

### Game Day Planning

1. **Objective**: Define what you're testing
2. **Scope**: Which components are affected
3. **Duration**: How long the experiment runs
4. **Participants**: Who is involved
5. **Success Criteria**: What makes it a pass

### Game Day Template

```yaml
name: "Database Connection Failure Game Day"
objective: "Validate system behavior when database becomes unavailable"
scope:
  - erlmcp_session_backend
  - erlmcp_registry
duration: 300000  % 5 minutes
participants:
  - Operations
  - Development
  - QA
success_criteria:
  - No data loss
  - MTTR < 60 seconds
  - Graceful degradation
scenarios:
  - step1: Disconnect database
  - step2: Verify failover
  - step3: Reconnect database
  - step4: Verify recovery
```

### Common Game Day Scenarios

#### 1. Full System Outage

```erlang
%% All services go down
Config = #{
    experiment => kill_servers,
    target => all,
    rate => 1.0,
    duration => 10000
}.
```

#### 2. Region Failure

```erlang
%% Simulate region isolation
Config = #{
    experiment => network_partition,
    nodes => [node(), 'backup@host'],
    duration => 30000
}.
```

#### 3. Database Disconnect

```erlang
%% Database becomes unavailable
Config = #{
    experiment => network_latency,
    target => erlmcp_session_backend,
    latency => 60000,  % 60 second timeout
    rate => 1.0
}.
```

#### 4. Load Balancer Failure

```erlang
%% Load balancer stops distributing
Config = #{
    experiment => packet_loss,
    target => erlmcp_server_sup,
    rate => 0.5,  % 50% packet loss
    duration => 20000
}.
```

## Safety Controls

### Auto-Rollback Triggers

- Error rate exceeds 25%
- Process count drops below 70%
- Memory usage exceeds 90%
- More than 5 incidents detected
- Experiment duration exceeded

### Blast Radius Limits

```erlang
%% Global limits
MaxConcurrentExperiments = 5
MaxGlobalBlastRadius = 0.5  % 50%
MinHealthyComponents = 0.7   % 70%
```

### Health Checks

```erlang
%% System health check
Health = erlmcp_health_monitor:get_system_health().

%% Component health check
Status = erlmcp_recovery_manager:get_circuit_status(ComponentId).
```

## Analysis

### Deviation Analysis

```erlang
%% Get deviations
{ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.1).

%% Filter by severity
Critical = [D || D <- Deviations, maps:get(severity, D) =:= critical].
Warnings = [D || D <- Deviations, maps:get(severity, D) =:= warning].
```

### Report Generation

```erlang
%% Get chaos report
Report = erlmcp_chaos:get_chaos_report().

%% Export to file
erlmcp_chaos_metrics:export_to_file("/tmp/chaos_report.json").
```

## Best Practices

1. **Start Small**: Begin with low blast radius
2. **Iterate**: Gradually increase chaos intensity
3. **Document**: Record all experiments and findings
4. **Automate**: Use CI/CD for regular chaos runs
5. **Review**: Analyze results and improve

## References

- [Principles of Chaos Engineering](https://principlesofchaos.org/)
- [erlmcp Chaos Module Documentation](../apps/erlmcp_observability/src/erlmcp_chaos.erl)
- [MTTR Benchmark Suite](../apps/erlmcp_validation/test/erlmcp_chaos_mttr_SUITE.erl)
- [Chaos Engineering Test Suite](../apps/erlmcp_validation/test/erlmcp_chaos_engineering_SUITE.erl)
