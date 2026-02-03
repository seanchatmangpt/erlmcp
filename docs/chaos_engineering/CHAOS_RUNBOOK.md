# Chaos Engineering Runbook

## Emergency Procedures

### Immediate Actions

**If the system appears unstable:**

1. Stop all chaos experiments immediately:
   ```erlang
   erlmcp_chaos:stop_all_experiments().
   ```

2. Clear all active faults:
   ```erlang
   erlmcp_chaos_fault_injector:clear_all_faults().
   ```

3. Verify system health:
   ```erlang
   Health = erlmcp_health_monitor:get_system_health().
   ```

4. Check component status:
   ```erlang
   Status = erlmcp_recovery_manager:get_all_recovery_status().
   ```

### Rollback Decision Tree

```
System Unstable?
  ├─ Yes → Stop Experiments → Recovered?
  │              ├─ Yes → Document & Investigate
  │              └─ No → Restart Components → Check Health
  └─ No → Continue Monitoring
```

## Experiment Procedures

### Pre-Experiment Setup

```erlang
%% 1. Start infrastructure
{ok, ChaosPid} = erlmcp_chaos:start_link().
{ok, SteadyPid} = erlmcp_chaos_steady_state:start_link().
{ok, FaultPid} = erlmcp_chaos_fault_injector:start_link().
{ok, MetricsPid} = erlmcp_chaos_metrics:start_link().

%% 2. Capture baseline
{ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state(5).
ok = erlmcp_chaos_steady_state:set_baseline(Baseline).

%% 3. Set safety limits
erlmcp_chaos:run(#{safety_checks => true,
                    max_blast_radius => 0.1,
                    auto_rollback => true}).
```

### Experiment Execution

```erlang
%% Define experiment
Experiment = #{
    experiment => network_latency,
    latency => 1000,           % 1 second
    rate => 0.2,               % 20% of traffic
    duration => 60000,         % 1 minute
    max_blast_radius => 0.1,   % 10% impact
    auto_rollback => true
}.

%% Run experiment
{ok, ExpId} = erlmcp_chaos:run(<<"latency_test">>, Experiment).

%% Monitor progress
{ok, Status} = erlmcp_chaos:get_experiment_status(ExpId).
```

### Post-Experiment Validation

```erlang
%% 1. Stop experiment
erlmcp_chaos:stop_experiment(ExpId).

%% 2. Verify steady state
{ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.1).

%% 3. Generate report
Report = erlmcp_chaos:get_chaos_report().

%% 4. Export metrics
erlmcp_chaos_metrics:export_to_file("chaos_report.json").
```

## Incident Response

### Severity Levels

| Level | Description | Action |
|-------|-------------|--------|
| P1 | System down | Stop all chaos, immediate rollback |
| P2 | Degraded performance | Monitor closely, reduce blast radius |
| P3 | Minor deviation | Continue experiment |
| P4 | Expected behavior | No action needed |

### Incident Response Flow

```
Incident Detected
  ↓
Assess Severity
  ↓
├─ P1 → Immediate Rollback → Post-Mortem
├─ P2 → Reduce Impact → Monitor
├─ P3 → Continue → Document
└─ P4 → Log → Continue
```

### Rollback Commands

```erlang
%% Level 1: Stop specific experiment
erlmcp_chaos:stop_experiment(ExpId).

%% Level 2: Stop all experiments
erlmcp_chaos:stop_all_experiments().

%% Level 3: Clear all faults
erlmcp_chaos_fault_injector:clear_all_faults().

%% Level 4: Restart components (supervisor will handle)
%% No action needed - supervisors auto-restart
```

## Monitoring

### Key Metrics to Watch

```erlang
%% Process health
ProcessCount = erlang:system_info(process_count).
RunQueue = erlang:statistics(run_queue).

%% Memory
MemoryTotal = erlang:memory(total).
MemoryProcesses = erlang:memory(processes).

%% GC
GCCount = erlang:statistics(garbage_collection).

%% Chaos status
ActiveExperiments = erlmcp_chaos:get_active_experiments().
ChaosReport = erlmcp_chaos:get_chaos_report().
```

### Health Check Script

```erlang
%% Comprehensive health check
check_system_health() ->
    %% Check 1: Process count stable
    ProcessCount = erlang:system_info(process_count),
    ct:log("Process count: ~p", [ProcessCount]),

    %% Check 2: Memory normal
    Memory = erlang:memory(total),
    ct:log("Memory: ~p MB", [Memory div (1024*1024)]),

    %% Check 3: No critical deviations
    {ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.2),
    Critical = [D || D <- Deviations,
                   maps:get(severity, D) =:= critical],
    case Critical of
        [] -> ok;
        _ -> ct:log("CRITICAL DEVIATIONS: ~p", [Critical])
    end.
```

## Common Scenarios

### Scenario 1: Process Kill Storm

**Symptoms:**
- Rapid process restarts
- High CPU from supervisor activity
- Increased error rate

**Response:**
```erlang
%% 1. Check experiment status
{ok, Status} = erlmcp_chaos:get_experiment_status(ExpId).

%% 2. If kill rate too high, stop
case maps:get(targets_affected, Status) of
    N when N > 100 ->
        erlmcp_chaos:stop_experiment(ExpId);
    _ ->
        ok
end.

%% 3. Verify recovery
timer:sleep(5000),
check_system_health().
```

### Scenario 2: Memory Exhaustion

**Symptoms:**
- High memory usage
- Frequent GC
- Slow response times

**Response:**
```erlang
%% 1. Check memory
Memory = erlang:memory(total),
MemoryMB = Memory div (1024*1024),

%% 2. If over 80%, stop memory experiment
if MemoryMB > 800 ->
    erlmcp_chaos:stop_all_experiments(),
    erlang:garbage_collect();
   true ->
    ok
end.
```

### Scenario 3: Network Partition

**Symptoms:**
- Nodes unreachable
- Split-brain warnings
- Inconsistent state

**Response:**
```erlang
%% 1. Stop partition experiment
erlmcp_chaos:stop_experiment(ExpId).

%% 2. Verify connectivity
Nodes = nodes(),
ct:log("Connected nodes: ~p", [Nodes]),

%% 3. Check for split brain
case check_split_brain() of
    true ->
        %% Need manual intervention
        ct:log("SPLIT BRAIN DETECTED - Manual intervention required");
    false ->
        ok
end.
```

## Recovery Procedures

### Automatic Recovery

```erlang
%% Supervisors automatically restart children
%% No manual intervention needed

%% Recovery manager tracks progress
{ok, Status} = erlmcp_recovery_manager:get_recovery_status(ComponentId).

%% Circuit breaker status
CircuitState = erlmcp_recovery_manager:get_circuit_status(ComponentId).
```

### Manual Recovery

```erlang
%% If automatic recovery fails:

%% 1. Check supervisor state
Children = supervisor:which_children(erlmcp_core_sup).

%% 2. Restart specific child
supervisor:restart_child(erlmcp_core_sup, ChildId).

%% 3. If restart fails, terminate and restart
supervisor:terminate_child(erlmcp_core_sup, ChildId),
supervisor:restart_child(erlmcp_core_sup, ChildId).
```

## Documentation

### Experiment Log Template

```markdown
## Experiment Log: [NAME]

**Date:** YYYY-MM-DD
**Experimenter:** Name
**Objective:** What we're testing

### Setup
- Baseline: [metrics]
- Configuration: [config]

### Execution
- Start time: [timestamp]
- Faults injected: [list]
- Deviations observed: [list]

### Results
- End time: [timestamp]
- Final state: [metrics]
- Recovery time: [duration]
- Status: Pass/Fail

### Lessons Learned
- What worked
- What didn't
- Action items
```

## Checklist

### Pre-Experiment
- [ ] Baseline captured
- [ ] Blast radius configured
- [ ] Auto-rollback enabled
- [ ] Monitoring active
- [ ] Team notified
- [ ] Rollback plan ready

### During Experiment
- [ ] Monitoring deviations
- [ ] Logging incidents
- [ ] Checking thresholds
- [ ] Ready to abort

### Post-Experiment
- [ ] All experiments stopped
- [ ] Steady state verified
- [ ] Report generated
- [ ] Findings documented
- [ ] Team debriefed
- [ ] Action items assigned
