# Chaos Engineering Guide

## Overview

The erlmcp chaos engineering framework provides controlled failure injection for resilience testing. It includes network failures, process crashes, resource exhaustion, and comprehensive safety controls.

## Architecture

```
erlmcp_chaos (orchestrator)
├── erlmcp_chaos_network     - Network failure injection
├── erlmcp_chaos_process     - Process crash scenarios
└── erlmcp_chaos_resource    - Resource exhaustion
```

Integration with:
- `erlmcp_health_monitor` - System health monitoring
- `erlmcp_recovery_manager` - Automatic recovery
- `erlmcp_bench_chaos` - Chaos benchmarking

## Quick Start

### Basic Usage

```erlang
% Start chaos framework
{ok, _Pid} = erlmcp_chaos:start_link().

% Run a simple experiment
Config = #{
    experiment => network_latency,
    latency => 100,        % 100ms delay
    rate => 0.2,           % Affect 20% of traffic
    duration => 300000     % 5 minutes
}.

{ok, ExperimentId} = erlmcp_chaos:run(Config).

% Monitor experiment
{ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId).

% Stop experiment
ok = erlmcp_chaos:stop_experiment(ExperimentId).
```

### Dry Run (Safe Mode)

```erlang
% Test experiment without actual damage
{ok, Result} = erlmcp_chaos:dry_run(Config).

% Result includes:
% - estimated_affected: Number of targets
% - estimated_blast_radius: Impact percentage
% - safety_checks: Pass/fail status
% - risks: Identified risks
% - recommendations: Best practices
```

## Experiment Types

### Network Chaos

#### Latency Injection
```erlang
erlmcp_chaos:run(#{
    experiment => network_latency,
    latency => 100,           % Milliseconds
    rate => 0.2,              % 20% of requests
    interval => 30000,        % Check every 30s
    duration => 300000,       % 5 minutes
    max_blast_radius => 0.3   % Max 30% affected
}).
```

#### Network Partition
```erlang
erlmcp_chaos:run(#{
    experiment => network_partition,
    nodes => [node1@host, node2@host],
    duration => 60000         % 1 minute partition
}).
```

#### Packet Loss
```erlang
erlmcp_chaos:run(#{
    experiment => packet_loss,
    rate => 0.1,              % 10% packet loss
    duration => 300000
}).
```

### Process Chaos

#### Kill Random Processes
```erlang
erlmcp_chaos:run(#{
    experiment => kill_random,
    rate => 0.1,              % 10% of processes
    interval => 30000,        % Every 30 seconds
    duration => 300000
}).
```

#### Kill Specific Server Type
```erlang
erlmcp_chaos:run(#{
    experiment => kill_servers,
    target => erlmcp_server,  % Process name prefix
    rate => 0.1,
    interval => 30000,
    duration => 300000
}).
```

#### Supervisor Cascade
```erlang
erlmcp_chaos:run(#{
    experiment => supervisor_cascade,
    target => erlmcp_client_sup,
    recovery_time_expected => 5000  % 5 seconds
}).
```

### Resource Chaos

#### Memory Exhaustion
```erlang
erlmcp_chaos:run(#{
    experiment => resource_memory,
    target_percent => 0.85,   % 85% memory usage
    duration => 300000,
    auto_rollback => true     % Auto release on critical
}).
```

#### CPU Saturation
```erlang
erlmcp_chaos:run(#{
    experiment => resource_cpu,
    target_load => 1.0,       % 100% CPU
    duration => 60000
}).
```

#### Disk Full
```erlang
erlmcp_chaos:run(#{
    experiment => resource_disk,
    target_percent => 0.95,   % 95% disk usage
    temp_file => "/tmp/chaos",
    duration => 300000
}).
```

## Safety Controls

### Blast Radius Limits

```erlang
% Per-experiment limit
Config = #{
    experiment => kill_servers,
    rate => 0.3,
    max_blast_radius => 0.3   % Max 30% of targets
}.

% Global limit (in framework init)
erlmcp_chaos:start_link([
    {safety_enabled, true},
    {global_limits, #{
        max_global_blast_radius => 0.5,  % Max 50% across all experiments
        max_concurrent_experiments => 5,
        min_healthy_components => 0.7     % 70% must stay healthy
    }}
]).
```

### Automatic Rollback

The framework automatically stops experiments if:
- Blast radius exceeds limits
- System health becomes critical
- SLA thresholds are violated
- Manual stop is triggered

```erlang
Config = #{
    experiment => resource_memory,
    target_percent => 0.90,
    auto_rollback => true,     % Enable auto-rollback
    sla_threshold => #{
        p95_latency_ms => 1000,  % Stop if p95 > 1s
        error_rate => 0.05       % Stop if errors > 5%
    }
}.
```

### Health Monitoring Integration

```erlang
% Framework checks health monitor automatically
erlmcp_chaos:start_link([
    {monitor_integration, true}  % Enable health checks
]).

% Experiments are automatically stopped if:
% - System health becomes unhealthy
% - Component health drops below threshold
% - Circuit breakers are triggered
```

## Monitoring & Reporting

### Get Active Experiments

```erlang
ActiveExperiments = erlmcp_chaos:get_active_experiments().
% Returns: [#{id, type, state, start_time, blast_radius, ...}, ...]
```

### Experiment Status

```erlang
{ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId).

% Status includes:
% - state: running | stopped | failed | completed
% - targets_affected: Count of affected targets
% - total_targets: Total possible targets
% - blast_radius: Current impact (0.0 to 1.0)
% - incidents: List of incidents during experiment
% - metrics: Performance metrics
```

### Comprehensive Report

```erlang
Report = erlmcp_chaos:get_chaos_report().

% Report includes:
% - total_experiments: All experiments run
% - active_experiments: Currently running
% - completed_experiments: Successfully completed
% - failed_experiments: Failed or stopped early
% - total_incidents: Sum of all incidents
% - current_blast_radius: System-wide impact
% - experiments: Detailed list of all experiments
```

## Integration with Benchmarks

### Run Chaos During Benchmarks

```erlang
% Start chaos experiment
{ok, ChaosId} = erlmcp_chaos:run(#{
    experiment => network_latency,
    latency => 100,
    rate => 0.2,
    duration => 300000
}).

% Run benchmark during chaos
Result = erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>).

% Stop chaos
erlmcp_chaos:stop_experiment(ChaosId).

% Compare results with baseline (no chaos)
```

### Chaos Benchmark Suite

```erlang
% Run comprehensive chaos benchmarks
{ok, Results} = erlmcp_bench_chaos:run_all_scenarios().

% Results include:
% - 11 chaos scenarios
% - Detection times
% - Recovery times
% - Refusal code validation
% - Bounded refusal checks
```

## Best Practices

### Start Small

```erlang
% Phase 1: Low impact, short duration
erlmcp_chaos:run(#{
    experiment => kill_servers,
    rate => 0.05,           % 5% only
    duration => 60000       % 1 minute
}).

% Phase 2: Increase gradually
erlmcp_chaos:run(#{
    experiment => kill_servers,
    rate => 0.1,            % 10%
    duration => 300000      % 5 minutes
}).

% Phase 3: Full resilience test
erlmcp_chaos:run(#{
    experiment => kill_servers,
    rate => 0.3,            % 30%
    duration => 1800000     % 30 minutes
}).
```

### Use Dry Run First

```erlang
% Always test with dry run
{ok, DryRunResult} = erlmcp_chaos:dry_run(Config).

% Review risks and recommendations
Risks = maps:get(risks, DryRunResult),
Recommendations = maps:get(recommendations, DryRunResult),

% Then run actual experiment
{ok, ExperimentId} = erlmcp_chaos:run(Config).
```

### Monitor Continuously

```erlang
% Set up monitoring before experiment
timer:send_interval(5000, self(), check_chaos_status).

% In your receive loop:
receive
    check_chaos_status ->
        Status = erlmcp_chaos:get_experiment_status(ExperimentId),
        Incidents = maps:get(incidents, Status),
        case length(Incidents) > 10 of
            true ->
                erlmcp_chaos:stop_experiment(ExperimentId),
                ?LOG_WARNING("Stopping chaos due to too many incidents");
            false ->
                ok
        end
end.
```

### Enable All Safety Controls

```erlang
erlmcp_chaos:start_link([
    {safety_enabled, true},
    {monitor_integration, true},
    {global_limits, #{
        max_global_blast_radius => 0.3,  % Conservative limit
        max_concurrent_experiments => 3,
        min_healthy_components => 0.8
    }}
]).
```

### Test Recovery Mechanisms

```erlang
% 1. Test supervisor restart
erlmcp_chaos:run(#{
    experiment => kill_servers,
    target => erlmcp_server,
    rate => 0.1
}).

% Verify: Supervisor restarts processes within 1s

% 2. Test circuit breakers
erlmcp_chaos:run(#{
    experiment => network_latency,
    latency => 5000,  % High latency
    rate => 0.5
}).

% Verify: Circuit breakers open, system degrades gracefully

% 3. Test memory backpressure
erlmcp_chaos:run(#{
    experiment => resource_memory,
    target_percent => 0.90
}).

% Verify: Refusal code 1089, no OOM, graceful degradation
```

## Troubleshooting

### Experiment Won't Start

```erlang
% Check safety constraints
{error, {safety_violation, Reason}} = erlmcp_chaos:run(Config).

% Common reasons:
% - Blast radius too high
% - Too many concurrent experiments
% - System health already degraded
% - Invalid configuration
```

### Experiment Stops Unexpectedly

```erlang
% Check experiment status
{ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId).

% If state is 'failed', check incidents:
Incidents = maps:get(incidents, Status).

% Common causes:
% - SLA violation (auto-rollback triggered)
% - System health became critical
% - Worker process crashed
```

### High Incident Count

```erlang
% Review incidents
{ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),
Incidents = maps:get(incidents, Status).

% If too many incidents:
% 1. Reduce rate
% 2. Increase interval
% 3. Strengthen recovery mechanisms
```

## Examples

### Example 1: Gradual Resilience Test

```erlang
% Test system resilience with gradually increasing chaos
test_resilience_gradual() ->
    Phases = [
        #{rate => 0.05, duration => 60000},   % 5% for 1 min
        #{rate => 0.10, duration => 120000},  % 10% for 2 min
        #{rate => 0.20, duration => 300000}   % 20% for 5 min
    ],
    
    lists:foreach(
        fun(Phase) ->
            Config = Phase#{
                experiment => kill_servers,
                target => erlmcp_server
            },
            {ok, ExpId} = erlmcp_chaos:run(Config),
            
            % Wait for phase to complete
            timer:sleep(maps:get(duration, Phase)),
            
            % Check results
            {ok, Status} = erlmcp_chaos:get_experiment_status(ExpId),
            ?LOG_INFO("Phase complete: ~p", [Status])
        end,
        Phases
    ).
```

### Example 2: Network Partition Test

```erlang
% Test distributed system behavior during network partition
test_network_partition() ->
    % Start experiment
    {ok, ExpId} = erlmcp_chaos:run(#{
        experiment => network_partition,
        nodes => [node1@host, node2@host],
        duration => 60000
    }),
    
    % During partition, verify:
    % - No split-brain
    % - Data consistency
    % - Graceful degradation
    
    timer:sleep(30000),
    
    % Check cluster state
    ClusterStatus = check_cluster_consistency(),
    
    % Wait for partition to heal
    timer:sleep(30000),
    
    % Verify recovery
    ok = erlmcp_chaos:stop_experiment(ExpId),
    PostRecoveryStatus = check_cluster_consistency(),
    
    {ClusterStatus, PostRecoveryStatus}.
```

### Example 3: Resource Exhaustion Recovery

```erlang
% Test recovery from memory exhaustion
test_memory_recovery() ->
    % Get baseline metrics
    BaselineMemory = erlang:memory(total),
    
    % Start memory exhaustion
    {ok, ExpId} = erlmcp_chaos:run(#{
        experiment => resource_memory,
        target_percent => 0.85,
        duration => 60000,
        auto_rollback => true
    }),
    
    % Monitor memory usage
    timer:sleep(30000),
    PeakMemory = erlang:memory(total),
    
    % Wait for experiment to complete
    timer:sleep(35000),
    
    % Check recovery
    RecoveredMemory = erlang:memory(total),
    
    #{
        baseline => BaselineMemory,
        peak => PeakMemory,
        recovered => RecoveredMemory,
        recovery_percentage => (BaselineMemory / RecoveredMemory) * 100
    }.
```

## Dashboard Integration

The chaos framework integrates with the erlmcp dashboard:

```
http://localhost:8080/chaos

Features:
- Active experiments view
- Real-time blast radius visualization
- Incident timeline
- Safety status indicators
- One-click stop/rollback
```

## API Reference

See module documentation:
- `erlmcp_chaos` - Main orchestration
- `erlmcp_chaos_network` - Network primitives
- `erlmcp_chaos_process` - Process primitives
- `erlmcp_chaos_resource` - Resource primitives

## Further Reading

- [Chaos Engineering Principles](https://principlesofchaos.org/)
- [Netflix Chaos Monkey](https://netflix.github.io/chaosmonkey/)
- [Erlang Supervisor Design Principles](https://www.erlang.org/doc/design_principles/sup_princ.html)
