# Armstrong Supervision Principles Implementation

## Overview

The `pqc_armstrong_sup` module implements Joe Armstrong's core supervision philosophy for SwarmFlow PQChain, providing advanced supervision capabilities with hierarchical failure isolation, circuit breakers, hot code upgrades, and health monitoring.

## Joe Armstrong's Core Principles

### 1. Let It Crash
- **Philosophy**: Don't defensive program; let processes fail and restart
- **Implementation**:
  - Fail fast on unexpected input
  - No elaborate error handling where restart is simpler
  - Trust the supervisor to restore correct state
- **Example**: Pattern matching failures crash immediately rather than returning error tuples

### 2. Hierarchical Supervision
- **Philosophy**: Isolate failures at the right level
- **Implementation**:
  - **Error Kernel (Tier 1)**: Core services that must never fail
  - **Critical (Tier 2)**: Important services with controlled restart
  - **Normal (Tier 3)**: Standard workers with independent restart
  - **Peripheral**: Optional services that can fail without impact
- **Example**: Identity registry failure restarts entire error kernel, but contract worker failure is isolated

### 3. Share Nothing
- **Philosophy**: Processes communicate via message passing only
- **Implementation**:
  - Each child owns its state
  - No shared ETS between critical components
  - Clean process isolation
- **Example**: Each validator runs in isolated process with own state

### 4. Fail Fast
- **Philosophy**: Detect errors early and crash immediately
- **Implementation**:
  - No graceful degradation in error kernel
  - Crash on invariant violations
  - Let supervisor handle recovery
- **Example**: Invalid crypto policy immediately crashes rather than logging error

### 5. Hot Code Upgrade
- **Philosophy**: Update code without stopping the system
- **Implementation**:
  - Rolling upgrades per child
  - Version-aware state migration
  - Canary deployment support
- **Example**: Upgrade consensus algorithm without restarting blockchain

### 6. Error Kernel
- **Philosophy**: Small core that never fails
- **Implementation**:
  - Registry and identity services
  - Minimal, battle-tested code
  - one_for_all restart strategy
- **Example**: pqc_identity + pqc_crypto_policy + registry = error kernel

## Supervision Tier Architecture

### Tier 1: Error Kernel
```erlang
Policy = #supervision_policy{
    strategy = one_for_all,       % All must be available
    intensity = 3,                % Max 3 restarts
    period = 60,                  % Per 60 seconds
    escalation = fun error_kernel_escalation/3,
    circuit_breaker = undefined   % No CB for error kernel
}
```

**Components**:
- `pqc_identity` - Identity registry (must start first)
- `pqc_crypto_policy` - Crypto policy engine
- `pqc_registry` (gproc) - Process registry

**Restart Policy**: If one fails, restart all (shared dependencies)

**Use Cases**:
- Core blockchain services
- Crypto-agility engine
- Identity management

### Tier 2: Critical Services
```erlang
Policy = #supervision_policy{
    strategy = rest_for_one,      % Restart dependents
    intensity = 5,                % Max 5 restarts
    period = 60,                  % Per 60 seconds
    escalation = fun critical_tier_escalation/3,
    circuit_breaker = #circuit_breaker_config{
        failure_threshold = 5,
        recovery_time_ms = 30000,
        half_open_attempts = 3
    }
}
```

**Components**:
- `pqc_consensus_sup` - Consensus round supervisor
- `pqc_peer_sup` - Peer channel supervisor
- `pqc_mempool_sup` - Transaction mempool

**Restart Policy**: Ordered dependencies, restart dependent services

**Use Cases**:
- Consensus processes
- Peer-to-peer networking
- Transaction management

### Tier 3: Normal Services
```erlang
Policy = #supervision_policy{
    strategy = one_for_one,       % Independent restarts
    intensity = 10,               % Max 10 restarts
    period = 60,                  % Per 60 seconds
    escalation = fun normal_tier_escalation/3,
    circuit_breaker = #circuit_breaker_config{
        failure_threshold = 10,
        recovery_time_ms = 60000,
        half_open_attempts = 2
    }
}
```

**Components**:
- `pqc_contract_sup` - Smart contract instances
- Per-connection workers
- Request handlers

**Restart Policy**: Independent workers, isolated failures

**Use Cases**:
- Smart contracts (workflow nets)
- Client connections
- Per-session processes

### Tier 4: Peripheral Services
```erlang
Policy = #supervision_policy{
    strategy = one_for_one,
    intensity = 5,
    period = 60,
    escalation = fun peripheral_tier_escalation/3,
    circuit_breaker = undefined   % Not needed
}
```

**Components**:
- `pqc_a2a_bridge` - A2A protocol bridge (optional)
- `pqc_mcp_bridge` - MCP protocol bridge (optional)
- Observability services
- Metrics collectors

**Restart Policy**: Transient (only restart on abnormal termination)

**Use Cases**:
- Protocol bridges
- Monitoring/metrics
- Debug/profiling tools

## API Reference

### Starting a Supervisor

```erlang
%% Define supervision policy
Policy = #supervision_policy{
    strategy = one_for_one,
    intensity = 10,
    period = 60,
    escalation = fun custom_escalation/3,
    circuit_breaker = #circuit_breaker_config{
        failure_threshold = 5,
        recovery_time_ms = 30000,
        half_open_attempts = 3
    }
},

%% Start supervisor
{ok, Sup} = pqc_armstrong_sup:start_link(my_sup, Policy).
```

### Adding Children

```erlang
%% Define child specification
ChildSpec = #armstrong_child{
    id = my_worker,
    module = my_worker_module,
    args = [arg1, arg2],
    restart = permanent,
    shutdown = 5000,
    type = worker,
    tier = normal,
    max_restarts = 10,
    restart_window_sec = 60
},

%% Add child to supervisor
{ok, Pid} = pqc_armstrong_sup:add_child(Sup, ChildSpec).
```

### Removing Children

```erlang
%% Remove child safely
ok = pqc_armstrong_sup:remove_child(Sup, my_worker).
```

### Health Monitoring

```erlang
%% Get comprehensive health report
Health = pqc_armstrong_sup:get_health(Sup),

%% Health report includes:
%% - Per-child status and uptime
%% - Restart counts
%% - Circuit breaker states
%% - Isolated children
%% - Policy compliance

io:format("Total Restarts: ~p~n", [Health#health_report.total_restarts]),
io:format("Isolated: ~p~n", [Health#health_report.isolated_children]).
```

### Circuit Breaker

```erlang
%% Apply circuit breaker to child
Config = #circuit_breaker_config{
    failure_threshold = 5,        % Open after 5 failures
    recovery_time_ms = 30000,     % Wait 30s before half-open
    half_open_attempts = 3        % Allow 3 attempts in half-open
},

ok = pqc_armstrong_sup:apply_circuit_breaker(Sup, my_worker, Config).
```

**Circuit Breaker States**:
- **CLOSED** (normal): All requests pass, failures tracked
- **OPEN** (failure threshold exceeded): Requests fail fast
- **HALF-OPEN** (recovery): Limited requests allowed

### Hot Code Upgrade

```erlang
%% Perform rolling upgrade
ok = pqc_armstrong_sup:hot_code_upgrade(Sup, my_worker, my_worker_v2),

%% Upgrade process:
%% 1. Get current child process and state
%% 2. Load new module version
%% 3. Migrate state (calls NewModule:upgrade_state/1 if exists)
%% 4. Perform code change
%% 5. Resume with new code

%% Example upgrade_state/1 callback:
-module(my_worker_v2).
-export([upgrade_state/1]).

upgrade_state(OldState) ->
    %% Migrate state from v1 to v2
    NewState = convert_state(OldState),
    {ok, NewState}.
```

### Failure Isolation

```erlang
%% Isolate failing child (remove from supervision)
ok = pqc_armstrong_sup:isolate_failure(Sup, failing_worker),

%% Child is terminated but spec is kept
%% Used by escalation policy to prevent cascade failures
```

### Escalation

```erlang
%% Escalate failure to parent supervisor
ok = pqc_armstrong_sup:escalate(Sup, my_worker, reason),

%% Triggers parent's escalation policy
%% May trigger one_for_all restart at higher level
```

### Escalation Policy

```erlang
%% Set custom escalation policy
EscalationFun = fun(ChildId, Reason, RestartCount) ->
    case RestartCount of
        N when N < 3 -> restart;           % Normal restart
        N when N < 5 -> circuit_break;     % Apply circuit breaker
        N when N < 10 -> isolate;          % Isolate failing child
        _ -> escalate                       % Escalate to parent
    end
end,

ok = pqc_armstrong_sup:set_escalation_policy(Sup, EscalationFun).
```

## Circuit Breaker Pattern

### State Machine

```
CLOSED --[threshold failures]--> OPEN
  ^                                |
  |                                |
  |                           [timeout]
  |                                |
  |                                v
  +----[successes]---------- HALF-OPEN
                                   |
                             [failure]
                                   |
                                   v
                                 OPEN
```

### Configuration

```erlang
#circuit_breaker_config{
    failure_threshold = 5,      % Number of failures to open circuit
    recovery_time_ms = 30000,   % Time to wait before half-open
    half_open_attempts = 3      % Successful attempts to close circuit
}
```

### Benefits
- Prevents resource exhaustion
- Fail-fast on repeated errors
- Automatic recovery
- Protects downstream services

## Escalation Actions

### 1. RESTART
- Normal supervisor restart
- Increment failure count
- May trigger circuit breaker

```erlang
restart  % Simple restart, trust supervisor
```

### 2. ISOLATE
- Remove from active supervision
- Prevent cascade failures
- Requires manual intervention

```erlang
isolate  % Quarantine failing child
```

### 3. ESCALATE
- Notify parent supervisor
- Escalate up supervision tree
- May trigger one_for_all restart

```erlang
escalate  % Critical failure, parent must decide
```

### 4. CIRCUIT_BREAK
- Open circuit breaker
- Fail fast on new requests
- Auto-recovery after timeout

```erlang
circuit_break  % Temporary protection
```

## Complete Usage Example

```erlang
%%%-------------------------------------------------------------------
%%% Example: PQChain Supervision Tree
%%%-------------------------------------------------------------------

start_pqchain() ->
    %% Tier 1: Error Kernel (one_for_all)
    EKPolicy = #supervision_policy{
        strategy = one_for_all,
        intensity = 3,
        period = 60,
        escalation = fun error_kernel_escalation/3,
        circuit_breaker = undefined
    },
    {ok, EKSup} = pqc_armstrong_sup:start_link(pqc_error_kernel_sup, EKPolicy),

    %% Add identity registry
    {ok, _} = pqc_armstrong_sup:add_child(EKSup, #armstrong_child{
        id = pqc_identity,
        module = pqc_identity,
        args = [],
        restart = permanent,
        shutdown = 5000,
        type = worker,
        tier = error_kernel,
        max_restarts = 3,
        restart_window_sec = 60
    }),

    %% Add crypto policy
    {ok, _} = pqc_armstrong_sup:add_child(EKSup, #armstrong_child{
        id = pqc_crypto_policy,
        module = pqc_crypto_policy,
        args = [],
        restart = permanent,
        shutdown = 5000,
        type = worker,
        tier = error_kernel,
        max_restarts = 3,
        restart_window_sec = 60
    }),

    %% Tier 2: Critical Services (rest_for_one)
    CritPolicy = #supervision_policy{
        strategy = rest_for_one,
        intensity = 5,
        period = 60,
        escalation = fun critical_escalation/3,
        circuit_breaker = #circuit_breaker_config{
            failure_threshold = 5,
            recovery_time_ms = 30000,
            half_open_attempts = 3
        }
    },
    {ok, CritSup} = pqc_armstrong_sup:start_link(pqc_critical_sup, CritPolicy),

    %% Add consensus supervisor
    {ok, _} = pqc_armstrong_sup:add_child(CritSup, #armstrong_child{
        id = pqc_consensus_sup,
        module = pqc_consensus_sup,
        args = [],
        restart = permanent,
        shutdown = infinity,
        type = supervisor,
        tier = critical,
        max_restarts = 5,
        restart_window_sec = 60
    }),

    %% Monitor health
    spawn(fun() -> health_monitor_loop([EKSup, CritSup]) end),

    {ok, EKSup, CritSup}.

health_monitor_loop(Sups) ->
    timer:sleep(10000),  % Every 10 seconds
    lists:foreach(fun(Sup) ->
        Health = pqc_armstrong_sup:get_health(Sup),
        logger:info("Supervisor ~p health: ~p restarts, ~p isolated",
                   [Sup, Health#health_report.total_restarts,
                    length(Health#health_report.isolated_children)])
    end, Sups),
    health_monitor_loop(Sups).

error_kernel_escalation(_ChildId, _Reason, RestartCount) ->
    case RestartCount of
        N when N < 3 -> restart;
        _ -> escalate  % Critical: escalate immediately
    end.

critical_escalation(_ChildId, _Reason, RestartCount) ->
    case RestartCount of
        N when N < 3 -> restart;
        N when N < 5 -> circuit_break;
        _ -> escalate
    end.
```

## Testing (When Docker Available)

### Compilation
```bash
docker compose run --rm erlmcp-build rebar3 compile
```

### Unit Tests
```bash
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_armstrong_sup
```

### Common Tests
```bash
docker compose run --rm erlmcp-ct rebar3 ct --suite=pqc_armstrong_sup_SUITE
```

### Quality Checks
```bash
docker compose run --rm erlmcp-check rebar3 dialyzer
docker compose run --rm erlmcp-check rebar3 xref
```

## Best Practices

### 1. Tier Selection
- **Error Kernel**: Only for services required by everyone
- **Critical**: Services with dependencies
- **Normal**: Independent workers
- **Peripheral**: Optional, non-critical services

### 2. Restart Limits
- **Error Kernel**: Strict (3 in 60s)
- **Critical**: Moderate (5 in 60s)
- **Normal**: Relaxed (10 in 60s)
- **Peripheral**: Very relaxed (5 in 60s, transient)

### 3. Escalation Policy
- Start with `restart`
- Progress to `circuit_break` for repeated failures
- Use `isolate` for persistent failures
- Reserve `escalate` for critical errors

### 4. Circuit Breakers
- Apply to services with external dependencies
- Set threshold based on expected failure rate
- Use longer recovery times for expensive resources
- Monitor half-open success rate

### 5. Hot Upgrades
- Test state migration in development
- Implement `upgrade_state/1` callback
- Have rollback plan
- Monitor upgrade success rate
- Use canary deployments

## Integration with erlmcp OTP Patterns

### Follows erlmcp 3-Tier Supervision Invariant
- **Tier 1**: Application Supervisors (one_for_all) - Error Kernel
- **Tier 2**: Service Supervisors (simple_one_for_one) - Critical
- **Tier 3**: Isolated Workers (temporary) - Normal/Peripheral

### Compatible with erlmcp Libraries
- **gproc**: Registry integration for child lookups
- **gun**: HTTP/2 transport for remote supervision
- **ranch**: TCP protocol handling
- **poolboy**: Connection pool supervision

### Memory Guard Integration (OTP 28)
```erlang
%% Enable memory guards for supervised children
init([]) ->
    erlmcp_memory_guard:enable_context_guard(),
    {ok, #state{}}.
```

## References

- Joe Armstrong's Thesis: "Making reliable distributed systems in the presence of software errors"
- erlmcp OTP Patterns: `/home/user/erlmcp/docs/otp-patterns.md`
- Supervision Trees: https://www.erlang.org/doc/design_principles/sup_princ.html
- Circuit Breaker Pattern: Michael Nygard, "Release It!"

## Files

- `/home/user/erlmcp/apps/swarmflow_pqchain/src/pqc_armstrong_sup.erl` - Implementation
- `/home/user/erlmcp/apps/swarmflow_pqchain/src/pqc_armstrong_example.erl` - Usage examples
- `/home/user/erlmcp/apps/swarmflow_pqchain/include/pqchain.hrl` - Type definitions
