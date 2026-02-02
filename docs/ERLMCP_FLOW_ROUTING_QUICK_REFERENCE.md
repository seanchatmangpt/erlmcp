# erlmcp-flow Routing Layer - Quick Reference

**Version**: 1.0.0 | **Date**: 2026-02-01

---

## 1-Minute Overview

**Purpose**: Intelligent routing layer for erlmcp-flow that uses Q-Learning to select optimal agents based on load, success rate, and latency.

**Key Components**:
1. **Task Router** - Q-Learning based agent selection
2. **Service Discovery** - O(log N) gproc registry
3. **Load Balancing** - 4 strategies (round-robin, least-connections, weighted, consistent-hash)
4. **Failure Handling** - Retry, fallback, circuit breaker
5. **Request Correlation** - UUID-based distributed tracing
6. **Byzantine Tolerance** - 3f+1 quorum consensus

---

## Quick Command Reference

```bash
# Start routing layer
application:start(erlmcp_flow).

# Register agent
erlmcp_flow_registry:register_agent(
    <<"agent-01">>,
    self(),
    [<<"gen_server">>, <<"supervisor">>]
).

# Route task
{ok, AgentId, TraceCtx} = erlmcp_flow_router:route_task(
    #{type => <<"implement">>, file => <<"src/app.erl">>},
    #{capabilities => [<<"gen_server">>], priority => high}
).

# Check circuit breaker
erlmcp_flow_circuit_breaker:allow_request(<<"agent-01">>).

# Query Q-Learning metrics
erlmcp_flow_q_learning:get_metrics().
```

---

## Module Map (10 modules, 3,350 LOC)

| Module | Purpose | API |
|--------|---------|-----|
| `erlmcp_flow_sup` | Supervisor | `start_link/0` |
| `erlmcp_flow_router` | Main router | `route_task/2` |
| `erlmcp_flow_registry` | Service discovery | `register_agent/3`, `find_agent/1` |
| `erlmcp_flow_q_learning` | Q-Learning | `select_agent/1`, `update_reward/4` |
| `erlmcp_flow_load_balancer` | Load balancing | `select_agent/3` |
| `erlmcp_flow_failure_detector` | Retry/fallback | `retry_task/3`, `select_fallback/2` |
| `erlmcp_flow_circuit_breaker` | Circuit breaker | `allow_request/1`, `record_result/2` |
| `erlmcp_flow_correlation_tracker` | Tracing | `generate_trace_id/0`, `store_correlation/5` |
| `erlmcp_flow_byzantine_detector` | BFT | `verify_signature/2`, `add_response/3` |
| `erlmcp_flow_serializer` | Message format | `encode_routing_request/1` |

---

## Performance Targets

| Metric | Target | Acceptance |
|--------|--------|------------|
| Agent lookup | p99 < 100μs | < 200μs |
| Routing decision | p99 < 50ms | < 100ms |
| Q-Learning inference | avg < 100μs | < 500μs |
| Throughput | > 50K ops/sec | > 40K ops/sec |
| Memory (1K agents) | < 512MB | < 1GB |

---

## Q-Learning Cheat Sheet

**State Space** (99 states):
- `agent_load`: 0..10
- `success_rate_bucket`: low | medium | high
- `latency_bucket`: fast | normal | slow

**Action Space** (65 actions):
- 60 individual agents
- 5 swarms

**Hyperparameters**:
- α (learning rate) = 0.1
- γ (discount factor) = 0.9
- ε (exploration) = 0.1 (decays to 0.01)

**Reward Function**:
```erlang
Success: -Latency/1000 + Bonus
  Fast (<50ms): +1.0 bonus
  Slow (>200ms): -1.0 penalty
Failure: -10.0
```

---

## Circuit Breaker States

```
CLOSED → (failure_rate > 50%) → OPEN
OPEN → (timeout 30s) → HALF_OPEN
HALF_OPEN → (3 successes) → CLOSED
HALF_OPEN → (1 failure) → OPEN
```

**Thresholds**:
- Failure rate: > 50%
- Min requests: 10
- Timeout: 30s
- Half-open probes: 3

---

## Load Balancing Strategies

1. **Round-Robin**: Sequential distribution
   ```erlang
   erlmcp_flow_load_balancer:select_agent(Agents, round_robin, State)
   ```

2. **Least-Connections**: Minimum load
   ```erlang
   erlmcp_flow_load_balancer:select_agent(Agents, least_connections, State)
   ```

3. **Weighted**: Q-value based (softmax)
   ```erlang
   erlmcp_flow_load_balancer:select_agent(Agents, weighted, State)
   ```

4. **Consistent-Hash**: Sticky sessions
   ```erlang
   erlmcp_flow_load_balancer:select_agent(Agents, consistent_hash, State)
   ```

---

## Failure Handling Flow

```
1. Execute task on selected agent
   ↓ (failure)
2. Retry with exponential backoff (max 3 attempts)
   - Delay: 100ms, 200ms, 400ms
   ↓ (retry exhausted)
3. Select fallback agent
   - Query agents with same capabilities
   - Exclude failed agent
   ↓ (no fallback)
4. Select fallback swarm
   - Query swarms by capabilities
   ↓ (no swarm)
5. Return error
```

---

## Trace Context (W3C Format)

```
traceparent: 00-{trace_id}-{span_id}-{flags}

Example:
00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01

Components:
- Version: 00
- Trace ID: 32 hex chars
- Span ID: 16 hex chars
- Flags: 01 (sampled) or 00 (not sampled)
```

---

## Byzantine Fault Tolerance

**Quorum Formula**: 2F + 1 (where F = max faulty nodes)

**Examples**:
- F=1: Requires 3 nodes (tolerates 1 faulty)
- F=2: Requires 5 nodes (tolerates 2 faulty)
- F=3: Requires 7 nodes (tolerates 3 faulty)

**Message Signing**:
```erlang
Signature = crypto:mac(hmac, sha256, SecretKey, Data)
```

---

## gproc Keys

```erlang
% Agent name (O(log N) lookup)
{n, l, {flow_agent, AgentId}}

% Capability indexing (O(M) scan)
{p, l, {flow_capability, Capability}}

% Load counter (O(1) update)
{c, l, {flow_load, AgentId}}
```

---

## Configuration (sys.config)

```erlang
{erlmcp_flow, [
    {q_learning, #{
        epsilon => 0.1,
        alpha => 0.1,
        gamma => 0.9
    }},
    {circuit_breaker, #{
        failure_threshold => 5,
        timeout => 30000
    }},
    {byzantine, #{
        byzantine_tolerance => 1,
        quorum_size => 3
    }}
]}
```

---

## Test Commands

```bash
# Unit tests
rebar3 eunit --app erlmcp_flow

# Integration tests
rebar3 ct --dir apps/erlmcp_flow/test

# Benchmarks
rebar3 eunit --module erlmcp_flow_bench

# Property tests
rebar3 proper --module erlmcp_flow_proper_tests

# Coverage
rebar3 cover --verbose
```

---

## Monitoring Metrics (Prometheus)

```
# Routing latency
erlmcp_flow_routing_latency_seconds{quantile="0.99"}

# Success/failure counters
erlmcp_flow_routing_success_total
erlmcp_flow_routing_failure_total

# Agent load
erlmcp_flow_agent_load{agent_id="..."}

# Circuit breaker state
erlmcp_flow_circuit_breaker_state{agent_id="..."}

# Q-Learning metrics
erlmcp_flow_q_learning_episode_count
erlmcp_flow_q_learning_epsilon
```

---

## Common Patterns

### Pattern 1: Route with Timeout
```erlang
{ok, AgentId, Ctx} = erlmcp_flow_router:route_task_with_timeout(
    Task,
    Requirements,
    5000  % 5 second timeout
).
```

### Pattern 2: Capability Query
```erlang
Agents = erlmcp_flow_registry:query_agents([
    <<"gen_server">>,
    <<"supervisor">>
]).
```

### Pattern 3: Trace Chain
```erlang
Chain = erlmcp_flow_correlation_tracker:get_trace_chain(TraceId).
lists:foreach(
    fun(Entry) ->
        io:format("Span: ~p, Agent: ~p, Latency: ~pms~n",
                  [Entry#correlation_entry.span_id,
                   Entry#correlation_entry.agent_id,
                   timer:now_diff(Entry#correlation_entry.end_time,
                                  Entry#correlation_entry.start_time) div 1000])
    end,
    Chain
).
```

### Pattern 4: Manual Q-Table Update
```erlang
State = extract_state(AgentMetrics),
Action = <<"agent-01">>,
Reward = -LatencyMs / 1000.0,
NextState = extract_state(NextMetrics),

erlmcp_flow_q_learning:update_reward(State, Action, Reward, NextState).
```

---

## Troubleshooting

### Issue: High routing latency (p99 > 100ms)
**Solutions**:
- Check Q-Learning model convergence
- Verify gproc registry size (should be O(log N))
- Reduce epsilon for less exploration

### Issue: Circuit breaker opening frequently
**Solutions**:
- Increase failure threshold (default: 5)
- Increase timeout window (default: 30s)
- Check agent health

### Issue: No agents available
**Solutions**:
- Verify agent registration
- Check capability matching
- Review agent lifecycle

### Issue: Byzantine consensus failing
**Solutions**:
- Verify N ≥ 3F + 1
- Check message signatures
- Confirm network connectivity

---

## Next Steps

1. **Read Full Design**: `/home/user/erlmcp/docs/ERLMCP_FLOW_ROUTING_LAYER_DESIGN.md`
2. **Implementation Plan**: 48 hours (6 phases × 8 hours)
3. **Test Coverage**: ≥ 91% (278 tests)
4. **Benchmarks**: Validate all performance targets

---

## Support

For detailed algorithm explanations, see main design document sections:
- Section 2: Q-Learning Task Router
- Section 3: Service Discovery
- Section 4: Load Balancing
- Section 5: Failure Handling
- Section 6: Request Correlation
- Section 7: Byzantine Tolerance

**Status**: Design Complete, Ready for Implementation
