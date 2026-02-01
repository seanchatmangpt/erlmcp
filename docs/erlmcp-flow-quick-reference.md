# erlmcp-flow Quick Reference Guide

**Status**: DRAFT | **Version**: 1.0.0 | **Date**: 2026-02-01

---

## 1. Four Subsystems at a Glance

### 1.1 Agent Foundation (erlmcp_core)

**Purpose**: Spawn, manage, and supervise intelligent agents.

**Key Functions**:
```erlang
spawn_agent(#{id, role, cognitive_pattern, capabilities})
    → {ok, #{id, pid, config, state, metrics}} | {error, Reason}

get_agent(agent_id)
    → {ok, agent()} | {error, not_found}

list_agents(#{role => atom(), state => atom()})
    → [agent()]

assign_task(agent_id, task())
    → {ok, task_id()} | {error, Reason}

set_agent_state(agent_id, agent_state())
    → ok | {error, Reason}

terminate_agent(agent_id)
    → ok | {error, Reason}
```

**Key Data Structures**:
```erlang
agent_config() = #{
    id => binary(),              % UUID
    role => worker | specialist | scout,
    cognitive_pattern => convergent | divergent | lateral | systems | critical | adaptive,
    capabilities => [atom()],    % [routing, learning, security, etc]
    learning_enabled => boolean(),
    max_concurrent_tasks => pos_integer(),
    timeout_ms => pos_integer()
}

agent() = #{
    id => binary(),
    pid => pid(),
    config => agent_config(),
    state => idle | active | learning | blocked | failed,
    created_at => timestamp(),
    updated_at => timestamp(),
    metrics => #{task_count => N, success_rate => F, ...}
}
```

**Supervision Tree**:
```
erlmcp_core_sup (one_for_one)
├── erlmcp_agent_spawner (gen_server)
│   └── agent_pool_worker_1..N (simple_one_for_one)
├── erlmcp_agent_registry (gproc wrapper)
├── erlmcp_agent_state_manager (gen_server)
│   └── Backend: ETS (fast) | DETS (durable) | Mnesia (cluster)
├── erlmcp_agent_lifecycle_fsm (gen_statem)
└── erlmcp_agent_health_monitor (gen_server)
```

---

### 1.2 Coordination (erlmcp_transports)

**Purpose**: Coordinate agents through topologies and message routing.

**Key Functions**:
```erlang
create_topology(mesh | hierarchical | ring | star, #{max_nodes, redundancy, ...})
    → {ok, topology()} | {error, Reason}

add_node_to_topology(topology_id, agent_id)
    → ok | {error, Reason}

send_message(from_agent_id, to_agent_id, message())
    → ok | {error, Reason}

broadcast_message(topology_id, source_agent_id, message())
    → {ok, count()} | {error, Reason}

create_swarm(#{agents => [agent_id()], topology => atom()})
    → {ok, swarm()} | {error, Reason}

propose_consensus(swarm_id, proposal())
    → {ok, proposal_id()} | {error, Reason}

vote_consensus(swarm_id, proposal_id, accept | reject)
    → ok | {error, Reason}
```

**Topology Types**:
```
MESH: All agents connected to all others
  ┌─ A ─┐
  │ ╱ ╲ │
  B ─── C

  Pros: Direct paths, low latency
  Cons: O(N²) edges, high overhead

HIERARCHICAL: Leader-follower tree
  ┌─ A (leader)
  ├─ B
  ├─ C
  ├─ D

  Pros: Low overhead, clear structure
  Cons: Single point of failure (leader)

RING: Circular chain
  A → B → C → D → A

  Pros: Minimal edges (O(N))
  Cons: High latency (N/2 hops average)

STAR: Central hub
  ┌─ B ─┐
  A     C
  └─ D ─┘

  Pros: Balanced latency (2 hops)
  Cons: Hub is bottleneck
```

**Key Data Structures**:
```erlang
topology() = #{
    id => topology_id(),
    type => mesh | hierarchical | ring | star,
    nodes => [agent_id()],
    edges => [{agent_id(), agent_id(), #{redundancy => 1..3}}],
    status => active | degraded | recovering,
    metadata => map()
}

swarm() = #{
    id => swarm_id(),
    agents => [agent_id()],
    topologies => [topology_id()],
    leader => {leader_id(), epoch()},
    consensus_state => map(),
    created_at => timestamp()
}
```

**Supervision Tree**:
```
erlmcp_transports_app
├── erlmcp_swarm_manager (gen_server)
├── erlmcp_topology_manager (gen_server)
├── erlmcp_consensus_manager (gen_server)
├── erlmcp_message_router (gen_server)
└── erlmcp_coordination_sup (one_for_one)
    ├── erlmcp_mesh_topology_worker (per topology)
    ├── erlmcp_hierarchical_topology_worker (per topology)
    ├── erlmcp_ring_topology_worker (per topology)
    └── erlmcp_star_topology_worker (per topology)
```

---

### 1.3 Intelligence (erlmcp_observability)

**Purpose**: Route agents intelligently, learn from execution, maintain semantic memory.

**Key Functions**:
```erlang
route_task(#{task_type, complexity, constraints, deadline_ms, cost_budget})
    → {ok, routing_decision()} | {error, Reason}

start_trajectory(agent_id, task())
    → {ok, trajectory_id()} | {error, Reason}

record_step(trajectory_id, #{action, result, quality, latency_ms})
    → ok | {error, Reason}

end_trajectory(trajectory_id, outcomes(), feedback())
    → ok | {error, Reason}

trigger_learning_cycle(#{
    min_trajectories => pos_integer(),
    consolidation_strategy => sona | ewc_plus_plus,
    preserve_critical => boolean()
})
    → {ok, learning_result()} | {error, Reason}

store_pattern(pattern(), confidence_score(), metadata())
    → {ok, pattern_id()} | {error, Reason}

search_patterns(query_text(), #{top_k, threshold, namespace})
    → {ok, [{pattern_id(), similarity_score(), pattern()}]} | {error, Reason}
```

**Key Data Structures**:
```erlang
routing_decision() = #{
    agent_id => agent_id(),
    confidence => float(),           % 0.0-1.0
    reasoning => {reason_type(), term()},
    latency_p95_ms => non_neg_integer(),
    success_rate => float(),
    alternatives => [agent_id()]
}

trajectory() = #{
    id => trajectory_id(),
    agent_id => agent_id(),
    task => task(),
    steps => [trajectory_step()],
    outcomes => [success | failure | timeout],
    reward => float(),
    feedback => map(),
    created_at => timestamp()
}

trajectory_step() = #{
    action => atom(),
    result => term(),
    quality => float(),              % 0.0-1.0
    latency_ms => non_neg_integer()
}
```

**Supervision Tree**:
```
erlmcp_observability_sup
├── erlmcp_routing_engine (gen_server)
│   └── erlmcp_router_state (ETS cache)
├── erlmcp_learning_system (gen_server)
│   ├── erlmcp_trajectory_recorder (gen_server)
│   ├── erlmcp_sona_consolidator (gen_server)
│   └── erlmcp_ewc_consolidator (gen_server)
├── erlmcp_memory_subsystem (gen_server)
│   ├── erlmcp_semantic_store (sql.js + HNSW)
│   ├── erlmcp_pattern_bank (vector store)
│   └── erlmcp_memory_reaper (TTL manager)
└── erlmcp_metrics_collector (gen_server)
    ├── erlmcp_histogram_aggregator
    └── erlmcp_percentile_estimator
```

---

### 1.4 Security (erlmcp_validation)

**Purpose**: Detect threats, audit operations, enforce compliance.

**Key Functions**:
```erlang
scan_input(input_text(), #{context})
    → {ok, [threat_assessment()]} | {error, Reason}

analyze_threat(threat_assessment(), scan_context())
    → {ok, detailed_analysis()} | {error, Reason}

log_audit_event(#{
    event_type => atom(),
    agent_id => binary(),
    user_id => binary(),
    action => term(),
    result => {ok|error, term()}
})
    → {ok, event_id()} | {error, Reason}

query_audit(#{agent_id, event_type, date_range, threat_level})
    → {ok, [audit_event()]} | {error, Reason}

check_compliance(agent_id, action(), context())
    → {compliant, details()} | {violates, policy_id(), details()}
```

**Key Data Structures**:
```erlang
threat_assessment() = #{
    threat_type => prompt_injection | jailbreak | pii_exposure |
                   privilege_escalation | data_exfiltration |
                   model_poisoning | token_theft | replay_attack,
    severity => critical | high | medium | low,
    confidence => float(),           % 0.0-1.0
    indicators => [atom()],
    recommended_action => block | sanitize | warn | log | escalate,
    detected_at => timestamp(),
    source_agent_id => agent_id()
}

audit_event() = #{
    event_id => uuid(),
    event_type => spawn_agent | terminate_agent | assign_task | learn |
                  route_decision | consensus_vote | threat_detected,
    timestamp => timestamp(),
    agent_id => agent_id(),
    user_id => binary(),
    action => term(),
    result => {ok|error, term()},
    pii_detected => boolean(),
    threat_level => critical | high | medium | low | none,
    metadata => map()
}
```

**Supervision Tree**:
```
erlmcp_validation_sup
├── erlmcp_aidefence_engine (gen_server)
│   ├── erlmcp_threat_detector (Soufflé + heuristics)
│   ├── erlmcp_pattern_matcher (similarity search)
│   └── erlmcp_pii_scanner (regex + ML)
├── erlmcp_audit_manager (gen_server)
│   ├── erlmcp_audit_logger (persistent)
│   ├── erlmcp_audit_indexer (full-text search)
│   └── erlmcp_chain_of_custody (immutable log)
├── erlmcp_compliance_validator (gen_server)
│   ├── erlmcp_policy_engine (rule interpreter)
│   └── erlmcp_compliance_reporter (generator)
└── erlmcp_secret_manager (gen_server)
    ├── erlmcp_encryption_service
    └── erlmcp_key_rotation_service
```

---

## 2. Inter-Subsystem Communication

### Message Flow Example: Agent Spawn + Route Task

```
User API
  │
  ├─→ spawn_agent(#{id: agent_1, role: worker, ...})
  │   │
  │   └─→ [Agent Foundation]
  │       erlmcp_agent_spawner
  │          │
  │          ├─→ create child (supervisor:start_child)
  │          ├─→ register (gproc)
  │          ├─→ init state
  │          └─→ {ok, agent{id, pid, state, metrics}}
  │
  ├─→ route_task(#{task_type: calculation, complexity: high, ...})
  │   │
  │   └─→ [Intelligence]
  │       erlmcp_routing_engine
  │          │
  │          ├─→ load metrics (ETS cache)
  │          ├─→ check capability
  │          ├─→ HNSW pattern match
  │          ├─→ model inference
  │          └─→ {ok, routing_decision{
  │              agent_id: agent_1,
  │              confidence: 0.92,
  │              ...
  │          }}
  │
  ├─→ assign_task(agent_1, task{...})
  │   │
  │   └─→ [Agent Foundation]
  │       erlmcp_agent_spawner
  │          │
  │          ├─→ check load (backpressure)
  │          ├─→ send task to agent_1
  │          ├─→ start timeout (30s)
  │          └─→ {ok, task_id}
  │
  (... agent executes task ...)
  │
  └─→ [Agent Foundation] receives completion
      erlmcp_agent_spawner
         │
         ├─→ mark task done
         ├─→ update metrics
         └─→ [Intelligence]
             erlmcp_learning_system.record_step(...)
                │
                ├─→ store trajectory
                ├─→ compute reward
                └─→ schedule learning cycle
```

---

## 3. Failure Scenarios Quick Reference

| Failure | Detection | Recovery | RTO | RPO |
|---------|-----------|----------|-----|-----|
| **F-1** Agent crash | Supervisor Down signal | Restart (≤5 in 60s) | <5s | <1s |
| **F-5** Message loss | ACK timeout | Retry (3×) + reroute | <30s | <100ms |
| **F-10** Stale metrics | Age check (>5min) | Use fallback + refresh | <1s | <5min |
| **F-15** AIDefence timeout | Timer expired | Allow + warn + log | <100ms | <1min |
| **F-17** Threat detected | Pattern match | Log + block + alert | <100ms | 0 |

---

## 4. Testing Quick Start

### Unit Tests (EUnit)

```bash
# Test single module
rebar3 eunit --module=erlmcp_agent_spawner_tests

# Test all agent foundation
rebar3 eunit --module=erlmcp_core,tests

# With coverage
rebar3 eunit --cover
```

### Integration Tests (Common Test)

```bash
# Test coordination subsystem
rebar3 ct --suite=apps/erlmcp_transports/test/erlmcp_swarm_manager_SUITE

# Test all
rebar3 ct
```

### Property Tests (Proper)

```bash
# Test agent spawn invariants
rebar3 eunit --module=erlmcp_agent_spawner_prop_tests
```

---

## 5. Key Invariants (Reminders)

### Safety (Nothing Bad Happens)
- I-1: One supervisor per agent ✓
- I-2: Valid state transitions only ✓
- I-3: Registry = canonical PID list ✓
- I-17: Audit log cryptographically immutable ✓
- I-20: Policy enforced BEFORE execution ✓

### Liveness (Something Good Happens)
- All tasks eventually complete or fail with reason ✓
- All messages eventually delivered (retry) or dropped with notification ✓
- All agents eventually recover or marked failed ✓

### Consistency
- Agent registry agrees across all queries (gproc) ✓
- Topology state consistent across nodes (gossip) ✓
- Audit trail total order (Lamport clocks) ✓

---

## 6. Deployment Checklist

- [ ] OTP 28.3.1 installed
- [ ] rebar3 configured
- [ ] All 4 apps compile (`make compile`)
- [ ] All tests pass (`make test`)
- [ ] Coverage ≥80% (`make coverage`)
- [ ] Dialyzer warnings = 0 (`make dialyzer`)
- [ ] Xref undefined = 0 (`make xref`)
- [ ] Code formatted (`rebar3 format`)
- [ ] ADRs reviewed (7 decision records)
- [ ] Supervision trees verified (no orphans)
- [ ] SLOs set (routing <500ms p99, consensus <500ms p99)
- [ ] Monitoring enabled (OTEL + Prometheus)
- [ ] Audit logging active (immutable chain)
- [ ] Security scan passed (AIDefence + PII)

---

## 7. Performance Targets (SLOs)

| Operation | Target | Max Acceptable |
|-----------|--------|-----------------|
| spawn_agent | <100ms p95 | <500ms p99 |
| route_task | <50ms p95 | <200ms p99 |
| send_message | <10ms p95 | <50ms p99 |
| scan_input (AIDefence) | <100ms p95 | <500ms p99 |
| audit_event (immutable) | <10ms p95 | <100ms p99 |
| pattern_search (HNSW) | <20ms p95 | <100ms p99 |
| consensus_vote | <200ms p95 | <500ms p99 |

---

## 8. Emergency Procedures

### Agent Spawner Overload
1. Check pool size: `erlmcp_agent_spawner:pool_size()`
2. Increase workers: `erlmcp_agent_spawner:add_workers(N)`
3. Monitor metrics: `erlmcp_metrics_collector:get_spawn_rate()`
4. If needed, scale pods (Kubernetes)

### Memory Leak (Pattern Store)
1. Check size: `erlmcp_memory_subsystem:get_store_size()`
2. Check TTL: `erlmcp_memory_reaper:check_expiry()`
3. Force eviction: `erlmcp_memory_subsystem:evict_lru(N)`
4. Restart if necessary: `erlmcp_observability_sup:restart_child(memory)`

### Audit Log Full
1. Check disk: `erlmcp_audit_manager:get_log_size()`
2. Archive old logs: `erlmcp_audit_manager:roll_partition()`
3. Configure rotation: `erlmcp_audit_manager:set_retention(30)` (days)

### Consensus Deadlock
1. Check leader: `erlmcp_consensus_manager:get_leader()`
2. Force new election: `erlmcp_consensus_manager:force_election(swarm_id)`
3. View proposals: `erlmcp_consensus_manager:list_proposals(swarm_id)`

---

## 9. Useful Commands

```bash
# System status
erlmcp:status().

# List all agents
erlmcp:agents().

# Get agent info
erlmcp:agent_info(agent_id).

# List swarms
erlmcp:swarms().

# Routing metrics
erlmcp:routing_metrics().

# Audit trail (last 100)
erlmcp:audit_tail(100).

# Memory usage
erlmcp:memory_stats().

# Reset learning system
erlmcp:learning_reset().

# Force consensus resolution
erlmcp:consensus_force_election(swarm_id).
```

---

## 10. Further Reading

- Full Architecture: `/home/user/erlmcp/docs/erlmcp-flow-architecture-design.md`
- C4 Diagrams: `/home/user/erlmcp/docs/erlmcp-flow-c4-architecture.md`
- ADRs: `/home/user/erlmcp/docs/adr/ADR-*.md`
- Implementation Guide: `/home/user/erlmcp/docs/erlmcp-flow-implementation-guide.md`

---

**Version**: 1.0.0 | **Last Updated**: 2026-02-01

