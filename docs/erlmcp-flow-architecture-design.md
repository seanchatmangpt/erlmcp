# erlmcp-flow Architecture Design v1.0.0

**Status**: DRAFT | **Date**: 2026-02-01 | **Author**: System Architecture Designer

---

## Executive Summary

**erlmcp-flow** is a multi-agent orchestration system built on top of erlmcp that enables:
- Dynamic agent spawning and lifecycle management
- Swarm coordination with multiple topologies (mesh, hierarchical, ring, star)
- Intelligent routing and adaptive learning
- Comprehensive security, validation, and audit trails

The architecture comprises **4 core subsystems** mapping to erlmcp's **4 OTP applications**:

| Subsystem | App | Purpose | Modules |
|-----------|-----|---------|---------|
| **Agent Foundation** | erlmcp_core | Agent spawn, state, lifecycle | agent_*, lifecycle_*, state_* |
| **Coordination** | erlmcp_transports | Inter-agent messaging, topology | swarm_*, coordination_*, topology_* |
| **Intelligence** | erlmcp_observability | Routing, learning, memory, metrics | routing_*, learning_*, memory_*, pattern_* |
| **Security** | erlmcp_validation | AIDefence, audit, compliance | security_*, audit_*, defence_* |

**Quality Gates**: Compilation ✓ | Tests ✓ | Coverage ≥80% | Dialyzer ✓ | Xref ✓

---

## Part I: Subsystem Definitions

### 1. Agent Foundation (erlmcp_core)

**Responsibility**: Spawn, manage, and supervise intelligent agents throughout their lifecycle.

**Key Processes**:
```
erlmcp_core_sup
├── erlmcp_agent_spawner (gen_server)
│   └── Pool: agent_pool_worker_* (simple_one_for_one)
├── erlmcp_agent_registry (gproc)
├── erlmcp_agent_state_manager (gen_server)
│   └── Backend: {ETS | DETS | Mnesia}
├── erlmcp_agent_lifecycle_fsm (gen_statem)
└── erlmcp_agent_health_monitor (gen_server)
```

**Type Hierarchy**:
```erlang
-type agent_id() :: binary().                          % UUID
-type agent_role() :: atom().                          % worker|specialist|scout
-type agent_state() :: idle | active | learning | blocked | failed.
-type agent_config() :: #{
    id := agent_id(),
    role := agent_role(),
    cognitive_pattern := convergent|divergent|lateral|systems|critical|adaptive,
    capabilities := [capability()],
    learning_enabled := boolean(),
    max_concurrent_tasks := non_neg_integer(),
    timeout_ms := pos_integer()
}.

-type agent() :: #{
    id := agent_id(),
    pid := pid(),
    config := agent_config(),
    state := agent_state(),
    created_at := non_neg_integer(),
    updated_at := non_neg_integer(),
    metrics := agent_metrics()
}.
```

**Core API**:
```erlang
%% Spawn a new agent
-spec spawn_agent(agent_config()) -> {ok, agent()} | {error, term()}.

%% Query agent state
-spec get_agent(agent_id()) -> {ok, agent()} | {error, not_found}.
-spec list_agents(#{role => agent_role(), state => agent_state()}) -> [agent()].

%% Update agent state
-spec set_agent_state(agent_id(), agent_state()) -> ok | {error, term()}.
-spec add_agent_metric(agent_id(), metric_name(), metric_value()) -> ok.

%% Agent lifecycle
-spec terminate_agent(agent_id()) -> ok | {error, term()}.
-spec pause_agent(agent_id()) -> ok | {error, term()}.
-spec resume_agent(agent_id()) -> ok | {error, term()}.

%% Assign work
-spec assign_task(agent_id(), task()) -> {ok, task_id()} | {error, term()}.
-spec get_agent_load(agent_id()) -> {ok, integer()} | {error, term()}.
```

**Invariants**:
- I-1: ∀a ∈ agents. ∃!supervisor. supervises(a) ∧ running(supervisor)
- I-2: Agent state transitions: idle → active → {learning,blocked} → idle | failed
- I-3: ∀a ∈ agents. registry[a.id] = a.pid (registration is consistent)
- I-4: Agent spawn always returns config + pid or error (never partial state)
- I-5: Max concurrent tasks enforced per-agent (backpressure)

**Failure Modes**:
- **F-1**: Agent crash → supervisor restarts (max_restarts before giving up)
- **F-2**: Registry unavailable → spawn fails (return error, don't create orphan)
- **F-3**: State backend down → agent continues (state ops cached locally)
- **F-4**: Task assignment overflow → queue with timeout (prevent mailbox explosion)

---

### 2. Coordination (erlmcp_transports)

**Responsibility**: Coordinate agents through message passing and swarm topologies.

**Key Processes**:
```
erlmcp_transports_app
├── erlmcp_swarm_manager (gen_server)
│   ├── erlmcp_topology_manager (gen_server)
│   ├── erlmcp_consensus_manager (gen_server)
│   └── erlmcp_message_router (gen_server)
├── erlmcp_coordination_sup (one_for_one)
│   ├── erlmcp_mesh_topology_worker (gen_server)
│   ├── erlmcp_hierarchical_topology_worker (gen_server)
│   ├── erlmcp_ring_topology_worker (gen_server)
│   └── erlmcp_star_topology_worker (gen_server)
└── erlmcp_transport_messages (message encoder/decoder)
```

**Topology Types**:
```erlang
-type topology_type() :: mesh | hierarchical | ring | star.

-type topology_config() :: #{
    type := topology_type(),
    max_nodes := pos_integer(),
    redundancy := 1..3,                    % # of backup paths
    consensus_algorithm := raft | byzantine | gossip | crdt,
    sync_timeout_ms := pos_integer(),
    heartbeat_interval_ms := pos_integer()
}.

-type topology() :: #{
    id := topology_id(),
    type := topology_type(),
    nodes := [node_ref()],                 % agent IDs
    edges := [{agent_id(), agent_id(), edge_config()}],
    status := active | degraded | recovering,
    metadata := map()
}.

-type swarm() :: #{
    id := swarm_id(),
    agents := [agent_id()],
    topologies := [topology()],
    consensus_state := term(),
    leader := {leader_id(), epoch()},
    created_at := timestamp()
}.
```

**Core API**:
```erlang
%% Topology management
-spec create_topology(topology_type(), topology_config()) -> {ok, topology()} | {error, term()}.
-spec add_node_to_topology(topology_id(), agent_id()) -> ok | {error, term()}.
-spec remove_node_from_topology(topology_id(), agent_id()) -> ok | {error, term()}.
-spec get_topology(topology_id()) -> {ok, topology()} | {error, not_found}.

%% Swarm management
-spec create_swarm(#{agents := [agent_id()], topology := topology_type()})
    -> {ok, swarm()} | {error, term()}.
-spec dissolve_swarm(swarm_id()) -> ok | {error, term()}.
-spec add_agent_to_swarm(swarm_id(), agent_id()) -> ok | {error, term()}.

%% Message routing
-spec send_message(from_agent_id(), to_agent_id(), message()) -> ok | {error, term()}.
-spec broadcast_message(topology_id(), source_agent_id(), message()) -> {ok, count()} | {error, term()}.
-spec route_to_specialist(swarm_id(), specialist_type(), message()) -> ok | {error, term()}.

%% Consensus
-spec propose_consensus(swarm_id(), proposal()) -> {ok, proposal_id()} | {error, term()}.
-spec vote_consensus(swarm_id(), proposal_id(), vote()) -> ok | {error, term()}.
-spec get_consensus_status(swarm_id(), proposal_id()) -> {pending | accepted | rejected, details()}.
```

**Invariants**:
- I-6: ∀t ∈ topologies. ∃!manager. manages(t) ∧ running(manager)
- I-7: Message order preserved within single channel (FIFO per edge)
- I-8: Topology graph is acyclic OR all cycles have heartbeat
- I-9: Consensus requires quorum(nodes) votes OR timeout
- I-10: Leader election ⟹ unique leader + epoch increment
- I-11: Broadcast reaches all nodes OR reports partial failure

**Failure Modes**:
- **F-5**: Message loss on edge → retry with exponential backoff (max 3)
- **F-6**: Node removed mid-consensus → proposal fails, re-elect (if leader)
- **F-7**: Quorum lost → consensus blocked, notify waiting agents
- **F-8**: Network partition → dual-brain risk (mitigation: fencing, witness)
- **F-9**: Topology manager crash → stale view until recovery (cached locally)

---

### 3. Intelligence (erlmcp_observability)

**Responsibility**: Route agents intelligently, learn from execution, maintain semantic memory.

**Key Processes**:
```
erlmcp_observability_sup
├── erlmcp_routing_engine (gen_server)
│   ├── erlmcp_router_state (ETS cache)
│   └── erlmcp_router_metrics (accumulator)
├── erlmcp_learning_system (gen_server)
│   ├── erlmcp_trajectory_recorder (gen_server)
│   ├── erlmcp_sona_consolidator (gen_server)
│   └── erlmcp_ewc_consolidator (gen_server)
├── erlmcp_memory_subsystem (gen_server)
│   ├── erlmcp_semantic_store (sql.js + HNSW)
│   ├── erlmcp_pattern_bank (vector embeddings)
│   └── erlmcp_memory_reaper (TTL manager)
└── erlmcp_metrics_collector (gen_server)
    ├── erlmcp_histogram_aggregator
    └── erlmcp_percentile_estimator
```

**Routing Decision**:
```erlang
-type routing_decision() :: #{
    agent_id := agent_id(),
    confidence := float(),              % 0.0 to 1.0
    reasoning := {reason_type(), term()},
    latency_p95_ms := non_neg_integer(),
    success_rate := float(),
    alternatives := [agent_id()]
}.

-type routing_context() :: #{
    task_type := atom(),
    complexity := low | medium | high | critical,
    constraints := [constraint()],
    deadline_ms := pos_integer() | infinity,
    cost_budget := number(),
    user_id := binary()
}.

-spec route_task(routing_context())
    -> {ok, routing_decision()} | {error, term()}.
```

**Learning Trajectory**:
```erlang
-type trajectory() :: #{
    id := trajectory_id(),
    agent_id := agent_id(),
    task := task(),
    steps := [trajectory_step()],
    outcomes := [outcome()],
    reward := float(),
    feedback := feedback(),
    created_at := timestamp()
}.

-type trajectory_step() :: #{
    action := atom(),
    result := term(),
    quality := float(),              % Step quality (0.0-1.0)
    latency_ms := non_neg_integer()
}.

%% Record learning trajectory
-spec start_trajectory(agent_id(), task()) -> {ok, trajectory_id()} | {error, term()}.
-spec record_step(trajectory_id(), trajectory_step()) -> ok | {error, term()}.
-spec end_trajectory(trajectory_id(), outcomes(), feedback()) -> ok | {error, term()}.

%% Learn from trajectories (SONA + EWC++)
-spec trigger_learning_cycle(#{
    min_trajectories := pos_integer(),
    consolidation_strategy := {sona | ewc_plus_plus},
    preserve_critical := boolean()
}) -> {ok, learning_result()} | {error, term()}.
```

**Semantic Memory**:
```erlang
%% Store semantic patterns
-spec store_pattern(pattern(), confidence_score(), metadata())
    -> {ok, pattern_id()} | {error, term()}.

%% Semantic similarity search (HNSW)
-spec search_patterns(query_text(), #{
    top_k := pos_integer(),
    threshold := float(),
    namespace := atom()
}) -> {ok, [{pattern_id(), similarity_score(), pattern()}]} | {error, term()}.

%% Retrieve cached patterns
-spec get_pattern(pattern_id()) -> {ok, pattern()} | {error, not_found}.
```

**Core Metrics**:
```erlang
-record(agent_metrics, {
    task_count :: non_neg_integer(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer(),
    latency_histogram :: histogram(),
    error_rate :: float(),
    learning_events :: non_neg_integer(),
    pattern_matches :: non_neg_integer(),
    last_updated :: timestamp()
}).
```

**Invariants**:
- I-12: Routing decision based on recent metrics (≤5min old) OR error if stale
- I-13: ∀trajectory. ends with {success|failure|timeout} outcome (no partial)
- I-14: ∀pattern. ∃semantic_embedding. computed via ONNX + Poincaré ball
- I-15: Learning cycle ⟹ EWC++ consolidation (prevent catastrophic forgetting)
- I-16: Memory TTL enforced: patterns expire after N days (configurable)

**Failure Modes**:
- **F-10**: Routing engine stale → use fallback (random from healthy agents)
- **F-11**: Learning cycle interrupted → checkpoint + resume on next trigger
- **F-12**: Memory store overflow → evict oldest patterns (LRU)
- **F-13**: Pattern search timeout → return cached results (graceful degrade)
- **F-14**: Semantic embedding crash → recompute on demand (batch)

---

### 4. Security (erlmcp_validation)

**Responsibility**: Detect threats, audit operations, ensure compliance and data protection.

**Key Processes**:
```
erlmcp_validation_sup
├── erlmcp_aidefence_engine (gen_server)
│   ├── erlmcp_threat_detector (Soufflé queries)
│   ├── erlmcp_pattern_matcher (similarity search)
│   └── erlmcp_pii_scanner (regex + heuristics)
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

**AIDefence Threat Model**:
```erlang
-type threat_type() ::
    prompt_injection | jailbreak | pii_exposure | privilege_escalation |
    data_exfiltration | model_poisoning | token_theft | replay_attack.

-type threat_assessment() :: #{
    threat_type := threat_type(),
    severity := critical | high | medium | low,
    confidence := float(),              % 0.0-1.0
    indicators := [indicator()],
    recommended_action := block | sanitize | warn | log | escalate,
    detected_at := timestamp(),
    source_agent_id := agent_id()
}.

%% Scan input for threats
-spec scan_input(input_text(), scan_context())
    -> {ok, [threat_assessment()]} | {error, term()}.

%% Deep analysis with pattern matching
-spec analyze_threat(threat_assessment(), scan_context())
    -> {ok, detailed_analysis()} | {error, term()}.

%% Record mitigation
-spec record_mitigation(threat_assessment(), mitigation_action(), success())
    -> ok | {error, term()}.
```

**Audit Trail**:
```erlang
-type audit_event() :: #{
    event_id := uuid(),
    event_type := spawn_agent | terminate_agent | assign_task | learn |
                  route_decision | consensus_vote | threat_detected,
    timestamp := timestamp(),
    agent_id := agent_id(),
    user_id := binary(),
    action := term(),
    result := {ok | error, term()},
    pii_detected := boolean(),
    threat_level := critical | high | medium | low | none,
    metadata := map()
}.

%% Log audit event
-spec log_audit_event(audit_event()) -> {ok, event_id()} | {error, term()}.

%% Query audit trail
-spec query_audit(#{
    agent_id => agent_id(),
    event_type => atom(),
    date_range => {start_ts(), end_ts()},
    threat_level => atom()
}) -> {ok, [audit_event()]} | {error, term()}.

%% Immutable append-only log
-spec append_to_chain(audit_event()) -> {ok, block_hash()} | {error, term()}.
```

**Compliance Policy**:
```erlang
-type policy() :: #{
    id := policy_id(),
    name := binary(),
    rules := [compliance_rule()],
    applies_to := all | [agent_id()],
    severity := error | warning,
    enabled := boolean()
}.

-type compliance_rule() :: #{
    name := binary(),
    condition := query_expression(),
    action := allow | deny | warn,
    metadata := map()
}.

%% Check compliance
-spec check_compliance(agent_id(), action(), context())
    -> {compliant, details()} | {violates, policy_id(), details()}.

%% Generate compliance report
-spec generate_compliance_report(#{
    date_range => {start_ts(), end_ts()},
    focus => all | pii | threats | policies
}) -> {ok, report()} | {error, term()}.
```

**Invariants**:
- I-17: ∀event ∈ audit_log. cryptographically_signed(event) ∧ immutable
- I-18: ∀threat. detected ⟹ logged immediately (no race conditions)
- I-19: PII detection scans ALL inputs (agent messages, stored data)
- I-20: Policy violations ⟹ action taken (block|warn|log) BEFORE execution
- I-21: Encryption key rotation ≤90 days (NIST SP 800-57 compliance)

**Failure Modes**:
- **F-15**: AIDefence timeout → allow with warning (fail-open), log incident
- **F-16**: Audit log full → roll over to new partition, keep ≥30 days
- **F-17**: Threat detected → log + alert, prevent agent action, notify SOC
- **F-18**: Encryption key lost → fail-closed (no access to encrypted data)
- **F-19**: Policy engine slow → cache policy evaluations (with TTL)

---

## Part II: Inter-Subsystem APIs

### A. Agent Foundation ↔ Coordination

**Agent → Coordination** (spawn event):
```erlang
{agent_spawned, #{
    agent_id := agent_id(),
    role := agent_role(),
    capabilities := [capability()]
}}
```

**Coordination → Agent** (assign to swarm):
```erlang
{assign_to_swarm, #{
    swarm_id := swarm_id(),
    topology := topology_id()
}}
```

### B. Agent Foundation ↔ Intelligence

**Agent → Intelligence** (work completion):
```erlang
{task_completed, #{
    agent_id := agent_id(),
    task_id := task_id(),
    result := term(),
    latency_ms := pos_integer(),
    success := boolean()
}}
```

**Intelligence → Agent** (routing decision):
```erlang
{route_decision, #{
    routing_id := routing_id(),
    recommended_agent := agent_id(),
    confidence := float(),
    alternatives := [agent_id()]
}}
```

### C. Agent Foundation ↔ Security

**Agent → Security** (all inputs):
```erlang
{scan_agent_input, #{
    agent_id := agent_id(),
    input_text := binary(),
    context := map()
}}
```

**Security → Agent** (threat verdict):
```erlang
{threat_verdict, #{
    verdict := {allow | block | warn},
    threats := [threat_assessment()],
    required_action := atom()
}}
```

### D. Coordination ↔ Intelligence

**Coordination → Intelligence** (topology change):
```erlang
{topology_changed, #{
    topology_id := topology_id(),
    change_type := node_added | node_removed | partition_detected,
    affected_nodes := [agent_id()]
}}
```

**Intelligence → Coordination** (rebalance recommendation):
```erlang
{rebalance_recommendation, #{
    swarm_id := swarm_id(),
    actions := [rebalance_action()],
    expected_improvement := percentage()
}}
```

### E. Coordination ↔ Security

**Coordination → Security** (message audit):
```erlang
{audit_message, #{
    from := agent_id(),
    to := agent_id(),
    topic := atom(),
    size_bytes := non_neg_integer()
}}
```

**Security → Coordination** (policy enforcement):
```erlang
{enforce_policy, #{
    policy_id := policy_id(),
    action := allow | deny,
    reason := binary()
}}
```

### F. Intelligence ↔ Security

**Intelligence → Security** (pattern learned):
```erlang
{pattern_learned, #{
    pattern_id := pattern_id(),
    category := atom(),
    confidence := float()
}}
```

**Security → Intelligence** (threat pattern):
```erlang
{threat_pattern_updated, #{
    threat_type := threat_type(),
    patterns_added := pos_integer(),
    patterns_removed := pos_integer()
}}
```

---

## Part III: Invariants & Safety Properties

### Global Invariants

**G-1: Safety (Nothing Bad Happens)**
```
∀agent ∈ active_agents. ¬(violates_policy(agent) ∧ allowed_execution(agent))
```
*Agents never violate policies and execute simultaneously.*

**G-2: Liveness (Something Good Happens)**
```
∀task ∈ pending_tasks. eventually(completed(task) ∨ failed_with_reason(task))
```
*All tasks eventually complete or fail with explanation.*

**G-3: Causality (Events Ordered)**
```
∀event ∈ audit_log. parent_events_before(event) ∧ lamport_timestamp(event)
```
*Audit trail respects causal ordering via Lamport clocks.*

**G-4: Consistency (State Agreement)**
```
∀replica ∈ state_replicas. state(replica) = canonical_state ∨ recovering(replica)
```
*All state replicas agree or are recovering (eventual consistency).*

**G-5: Isolation (No Interference)**
```
∀a₁, a₂ ∈ agents. a₁ ≠ a₂ ⟹ ¬(task_a₁ interferes_with task_a₂)
```
*Agent tasks don't interfere (via shared memory isolation).*

### Subsystem-Level Invariants

**Agent Foundation Invariants:**
- I-1: One supervisor per agent
- I-2: Valid state transitions
- I-3: Registry consistency
- I-4: Atomic spawn
- I-5: Backpressure enforced

**Coordination Invariants:**
- I-6: One topology manager per topology
- I-7: Message FIFO
- I-8: Acyclic or heartbeat-covered
- I-9: Consensus quorum required
- I-10: Unique leader with epoch
- I-11: Broadcast coverage

**Intelligence Invariants:**
- I-12: Fresh metrics for routing (≤5min)
- I-13: Complete trajectories
- I-14: Semantic embeddings present
- I-15: EWC++ consolidation
- I-16: Memory TTL enforced

**Security Invariants:**
- I-17: Immutable audit log
- I-18: Threat detection immediate
- I-19: PII scan all inputs
- I-20: Policies enforced pre-execution
- I-21: Key rotation ≤90 days

---

## Part IV: Failure Modes & Recovery

### Failure Mode Table

| ID | Subsystem | Failure | Detection | Recovery | RTO | RPO |
|----|-----------|---------|-----------|----------|-----|-----|
| F-1 | Agent | Agent crash | Supervisor | Restart (max 5 in 60s) | <5s | 0 |
| F-2 | Agent | Registry down | Spawn error | Retry + fallback | <10s | <1s |
| F-3 | Agent | State backend down | Query error | Cache locally | N/A | <5min |
| F-4 | Agent | Mailbox overflow | Queue depth | Reject + backpressure | N/A | 0 |
| F-5 | Coord | Message loss | Ack timeout | Exponential retry (3x) | <30s | <100ms |
| F-6 | Coord | Node removal (consensus) | Not ACKing | Re-elect leader | <10s | N/A |
| F-7 | Coord | Quorum lost | Vote timeout | Block + notify | ∞ | N/A |
| F-8 | Coord | Network partition | Heartbeat timeout | Witness-based fencing | <30s | <5min |
| F-9 | Coord | Topology mgr crash | No updates | Cached view + full recovery | <5s | <1min |
| F-10 | Intel | Routing stale | Age check | Fallback random + refresh | <1s | <5min |
| F-11 | Intel | Learning interrupted | Checkpoint missing | Resume + validate | <1min | <1min |
| F-12 | Intel | Memory full | Size check | LRU eviction | <100ms | <1day |
| F-13 | Intel | Pattern search timeout | Latency exceeded | Return cache + async recompute | <100ms | <1day |
| F-14 | Intel | Embedding crash | Computation error | Recompute on demand | <5s | N/A |
| F-15 | Security | AIDefence timeout | Timer expired | Allow + warn + log | <100ms | <1min |
| F-16 | Security | Audit log full | Size exceeded | Roll partition + keep 30d | <1s | <1day |
| F-17 | Security | Threat detected | Pattern match | Log + alert + block | <100ms | 0 |
| F-18 | Security | Encryption key lost | Decryption failure | Fail-closed (no access) | ∞ | ∞ |
| F-19 | Security | Policy engine slow | CPU high | Cache + TTL | <1s | <5min |

### Recovery Orchestration

**Single-Agent Failure** (F-1):
```
Agent Crash
    ↓
[Supervisor detects via Down signal]
    ↓
[Increment restart counter]
    ↓
[Counter < 5 in 60s window?]
    ├─ YES → Restart agent (regenerate PID, restore state from backend)
    │         Notify coordination (removed from active topology)
    │         RTO: <5s, RPO: <1s (state checkpoints)
    │
    └─ NO → Supervisor gives up
            Notify swarm leader
            Attempt task redistribution
            RTO: <30s, RPO: <10s (in-flight tasks lost)
```

**Coordination Failure** (F-5, F-6, F-7):
```
Message Loss on Edge
    ↓
[Sender receives no ACK within timeout]
    ↓
[Increment retry counter]
    ↓
[Counter < 3?]
    ├─ YES → Exponential backoff: 100ms, 200ms, 400ms
    │         Retry send (idempotent message key)
    │         RTO: <30s
    │
    └─ NO → Report to swarm leader
            Attempt reroute (alternate path)
            If no paths exist → quorum lost
            RTO: <60s, RPO: <100ms
```

**Intelligence Failure** (F-10, F-11, F-12):
```
Routing Decision Stale (age > 5min)
    ↓
[Router detects during route_task()]
    ↓
[Emit warning + use fallback]
    ↓
[Trigger async metrics refresh]
    ├─ Use random healthy agent (round-robin)
    ├─ Refresh completes → future routes use new data
    └─ RTO: <1s, RPO: <5min (decisions may be suboptimal)
```

**Security Failure** (F-15, F-17):
```
AIDefence Scan Timeout
    ↓
[Timer expires during scan]
    ↓
[Return ALLOW verdict + warning log]
    ├─ Record in audit: timeout_vulnerability
    ├─ Schedule offline re-scan
    └─ RTO: <100ms, RPO: <1min (threat may slip through)

Threat Detected
    ↓
[Threat detector matches pattern]
    ↓
[Immediately log to immutable audit]
    ↓
[Execute mitigation action]
    ├─ BLOCK → Prevent execution + alert
    ├─ WARN → Log + proceed with monitoring
    └─ RTO: <100ms, RPO: 0 (immediate)
```

---

## Part V: App-to-Subsystem Mapping

### erlmcp_core → Agent Foundation + Routing

**Existing Modules** (97 total):
- erlmcp_registry* → Registry management
- erlmcp_session_* → Session state
- erlmcp_server* → Server lifecycle
- erlmcp_client* → Client lifecycle
- erlmcp_supervision* → Supervision trees

**New Modules** (to implement):
- erlmcp_agent_spawner.erl (gen_server)
- erlmcp_agent_registry.erl (gproc wrapper)
- erlmcp_agent_state_manager.erl (gen_server)
- erlmcp_agent_lifecycle_fsm.erl (gen_statem)
- erlmcp_agent_health_monitor.erl (gen_server)
- erlmcp_routing_engine.erl (gen_server)
- erlmcp_router_state.erl (ETS cache)
- erlmcp_router_metrics.erl (metrics accumulator)

### erlmcp_transports → Coordination

**Existing Modules** (23 total):
- erlmcp_transport_* → Protocol implementations
- erlmcp_transport_behavior → Behavior spec

**New Modules** (to implement):
- erlmcp_swarm_manager.erl (gen_server)
- erlmcp_topology_manager.erl (gen_server)
- erlmcp_consensus_manager.erl (gen_server)
- erlmcp_message_router.erl (gen_server)
- erlmcp_mesh_topology_worker.erl (gen_server)
- erlmcp_hierarchical_topology_worker.erl (gen_server)
- erlmcp_ring_topology_worker.erl (gen_server)
- erlmcp_star_topology_worker.erl (gen_server)
- erlmcp_transport_messages.erl (encoder/decoder)

### erlmcp_observability → Intelligence + Metrics

**Existing Modules** (31 total):
- erlmcp_otel* → OpenTelemetry integration
- erlmcp_metrics* → Metrics collection
- erlmcp_tracing* → Distributed tracing
- erlmcp_dashboard* → Visualization

**New Modules** (to implement):
- erlmcp_learning_system.erl (gen_server)
- erlmcp_trajectory_recorder.erl (gen_server)
- erlmcp_sona_consolidator.erl (gen_server, SONA learning)
- erlmcp_ewc_consolidator.erl (gen_server, EWC++ learning)
- erlmcp_memory_subsystem.erl (gen_server)
- erlmcp_semantic_store.erl (sql.js + HNSW)
- erlmcp_pattern_bank.erl (vector store)
- erlmcp_memory_reaper.erl (TTL manager)
- erlmcp_pattern_search.erl (semantic similarity)

### erlmcp_validation → Security + Audit

**Existing Modules** (13 total):
- erlmcp_compliance* → Policy validation
- erlmcp_validation* → Input validation

**New Modules** (to implement):
- erlmcp_aidefence_engine.erl (gen_server)
- erlmcp_threat_detector.erl (Soufflé + heuristics)
- erlmcp_pattern_matcher.erl (threat patterns)
- erlmcp_pii_scanner.erl (regex + ML)
- erlmcp_audit_manager.erl (gen_server)
- erlmcp_audit_logger.erl (persistent storage)
- erlmcp_audit_indexer.erl (full-text search)
- erlmcp_chain_of_custody.erl (immutable log)
- erlmcp_secret_manager.erl (gen_server)
- erlmcp_encryption_service.erl (crypto)
- erlmcp_key_rotation_service.erl (scheduled rotation)

---

## Part VI: Data Flow Diagrams

### Scenario 1: Agent Spawn & Join Swarm

```
User API
  │
  ├─→ spawn_agent(config)
  │       │
  │       └─→ [Agent Foundation]
  │           erlmcp_agent_spawner
  │              │
  │              ├─→ Validate config
  │              ├─→ Create sup child
  │              ├─→ Register in gproc
  │              └─→ Return {ok, agent()}
  │
  └─→ assign_to_swarm(agent_id, swarm_id)
          │
          └─→ [Coordination]
              erlmcp_swarm_manager
                 │
                 ├─→ Add to topology
                 ├─→ Send topology_changed → [Intelligence]
                 ├─→ Notify other agents (broadcast)
                 └─→ [Security] audit_event: {agent_joined_swarm}
```

### Scenario 2: Task Routing Decision

```
Pending Task
  │
  ├─→ [Intelligence]
  │   erlmcp_routing_engine.route_task(context)
  │      │
  │      ├─→ Load recent metrics (ETS cache)
  │      ├─→ Compute compatibility (agent caps vs task)
  │      ├─→ HNSW pattern match (similar past tasks)
  │      ├─→ Model inference (learned routing policy)
  │      └─→ Return routing_decision
  │
  └─→ [Agent Foundation]
      erlmcp_agent_spawner.assign_task(agent_id, task)
         │
         ├─→ Check agent load (backpressure)
         ├─→ Send task to agent
         ├─→ Record task_id in pending
         └─→ Start timeout timer
             │
             (... task executes in agent ...)
             │
             └─→ Agent reports completion
                   │
                   └─→ [Intelligence]
                       erlmcp_learning_system.record_step()
                          │
                          ├─→ Update trajectory
                          ├─→ Compute reward
                          └─→ Schedule learning cycle
```

### Scenario 3: Threat Detection & Audit

```
Agent Input
  │
  ├─→ [Security]
  │   erlmcp_aidefence_engine.scan_input(text, context)
  │      │
  │      ├─→ Tokenize input
  │      ├─→ Run threat patterns (Soufflé)
  │      ├─→ Check for PII (regex + heuristics)
  │      ├─→ Compute threat_assessment
  │      │
  │      └─→ [Audit]
  │          erlmcp_audit_manager.log_event(threat_detected)
  │             │
  │             ├─→ Create audit_event record
  │             ├─→ Append to immutable log
  │             ├─→ Hash + sign (block_hash)
  │             └─→ Index in full-text search
  │
  └─→ Return verdict: {allow|block|warn}
        │
        └─→ If threat → [Coordination] notify swarm leader
                       → escalate to SOC team
                       → enforce policy action
```

---

## Part VII: Architecture Decision Records (ADRs)

### ADR-001: Agent Spawn Synchronicity

**Status**: ACCEPTED

**Decision**: Agent spawn is synchronous (returns {ok, agent()} immediately), not async.

**Rationale**:
- Caller needs PID immediately for routing decisions
- Async spawn adds complexity (spawn result channel)
- Supervision ensures agent is ready before return (init guards)
- Caller can spawn async if needed (cast to spawner)

**Trade-offs**:
- ✗ Higher latency for caller (≤100ms)
- ✓ Simpler caller code (no spawn result channel)
- ✓ No orphan processes (caller has PID or error)

**Alternatives**:
1. Async spawn (actor model) - rejected (complexity)
2. Lazy spawn (agent created on first task) - rejected (unpredictable latency)

---

### ADR-002: Topology Redundancy via Multiple Paths

**Status**: ACCEPTED

**Decision**: Topologies support redundancy level (1-3) with multiple edges between nodes.

**Rationale**:
- Single path → single point of failure
- 3+ paths provides Byzantine fault tolerance (2f+1)
- Messages rerouted on edge failure
- Consensus tolerates up to f node failures

**Trade-offs**:
- ✗ Higher network overhead (edges × redundancy)
- ✓ Fault tolerance with gossip/consensus
- ✓ Automatic failover on edge loss

**Alternatives**:
1. No redundancy - rejected (not fault-tolerant)
2. Adaptive redundancy (cost-based) - deferred to v1.1

---

### ADR-003: Routing via Learned Models vs Rule-Based

**Status**: ACCEPTED

**Decision**: Hybrid routing - learned model + rule-based fallback.

**Rationale**:
- Pure learned models: slow startup (cold learning)
- Pure rules: inflexible (static cost functions)
- Hybrid: learns over time, safe fallback available
- Uses HNSW for fast pattern matching

**Trade-offs**:
- ✗ Dual-stack (both models + rules)
- ✓ Robust: learned model fails → rules kick in
- ✓ Learns: improves over time (convergent)

**Alternatives**:
1. Pure ML - rejected (cold start problem)
2. Pure rules - rejected (inflexible, manual tuning)
3. Ensemble voting - deferred to v1.2

---

### ADR-004: Immutable Audit Log

**Status**: ACCEPTED

**Decision**: Audit log is cryptographically immutable (append-only, hash-chained).

**Rationale**:
- NIST compliance (post-incident analysis)
- Audit trail cannot be tampered (legal evidence)
- Hash chaining catches any modification
- Rolling partitions (keep ≥30 days) for cost

**Trade-offs**:
- ✗ Hash computation overhead (SHA-256 per event)
- ✗ Large storage (audit events + hashes)
- ✓ Tamper-evident (modifications detected)
- ✓ Regulatory compliance (HIPAA, SOC2)

**Alternatives**:
1. Mutable audit log - rejected (legal risk)
2. 3rd-party audit service - deferred to v2.0
3. Blockchain - rejected (overkill, costly)

---

### ADR-005: PII Detection Strategy

**Status**: ACCEPTED

**Decision**: Multi-layered PII detection: regex patterns + ML classifier + heuristics.

**Rationale**:
- Regex alone: high false negatives (novel formats)
- ML alone: slow, requires training data
- Hybrid: catches obvious PII (regex), learns patterns (ML), adapts (heuristics)
- Scans ALL inputs: agent messages, stored data, logs

**Trade-offs**:
- ✗ False positives possible (manual review)
- ✗ Computational cost (3 passes per input)
- ✓ High recall (catches most PII)
- ✓ Adaptive (learns new patterns)

**Alternatives**:
1. Regex only - rejected (too many false negatives)
2. ML only - rejected (cold start, training time)
3. External PII service - deferred to v2.0

---

### ADR-006: Learning Consolidation (SONA + EWC++)

**Status**: ACCEPTED

**Decision**: Use SONA (Self-Organizing Neural Architecture) + EWC++ (Elastic Weight Consolidation) for lifelong learning.

**Rationale**:
- SONA: self-organizing trajectories (no manual labeling)
- EWC++: prevents catastrophic forgetting (preserves old knowledge)
- Combined: learns continuously without manual intervention
- Periodic cycles (≥10 trajectories) for consolidation

**Trade-offs**:
- ✗ Computational cost (training cycles)
- ✗ Storage (consolidation checkpoints)
- ✓ Automatic learning (no labels)
- ✓ Stable (old knowledge preserved)

**Alternatives**:
1. Supervised learning - rejected (requires labels)
2. Pure EWC - rejected (no self-organization)
3. No consolidation - rejected (forgetting)

---

### ADR-007: Vector Embedding Backend (ONNX + Poincaré Ball)

**Status**: ACCEPTED

**Decision**: Use ONNX models for Euclidean embeddings + Poincaré ball (hyperbolic) for semantic similarity.

**Rationale**:
- ONNX: portable, no external service dependency
- Poincaré ball: hierarchical similarity (tree-like data)
- HNSW: O(log N) nearest neighbor search
- LRU cache: avoid recomputation

**Trade-offs**:
- ✗ Model inference cost (≤50ms per text)
- ✗ Curvature tuning (hyperparameter)
- ✓ Fast search (HNSW, <10ms for 1M items)
- ✓ Interpretable (hyperbolic geometry)

**Alternatives**:
1. Dense vector DB (Weaviate/Pinecone) - deferred to v2.0
2. BM25 (keyword search) - rejected (misses semantic matches)
3. GPT embeddings - rejected (external dependency, cost)

---

## Part VIII: Testing Strategy

### Test Matrix

| Layer | Type | Coverage | Tools |
|-------|------|----------|-------|
| Unit | EUnit | ≥80% per module | eunit, meck |
| Integration | CT | ≥70% happy paths | common_test |
| Smoke | CT | ≥50% basic flows | common_test |
| Property | Proper | ≥3 generators per function | proper |
| Performance | Benchmark | <10% regression | rebar3 bench |
| Chaos | CT | ≥3 failure scenarios per subsystem | erlmcp_chaos |

### Test Scenarios

**Agent Foundation**:
- [ ] Spawn agent with valid config
- [ ] Spawn agent with invalid config (error handling)
- [ ] Agent crash → supervisor restarts (≤5 in 60s limit)
- [ ] Assign task during agent processing (backpressure)
- [ ] Registry consistency after multiple spawns

**Coordination**:
- [ ] Create mesh topology with N agents
- [ ] Add/remove node from running topology
- [ ] Message routing through topology
- [ ] Broadcast to all agents
- [ ] Consensus with quorum vote
- [ ] Leader election after leader crash
- [ ] Network partition recovery

**Intelligence**:
- [ ] Route task using learned model
- [ ] Fallback to rule-based routing (model failure)
- [ ] Record learning trajectory
- [ ] Learning cycle consolidation (SONA)
- [ ] Memory search via HNSW
- [ ] Pattern matching on new task

**Security**:
- [ ] Scan input for prompt injection
- [ ] Detect PII in agent messages
- [ ] Audit trail immutability
- [ ] Threat detection + mitigation
- [ ] Policy enforcement (block/warn)
- [ ] Encryption key rotation

---

## Part IX: Deployment Architecture

### Cloud Deployment (AWS/GCP/Azure)

```
┌─────────────────────────────────────────┐
│ Kubernetes Cluster                      │
├─────────────────────────────────────────┤
│ erlmcp-flow StatefulSet (replicas=3)    │
│ ├─ Pod 1 (primary)                      │
│ │  ├─ erlmcp_core (agent foundation)    │
│ │  ├─ erlmcp_transports (coordination)  │
│ │  ├─ erlmcp_observability (learning)   │
│ │  └─ erlmcp_validation (security)      │
│ ├─ Pod 2 (replica) - all apps          │
│ └─ Pod 3 (replica) - all apps          │
├─────────────────────────────────────────┤
│ Services:                               │
│ ├─ erlmcp-flow-internal (headless DNS)  │
│ ├─ erlmcp-flow-api (external LoadBalancer)
│ └─ erlmcp-flow-audit (write-only)       │
├─────────────────────────────────────────┤
│ Storage:                                │
│ ├─ PVC: agent_state (EBS)               │
│ ├─ PVC: audit_log (S3)                  │
│ └─ PVC: memory_store (local SSD)        │
└─────────────────────────────────────────┘
```

### Horizontal Scaling

**Sharding Strategy**:
- Agent registry: hash(agent_id) → node (consistent hashing)
- Topology manager: hash(topology_id) → node
- Audit log: immutable, replicated to S3 (no sharding)
- Memory store: local HNSW (no cross-node search)

**Load Balancing**:
- Routing requests: round-robin (agents balanced by latency)
- Swarm membership: local (no global consensus)
- Consensus votes: Raft across 3 nodes (≥50% quorum)

---

## Part X: Monitoring & Observability

### Key Metrics

```
Agent Foundation:
├─ erlmcp_agents_spawned_total (counter)
├─ erlmcp_agents_active (gauge)
├─ erlmcp_agent_lifespan_seconds (histogram)
└─ erlmcp_agent_restarts_total (counter, by reason)

Coordination:
├─ erlmcp_swarms_active (gauge)
├─ erlmcp_topology_nodes (gauge, by type)
├─ erlmcp_message_latency_ms (histogram, by direction)
├─ erlmcp_consensus_proposals_total (counter)
└─ erlmcp_consensus_acceptance_rate (gauge)

Intelligence:
├─ erlmcp_routing_decisions_total (counter, by outcome)
├─ erlmcp_routing_latency_ms (histogram)
├─ erlmcp_learning_trajectories_recorded (counter)
├─ erlmcp_pattern_search_latency_ms (histogram)
└─ erlmcp_memory_store_size_bytes (gauge)

Security:
├─ erlmcp_threats_detected_total (counter, by type)
├─ erlmcp_aidefence_latency_ms (histogram)
├─ erlmcp_audit_events_total (counter, by type)
├─ erlmcp_pii_detected_total (counter, by category)
└─ erlmcp_policy_violations_total (counter, by policy)
```

### Alerts (SLOs)

| Alert | Threshold | Action |
|-------|-----------|--------|
| Agent spawn > 5s p99 | >500ms | Page on-call |
| Consensus vote latency > 1s | >500ms | Investigate leader |
| AIDefence timeout rate > 1% | >1% | Reduce scan depth |
| Memory store size > 80% | >80% | Trigger eviction |
| Audit log error rate > 0.1% | >0.1% | Page SRE |

---

## Part XI: Security Model

### Threat Model

**Attacker Capabilities**:
- Prompt injection (tricky input formatting)
- Man-in-the-middle (network compromise)
- Data exfiltration (agent compromise)
- Privilege escalation (policy bypass)
- Denial of service (resource exhaustion)

**Defense Layers** (Defense-in-Depth):
1. Input validation (AIDefence) → catch common attacks
2. Policy enforcement (Security subsystem) → enforce access control
3. Audit trail (immutable log) → post-incident forensics
4. Encryption (TLS + symmetric) → confidentiality in transit/rest
5. Key rotation (≤90 days) → limit key lifetime

### Compliance Mappings

| Framework | Requirement | Implementation |
|-----------|-------------|-----------------|
| NIST SP 800-53 | AC-3 (Access Control) | Policy engine, audit log |
| NIST SP 800-53 | AU-2 (Audit Events) | Immutable audit log, chain-of-custody |
| NIST SP 800-57 | Key Management | 90-day rotation, encrypted storage |
| SOC2 C1 | Availability | 99.9% SLA, chaos testing |
| GDPR Art. 32 | Data Protection | Encryption, PII detection, breach log |

---

## Part XII: Appendices

### A. Glossary

| Term | Definition |
|------|-----------|
| Agent | Autonomous process (gen_server) with identity, state, capabilities |
| Swarm | Collection of agents + topologies for coordination |
| Topology | Graph of agents + edges representing communication paths |
| Routing Decision | Recommendation of which agent to execute task |
| Trajectory | Recording of agent actions + outcomes for learning |
| Pattern | Reusable template learned from trajectories |
| Threat | Detected security violation or anomaly |
| Audit Event | Immutable record of system action |
| Policy | Rule enforcing compliance (block/warn/allow actions) |
| Consensus | Distributed agreement among swarm members |

### B. References

- Joe Armstrong, "Making reliable distributed systems in the presence of sodomy"
- Lamport, L., "Time, Clocks, and the Ordering of Events in a Distributed System"
- NIST SP 800-57 Revision 3, "Recommendation for Key Management"
- NIST SP 800-53 Revision 5, "Security and Privacy Controls"
- Poincaré, H., "Science and Method" (hyperbolic geometry)

### C. Future Work (v1.1+)

- [ ] ADR-008: Cost-aware routing (latency vs throughput vs cost)
- [ ] ADR-009: Byzantine fault tolerance (>f node failures)
- [ ] ADR-010: Sharded memory store (cross-node semantic search)
- [ ] ADR-011: Self-healing topologies (auto-rebalance)
- [ ] Multi-swarm choreography (workflows)
- [ ] Hybrid cloud/edge deployment

---

**Version History**:
- v1.0.0 (2026-02-01): Initial design, 4 subsystems, APIs, ADRs, test strategy

**Next Review**: 2026-05-01 (after v1.0.0 implementation)

