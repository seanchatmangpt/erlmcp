# erlmcp-flow Implementation Guide v1.0.0

**Status**: DRAFT | **Date**: 2026-02-01 | **Target**: v1.0.0 Implementation Sprint

---

## Part I: Module Implementation Roadmap

### Phase 1: Agent Foundation (erlmcp_core) - 8 Modules

**Timeline**: Week 1-2 | **Tests**: 25+ test suites | **Coverage**: ≥80%

#### 1.1 erlmcp_agent_spawner.erl (gen_server)

**Responsibility**: Spawn and manage agent lifecycle.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec spawn_agent(agent_config()) -> {ok, agent()} | {error, term()}.
-spec get_agent(agent_id()) -> {ok, agent()} | {error, not_found}.
-spec list_agents(filter_options()) -> [agent()].
-spec assign_task(agent_id(), task()) -> {ok, task_id()} | {error, term()}.
-spec set_agent_state(agent_id(), agent_state()) -> ok | {error, term()}.
-spec terminate_agent(agent_id()) -> ok | {error, term()}.
-spec add_workers(non_neg_integer()) -> ok.
-spec pool_size() -> pos_integer().
```

**Implementation Notes**:
- Delegates actual spawn to agent_pool_worker_1..N (simple_one_for_one)
- Each spawn: validate → start_child → register → init_state → return
- Supervisor creates agents with unique PIDs
- Max concurrent spawns = num_workers (default 3-5)

**Test Coverage**:
- ✓ Spawn agent with valid config
- ✓ Spawn agent with invalid config (validation errors)
- ✓ Spawn multiple agents concurrently (thread safety)
- ✓ Assign task to agent (backpressure handling)
- ✓ Terminate agent (cleanup)
- ✓ Agent crash → supervisor restarts (≤5 in 60s)
- ✓ Registry consistency after spawn

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_agent_spawner.erl
apps/erlmcp_core/test/erlmcp_agent_spawner_tests.erl
apps/erlmcp_core/test/erlmcp_agent_spawner_SUITE.erl
apps/erlmcp_core/test/erlmcp_agent_spawner_prop_tests.erl
```

---

#### 1.2 erlmcp_agent_registry.erl (gproc wrapper)

**Responsibility**: Global agent registry (process naming + discovery).

**Key Functions**:
```erlang
-spec register(agent_id(), pid()) -> true | {error, already_registered}.
-spec unregister(agent_id()) -> ok | {error, not_found}.
-spec lookup(agent_id()) -> {ok, pid()} | {error, not_found}.
-spec list_agents() -> [{agent_id(), pid()}].
-spec list_agents_by_role(role()) -> [{agent_id(), pid()}].
-spec list_agents_by_state(agent_state()) -> [{agent_id(), pid()}].
```

**Implementation Notes**:
- Wrapper around gproc (O(log N) lookup)
- Uses gproc:reg({n, l, {agent, AgentId}}, Pid)
- Provides convenience query functions
- No state (all data in gproc)

**Test Coverage**:
- ✓ Register agent (success + duplicate)
- ✓ Lookup agent (found + not found)
- ✓ Unregister agent
- ✓ List all agents
- ✓ Query by role/state
- ✓ Consistency with actual processes

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_agent_registry.erl
apps/erlmcp_core/test/erlmcp_agent_registry_tests.erl
```

---

#### 1.3 erlmcp_agent_state_manager.erl (gen_server)

**Responsibility**: Manage persistent agent state (ETS/DETS/Mnesia).

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec init_agent_state(agent_id(), agent_config()) -> ok | {error, term()}.
-spec get_agent_state(agent_id()) -> {ok, agent_state()} | {error, not_found}.
-spec set_agent_state(agent_id(), agent_state()) -> ok | {error, term()}.
-spec update_agent_metrics(agent_id(), metrics()) -> ok | {error, term()}.
-spec delete_agent_state(agent_id()) -> ok | {error, not_found}.
-spec configure_backend(ets | dets | mnesia) -> ok.
```

**Implementation Notes**:
- Uses ETS by default (fast, ephemeral)
- Optional DETS for durability (persist to disk)
- Optional Mnesia for clustering
- State structure: `{agent_id, config, state, metrics, created_at}`

**Test Coverage**:
- ✓ ETS backend (fast path)
- ✓ DETS backend (durability)
- ✓ Metrics update (concurrent)
- ✓ Crash recovery (DETS persistence)
- ✓ Backend switching (online reconfiguration)

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_agent_state_manager.erl
apps/erlmcp_core/src/erlmcp_agent_state_ets.erl
apps/erlmcp_core/src/erlmcp_agent_state_dets.erl
apps/erlmcp_core/src/erlmcp_agent_state_mnesia.erl
apps/erlmcp_core/test/erlmcp_agent_state_manager_tests.erl
```

---

#### 1.4 erlmcp_agent_lifecycle_fsm.erl (gen_statem)

**Responsibility**: Enforce valid state transitions for agents.

**State Machine**:
```
           ┌─────────────┐
           │    IDLE     │
           └────┬────────┘
                │ spawn_agent
           ┌────▼────────┐
    ┌─────→│   ACTIVE    │◄──┐
    │      └────┬────────┘   │
    │           │ ready      │ retry
    │      ┌────▼────────┐   │
    │ ┌───→│  LEARNING   │───┘
    │ │    └─────────────┘
    └─┼──────┐
      │ task │
      ├──────┴────────┐
      │               │
    success        timeout/error
      │               │
      └───────┬───────┘
              │
         ┌────▼────────┐
         │   BLOCKED   │
         └────┬────────┘
              │ recover
         ┌────▼────────┐
         │    FAILED   │
         └─────────────┘
```

**Key Functions**:
```erlang
-spec start_link(agent_id(), agent_config()) -> {ok, pid()}.
-spec activate(pid()) -> ok | {error, not_allowed}.
-spec learn(pid()) -> ok | {error, not_allowed}.
-spec block(pid()) -> ok | {error, not_allowed}.
-spec recover(pid()) -> ok | {error, not_allowed}.
-spec fail(pid(), reason()) -> ok.
```

**Implementation Notes**:
- Uses gen_statem (event-driven state machine)
- Enforces state transitions via callback state()
- Prevents invalid transitions (returns error)
- Logs transitions for observability

**Test Coverage**:
- ✓ Valid transitions (idle → active → learning)
- ✓ Invalid transitions (learning → idle, error)
- ✓ Timeout handling (learning → blocked)
- ✓ Recovery path (blocked → idle)
- ✓ Failure path (any state → failed)

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_agent_lifecycle_fsm.erl
apps/erlmcp_core/test/erlmcp_agent_lifecycle_fsm_tests.erl
```

---

#### 1.5 erlmcp_agent_health_monitor.erl (gen_server)

**Responsibility**: Monitor agent health and trigger recovery.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec monitor_agent(agent_id(), pid()) -> ok.
-spec unmonitor_agent(agent_id()) -> ok.
-spec check_health(agent_id()) -> {ok, health_status()} | {error, term()}.
-spec list_unhealthy_agents() -> [agent_id()].
-spec restart_agent(agent_id()) -> ok | {error, term()}.
```

**Implementation Notes**:
- Periodic health checks (5s interval, configurable)
- Checks: memory, message queue, response time
- Triggers recovery if agent unresponsive
- Records health metrics (for intelligence system)

**Test Coverage**:
- ✓ Monitor active agent (healthy)
- ✓ Detect crashed agent (unhealthy)
- ✓ Detect slow agent (timeout)
- ✓ Restart failed agent
- ✓ Health metrics accuracy

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_agent_health_monitor.erl
apps/erlmcp_core/test/erlmcp_agent_health_monitor_tests.erl
```

---

#### 1.6 erlmcp_routing_engine.erl (gen_server)

**Responsibility**: Intelligent agent routing (hybrid model + rules).

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec route_task(routing_context()) -> {ok, routing_decision()} | {error, term()}.
-spec learn_from_result(routing_id(), outcome()) -> ok.
-spec get_routing_metrics() -> #{success_rate, latency_p95, ...}.
```

**Implementation Notes**:
- Loads agent metrics from ETS cache (ETS lookups ≤1ms)
- Ranks agents by: capability match, success_rate, latency_p95
- Optional ML inference (learned policy) with fallback to rules
- HNSW pattern matching for similar tasks

**Test Coverage**:
- ✓ Route to capable agent (no fallback)
- ✓ Route with constraints (latency < deadline)
- ✓ Fallback to random (no capable agents)
- ✓ Metric staleness handling (>5min)
- ✓ Concurrent routing (thread-safe)

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_routing_engine.erl
apps/erlmcp_core/src/erlmcp_router_state.erl
apps/erlmcp_core/src/erlmcp_router_metrics.erl
apps/erlmcp_core/test/erlmcp_routing_engine_tests.erl
```

---

#### 1.7 erlmcp_core_sup.erl (supervisor)

**Responsibility**: Supervise all core subsystem processes.

**Structure**:
```erlang
-spec init([]) -> {ok, {sup_flags(), child_specs()}}.
init([]) ->
    ChildSpecs = [
        #{id => erlmcp_agent_spawner,
          start => {erlmcp_agent_spawner, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => worker},
        #{id => erlmcp_agent_registry,
          start => {erlmcp_agent_registry, start_link, []},
          restart => permanent,
          type => worker},
        #{id => erlmcp_agent_state_manager,
          start => {erlmcp_agent_state_manager, start_link, []},
          restart => permanent,
          type => worker},
        #{id => erlmcp_agent_lifecycle_fsm,
          start => {erlmcp_agent_lifecycle_fsm, start_link, []},
          restart => permanent,
          type => worker},
        #{id => erlmcp_agent_health_monitor,
          start => {erlmcp_agent_health_monitor, start_link, []},
          restart => permanent,
          type => worker},
        #{id => erlmcp_routing_engine,
          start => {erlmcp_routing_engine, start_link, []},
          restart => permanent,
          type => worker},
        %% Agent pool (simple_one_for_one)
        #{id => erlmcp_agent_sup,
          start => {erlmcp_agent_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor}
    ],
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 60},
    {ok, {SupFlags, ChildSpecs}}.
```

**Files to Create**:
```
apps/erlmcp_core/src/erlmcp_core_sup.erl
apps/erlmcp_core/src/erlmcp_agent_sup.erl
```

---

### Phase 2: Coordination (erlmcp_transports) - 8 Modules

**Timeline**: Week 3-4 | **Tests**: 20+ test suites | **Coverage**: ≥80%

#### 2.1 erlmcp_swarm_manager.erl (gen_server)

**Responsibility**: Create and manage swarms.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec create_swarm(#{agents, topology}) -> {ok, swarm()} | {error, term()}.
-spec dissolve_swarm(swarm_id()) -> ok | {error, term()}.
-spec add_agent_to_swarm(swarm_id(), agent_id()) -> ok | {error, term()}.
-spec remove_agent_from_swarm(swarm_id(), agent_id()) -> ok | {error, term()}.
-spec get_swarm(swarm_id()) -> {ok, swarm()} | {error, not_found}.
-spec list_swarms() -> [swarm()].
```

**Implementation Notes**:
- Stores swarms in ETS
- Coordinates with topology_manager for topology creation
- Coordinates with consensus_manager for leader election
- No state dependencies (can restart safely)

**Files to Create**:
```
apps/erlmcp_transports/src/erlmcp_swarm_manager.erl
apps/erlmcp_transports/test/erlmcp_swarm_manager_SUITE.erl
```

---

#### 2.2 erlmcp_topology_manager.erl (gen_server)

**Responsibility**: Manage topology graph (nodes + edges).

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec create_topology(topology_type(), topology_config())
    -> {ok, topology()} | {error, term()}.
-spec add_node(topology_id(), agent_id())
    -> ok | {error, term()}.
-spec remove_node(topology_id(), agent_id())
    -> ok | {error, term()}.
-spec add_edge(topology_id(), agent_id(), agent_id(), edge_config())
    -> ok | {error, term()}.
-spec get_neighbors(topology_id(), agent_id())
    -> {ok, [agent_id()]} | {error, not_found}.
-spec get_topology(topology_id())
    -> {ok, topology()} | {error, not_found}.
```

**Implementation Notes**:
- Delegates to type-specific workers (mesh, hierarchical, ring, star)
- Maintains graph consistency (no cycles unless heartbeat-covered)
- Supports redundant edges (for fault tolerance)

**Files to Create**:
```
apps/erlmcp_transports/src/erlmcp_topology_manager.erl
apps/erlmcp_transports/src/erlmcp_mesh_topology_worker.erl
apps/erlmcp_transports/src/erlmcp_hierarchical_topology_worker.erl
apps/erlmcp_transports/src/erlmcp_ring_topology_worker.erl
apps/erlmcp_transports/src/erlmcp_star_topology_worker.erl
apps/erlmcp_transports/test/erlmcp_topology_manager_SUITE.erl
```

---

#### 2.3 erlmcp_message_router.erl (gen_server)

**Responsibility**: Route messages through topology.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec send_message(from_agent_id(), to_agent_id(), message())
    -> ok | {error, term()}.
-spec broadcast_message(topology_id(), source_agent_id(), message())
    -> {ok, count()} | {error, term()}.
-spec route_to_specialist(swarm_id(), specialist_type(), message())
    -> ok | {error, term()}.
```

**Implementation Notes**:
- Uses gen_server to manage message queues
- Implements FIFO per edge (ordered delivery)
- Retries on failure (exponential backoff, max 3x)
- Tracks message ACKs (timeout = 1s)

**Files to Create**:
```
apps/erlmcp_transports/src/erlmcp_message_router.erl
apps/erlmcp_transports/src/erlmcp_message_queue.erl
apps/erlmcp_transports/src/erlmcp_message_retry_policy.erl
apps/erlmcp_transports/test/erlmcp_message_router_SUITE.erl
```

---

#### 2.4 erlmcp_consensus_manager.erl (gen_server)

**Responsibility**: Distributed consensus among swarm members.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec propose_consensus(swarm_id(), proposal())
    -> {ok, proposal_id()} | {error, term()}.
-spec vote_consensus(swarm_id(), proposal_id(), accept | reject)
    -> ok | {error, term()}.
-spec get_consensus_status(swarm_id(), proposal_id())
    -> {pending | accepted | rejected, details()}.
-spec elect_leader(swarm_id())
    -> {ok, leader_id()} | {error, quorum_lost}.
-spec get_leader(swarm_id())
    -> {ok, leader_id(), epoch()} | {error, not_found}.
```

**Implementation Notes**:
- Implements Raft protocol (simplified)
- Requires quorum (>N/2 votes)
- Leader election with epoch numbering
- Timeout handling (leadership timeout = 30s)

**Files to Create**:
```
apps/erlmcp_transports/src/erlmcp_consensus_manager.erl
apps/erlmcp_transports/src/erlmcp_raft_state.erl
apps/erlmcp_transports/src/erlmcp_leader_election.erl
apps/erlmcp_transports/test/erlmcp_consensus_manager_SUITE.erl
```

---

#### 2.5 erlmcp_coordination_sup.erl (supervisor)

**Responsibility**: Supervise coordination subsystem.

**Files to Create**:
```
apps/erlmcp_transports/src/erlmcp_coordination_sup.erl
```

---

### Phase 3: Intelligence (erlmcp_observability) - 8 Modules

**Timeline**: Week 5-6 | **Tests**: 15+ test suites | **Coverage**: ≥80%

#### 3.1 erlmcp_routing_engine.erl (already in Phase 1)

See erlmcp_core section. This is distributed; core makes decisions, observability learns.

---

#### 3.2 erlmcp_learning_system.erl (gen_server)

**Responsibility**: Manage learning cycles and consolidation.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec trigger_learning_cycle(learning_config())
    -> {ok, learning_result()} | {error, term()}.
-spec get_trajectory_count() -> pos_integer().
-spec consolidate_trajectories(consolidation_strategy())
    -> {ok, consolidation_result()} | {error, term()}.
```

**Implementation Notes**:
- Coordinates with trajectory_recorder (accumulate trajectories)
- Triggers learning after ≥10 trajectories
- Uses SONA + EWC++ for consolidation
- Preserves critical patterns (EWC++)

**Files to Create**:
```
apps/erlmcp_observability/src/erlmcp_learning_system.erl
apps/erlmcp_observability/src/erlmcp_trajectory_recorder.erl
apps/erlmcp_observability/src/erlmcp_sona_consolidator.erl
apps/erlmcp_observability/src/erlmcp_ewc_consolidator.erl
apps/erlmcp_observability/test/erlmcp_learning_system_tests.erl
```

---

#### 3.3 erlmcp_memory_subsystem.erl (gen_server)

**Responsibility**: Semantic memory (patterns + embeddings).

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec store_pattern(pattern(), confidence_score(), metadata())
    -> {ok, pattern_id()} | {error, term()}.
-spec search_patterns(query_text(), search_options())
    -> {ok, [{pattern_id(), similarity_score(), pattern()}]} | {error, term()}.
-spec get_pattern(pattern_id())
    -> {ok, pattern()} | {error, not_found}.
-spec delete_pattern(pattern_id())
    -> ok | {error, term()}.
-spec get_store_size()
    -> {ok, size_bytes()} | {error, term()}.
```

**Implementation Notes**:
- Stores patterns in sql.js (HNSW index)
- Computes embeddings via ONNX (all-MiniLM-L6-v2)
- Uses Poincaré ball (hyperbolic geometry) for hierarchical similarity
- LRU eviction when storage exceeds limit

**Files to Create**:
```
apps/erlmcp_observability/src/erlmcp_memory_subsystem.erl
apps/erlmcp_observability/src/erlmcp_semantic_store.erl
apps/erlmcp_observability/src/erlmcp_pattern_bank.erl
apps/erlmcp_observability/src/erlmcp_embeddings.erl
apps/erlmcp_observability/src/erlmcp_memory_reaper.erl
apps/erlmcp_observability/test/erlmcp_memory_subsystem_tests.erl
```

---

#### 3.4 erlmcp_metrics_collector.erl (gen_server)

**Responsibility**: Collect and aggregate metrics.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec record_spawn(agent_id(), latency_ms()) -> ok.
-spec record_routing_decision(agent_id(), outcome()) -> ok.
-spec record_message(from_agent_id(), to_agent_id(), latency_ms()) -> ok.
-spec record_threat(threat_type(), severity()) -> ok.
-spec get_agent_metrics(agent_id())
    -> {ok, agent_metrics()} | {error, not_found}.
-spec get_system_metrics()
    -> {ok, system_metrics()}.
```

**Implementation Notes**:
- Collects metrics in ETS (fast)
- Periodically aggregates to histograms
- Computes percentiles (p50, p95, p99)
- Feeds data to routing engine cache

**Files to Create**:
```
apps/erlmcp_observability/src/erlmcp_metrics_collector.erl
apps/erlmcp_observability/src/erlmcp_histogram_aggregator.erl
apps/erlmcp_observability/src/erlmcp_percentile_estimator.erl
apps/erlmcp_observability/test/erlmcp_metrics_collector_tests.erl
```

---

### Phase 4: Security (erlmcp_validation) - 8 Modules

**Timeline**: Week 7-8 | **Tests**: 20+ test suites | **Coverage**: ≥80%

#### 4.1 erlmcp_aidefence_engine.erl (gen_server)

**Responsibility**: Detect security threats in agent inputs.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec scan_input(input_text(), scan_context())
    -> {ok, [threat_assessment()]} | {error, term()}.
-spec analyze_threat(threat_assessment(), scan_context())
    -> {ok, detailed_analysis()} | {error, term()}.
```

**Implementation Notes**:
- Runs Soufflé threat detection rules
- Pattern matches against known threats (HNSW)
- Scans for PII (regex + ML classifier)
- Timeout = 100ms (fail-open: allow + warn)

**Files to Create**:
```
apps/erlmcp_validation/src/erlmcp_aidefence_engine.erl
apps/erlmcp_validation/src/erlmcp_threat_detector.erl
apps/erlmcp_validation/src/erlmcp_pattern_matcher.erl
apps/erlmcp_validation/src/erlmcp_pii_scanner.erl
apps/erlmcp_validation/test/erlmcp_aidefence_engine_SUITE.erl
```

---

#### 4.2 erlmcp_audit_manager.erl (gen_server)

**Responsibility**: Log audit events to immutable chain.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec log_audit_event(audit_event())
    -> {ok, event_id()} | {error, term()}.
-spec query_audit(query_options())
    -> {ok, [audit_event()]} | {error, term()}.
-spec append_to_chain(audit_event())
    -> {ok, block_hash()} | {error, term()}.
-spec verify_chain_integrity()
    -> {ok, verified} | {error, tampering_detected}.
```

**Implementation Notes**:
- Append-only log (no modifications)
- Hash-chains events (SHA-256)
- Immutable storage (can't delete, only append)
- Rolling partitions (keep ≥30 days)

**Files to Create**:
```
apps/erlmcp_validation/src/erlmcp_audit_manager.erl
apps/erlmcp_validation/src/erlmcp_audit_logger.erl
apps/erlmcp_validation/src/erlmcp_audit_indexer.erl
apps/erlmcp_validation/src/erlmcp_chain_of_custody.erl
apps/erlmcp_validation/test/erlmcp_audit_manager_SUITE.erl
```

---

#### 4.3 erlmcp_compliance_validator.erl (gen_server)

**Responsibility**: Enforce compliance policies.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec check_compliance(agent_id(), action(), context())
    -> {compliant, details()} | {violates, policy_id(), details()}.
-spec load_policies(policy_list())
    -> ok | {error, term()}.
-spec generate_compliance_report(report_config())
    -> {ok, report()} | {error, term()}.
```

**Implementation Notes**:
- Interprets policy rules (Soufflé-like syntax)
- Caches policy evaluations (TTL = 5min)
- Supports: allow, deny, warn actions
- Reports per policy + violations

**Files to Create**:
```
apps/erlmcp_validation/src/erlmcp_compliance_validator.erl
apps/erlmcp_validation/src/erlmcp_policy_engine.erl
apps/erlmcp_validation/src/erlmcp_compliance_reporter.erl
apps/erlmcp_validation/test/erlmcp_compliance_validator_SUITE.erl
```

---

#### 4.4 erlmcp_secret_manager.erl (gen_server)

**Responsibility**: Encryption and key rotation.

**Key Functions**:
```erlang
-spec start_link() -> {ok, pid()}.
-spec encrypt_data(plain_text(), secret_context())
    -> {ok, cipher_text()} | {error, term()}.
-spec decrypt_data(cipher_text(), secret_context())
    -> {ok, plain_text()} | {error, term()}.
-spec rotate_keys()
    -> ok | {error, term()}.
-spec get_key_age(key_id())
    -> {ok, age_days()} | {error, not_found}.
```

**Implementation Notes**:
- AES-256 encryption (default)
- Keys stored securely (encrypted at rest)
- Automatic rotation ≤90 days (NIST SP 800-57)
- Backward compatible (old keys for decryption)

**Files to Create**:
```
apps/erlmcp_validation/src/erlmcp_secret_manager.erl
apps/erlmcp_validation/src/erlmcp_encryption_service.erl
apps/erlmcp_validation/src/erlmcp_key_rotation_service.erl
apps/erlmcp_validation/test/erlmcp_secret_manager_SUITE.erl
```

---

### Summary Table: Modules to Implement

| Subsystem | App | Module | Type | Deps | Tests |
|-----------|-----|--------|------|------|-------|
| **Foundation** | core | erlmcp_agent_spawner | gen_server | gproc | 7 |
| | | erlmcp_agent_registry | wrapper | gproc | 3 |
| | | erlmcp_agent_state_manager | gen_server | ets | 5 |
| | | erlmcp_agent_lifecycle_fsm | gen_statem | - | 5 |
| | | erlmcp_agent_health_monitor | gen_server | - | 4 |
| | | erlmcp_routing_engine | gen_server | ets | 5 |
| | | erlmcp_core_sup | supervisor | - | 2 |
| **Coordination** | transports | erlmcp_swarm_manager | gen_server | ets | 4 |
| | | erlmcp_topology_manager | gen_server | ets | 5 |
| | | erlmcp_*_topology_worker | gen_server | - | 4×4 |
| | | erlmcp_message_router | gen_server | - | 5 |
| | | erlmcp_consensus_manager | gen_server | - | 5 |
| | | erlmcp_coordination_sup | supervisor | - | 1 |
| **Intelligence** | observability | erlmcp_learning_system | gen_server | - | 4 |
| | | erlmcp_trajectory_recorder | gen_server | ets | 3 |
| | | erlmcp_sona_consolidator | gen_server | - | 3 |
| | | erlmcp_ewc_consolidator | gen_server | - | 3 |
| | | erlmcp_memory_subsystem | gen_server | sql.js | 4 |
| | | erlmcp_semantic_store | - | sql.js | 3 |
| | | erlmcp_embeddings | - | onnx | 3 |
| | | erlmcp_metrics_collector | gen_server | ets | 4 |
| **Security** | validation | erlmcp_aidefence_engine | gen_server | souffle | 5 |
| | | erlmcp_threat_detector | - | - | 4 |
| | | erlmcp_audit_manager | gen_server | - | 5 |
| | | erlmcp_audit_logger | - | s3 | 3 |
| | | erlmcp_compliance_validator | gen_server | - | 4 |
| | | erlmcp_secret_manager | gen_server | crypto | 4 |

---

## Part II: Quality Gates & Testing

### Gate 1: Compilation

```bash
TERM=dumb rebar3 compile
```

**Pass Criteria**: No errors, ≤5 warnings (deprecations ok)

### Gate 2: Unit Tests (EUnit)

```bash
rebar3 eunit --all
```

**Pass Criteria**:
- ≥90 test modules
- ≥400 test cases
- 0 failures
- 0 errors

### Gate 3: Integration Tests (Common Test)

```bash
rebar3 ct
```

**Pass Criteria**:
- ≥15 test suites
- 0 failures
- 0 skipped tests

### Gate 4: Code Coverage

```bash
rebar3 cover
```

**Pass Criteria**: ≥80% line coverage per subsystem

### Gate 5: Type Checking (Dialyzer)

```bash
rebar3 dialyzer
```

**Pass Criteria**: 0 warnings (except known limitations)

### Gate 6: Cross-Reference (Xref)

```bash
rebar3 xref
```

**Pass Criteria**: 0 undefined functions

### Gate 7: Code Formatting

```bash
rebar3 format
```

**Pass Criteria**: All code auto-formatted (git diff = 0 after formatting)

---

## Part III: Documentation Requirements

### Per Subsystem

- [ ] Architecture diagram (C4 L4)
- [ ] Supervision tree diagram
- [ ] State machine diagram (if applicable)
- [ ] API documentation (erl_docs + markdown)
- [ ] Implementation notes (design decisions)
- [ ] Example usage (code snippets)
- [ ] Error handling guide
- [ ] Performance characteristics

### Per Module

- [ ] Edoc comments on all public functions
- [ ] `-spec` type declarations (all functions)
- [ ] Module-level documentation (purpose, usage)
- [ ] Example in module comment
- [ ] Invariants / assumptions documented

### Global

- [ ] Architecture Decision Records (7 ADRs minimum)
- [ ] C4 diagrams (C1-C4 levels)
- [ ] Failure mode analysis (20+ scenarios)
- [ ] Testing strategy (unit, integration, property)
- [ ] Deployment guide (cloud + local)
- [ ] SLOs and monitoring
- [ ] Emergency procedures

---

## Part IV: Implementation Schedule

### Week 1-2: Agent Foundation (Phase 1)

**Sprint Goal**: Spawn and manage agents

- Mon-Tue: erlmcp_agent_spawner (core logic)
- Wed: erlmcp_agent_registry (wrapper)
- Thu: erlmcp_agent_state_manager (backend)
- Fri-Mon: erlmcp_agent_lifecycle_fsm (state machine)
- Tue: erlmcp_agent_health_monitor (health checks)
- Wed: erlmcp_routing_engine (basic routing)
- Thu-Fri: Supervisors, tests, code review

**Deliverables**:
- 7 modules implemented
- 25+ test suites (≥80% coverage)
- ADR-001: Agent Spawn Synchronicity
- C4 L3 diagram (Agent Foundation)

### Week 3-4: Coordination (Phase 2)

**Sprint Goal**: Topology and message routing

- Mon-Tue: erlmcp_swarm_manager
- Wed-Thu: erlmcp_topology_manager (+ 4 workers)
- Fri-Mon: erlmcp_message_router
- Tue-Wed: erlmcp_consensus_manager
- Thu-Fri: Tests, supervisors, code review

**Deliverables**:
- 8 modules implemented
- 20+ test suites (≥80% coverage)
- ADRs: ADR-002, ADR-003
- C4 L3 diagram (Coordination)

### Week 5-6: Intelligence (Phase 3)

**Sprint Goal**: Learning and routing

- Mon-Tue: erlmcp_learning_system + trajectory_recorder
- Wed: erlmcp_sona_consolidator
- Thu: erlmcp_ewc_consolidator
- Fri-Mon: erlmcp_memory_subsystem (+ semantic_store)
- Tue: erlmcp_embeddings (ONNX)
- Wed-Fri: erlmcp_metrics_collector, tests, code review

**Deliverables**:
- 8 modules implemented
- 15+ test suites (≥80% coverage)
- ADRs: ADR-004, ADR-005, ADR-006, ADR-007
- C4 L3 diagram (Intelligence)

### Week 7-8: Security (Phase 4)

**Sprint Goal**: Threats, audit, compliance

- Mon-Tue: erlmcp_aidefence_engine
- Wed: erlmcp_threat_detector + erlmcp_pii_scanner
- Thu-Fri: erlmcp_audit_manager (+ chain_of_custody)
- Mon-Tue: erlmcp_compliance_validator
- Wed-Thu: erlmcp_secret_manager
- Fri: Tests, supervisors, code review

**Deliverables**:
- 8 modules implemented
- 20+ test suites (≥80% coverage)
- ADRs: ADR-008, ADR-009
- C4 L3 diagram (Security)

### Week 9: Integration & Hardening

**Sprint Goal**: All subsystems working together

- Mon-Wed: Integration tests (cross-subsystem)
- Thu: Performance benchmarking
- Fri: Code review, documentation polish

**Deliverables**:
- Full C4 architecture diagrams
- Integration test suite (≥10 scenarios)
- Performance baselines
- Deployment documentation

### Week 10: Code Review & Release

**Sprint Goal**: Final quality gates

- Mon-Wed: Code review (internal + external)
- Thu: Final testing (chaos scenarios)
- Fri: Release preparation, tagging

**Deliverables**:
- v1.0.0 release candidate
- All quality gates passing
- Release notes

---

## Part V: Success Metrics

### Code Quality

- [ ] ≥90 modules implemented
- [ ] ≥80% test coverage (all subsystems)
- [ ] 0 compilation errors
- [ ] 0 type errors (Dialyzer)
- [ ] 0 undefined functions (Xref)
- [ ] All code formatted

### Testing

- [ ] ≥100 test modules
- [ ] ≥500 test cases
- [ ] All integration scenarios passing
- [ ] Chaos testing: 20+ failure scenarios
- [ ] Property tests: ≥3 per subsystem

### Architecture

- [ ] 4 subsystems clearly defined
- [ ] APIs documented (with examples)
- [ ] 9+ ADRs completed
- [ ] C4 diagrams (C1-C4)
- [ ] Supervision trees verified (no orphans)

### Documentation

- [ ] Architecture design (main doc): 100+ pages
- [ ] Quick reference guide: <10 pages
- [ ] Implementation guide: <50 pages
- [ ] ADRs: 9 detailed records
- [ ] Module-level docs: 100% coverage

### Performance

- [ ] Agent spawn <100ms p95
- [ ] Routing decision <50ms p95
- [ ] Message delivery <10ms p95
- [ ] AIDefence scan <100ms p95
- [ ] Audit logging <10ms p95

### Operations

- [ ] SLOs defined (all key operations)
- [ ] Monitoring dashboards (OTEL + Prometheus)
- [ ] Alerts configured (PagerDuty integration)
- [ ] Runbooks for 5+ emergency scenarios
- [ ] Deployment guide (cloud + local)

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-01
**Implementation Starts**: 2026-02-15 (estimated)

