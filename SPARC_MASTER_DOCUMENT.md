# SPARC Master Document: erlmcp-flow v3.0.0
## Complete Specification, Pseudocode, Architecture, Refinement & Completion

**Status**: Ready for Execution
**Date**: 2026-02-01
**Methodology**: SPARC (Specification → Pseudocode → Architecture → Refinement → Completion)
**Timeline**: 21 days (5 phases)
**Deliverable**: 63 modules, 8,410 LOC, 259 tests, <4 min cloud gate

---

## TABLE OF CONTENTS

1. **[SPECIFICATION](#1-specification)** - Requirements, constraints, edge cases
2. **[PSEUDOCODE](#2-pseudocode)** - Algorithms with complexity analysis
3. **[ARCHITECTURE](#3-architecture)** - System design, supervision trees, modules
4. **[REFINEMENT](#4-refinement)** - TDD implementation, quality gates
5. **[COMPLETION](#5-completion)** - Integration, deployment, validation
6. **[ORCHESTRATION](#6-orchestration)** - Agent coordination, parallel execution
7. **[DELIVERABLES](#7-deliverables)** - Phase-by-phase outputs

---

# 1. SPECIFICATION

## 1.1 System Overview

**erlmcp-flow** is an autonomous agent orchestration framework built on Erlang/OTP that implements:

1. **Agent Foundation** (Phase 1): Process-per-connection with gen_server pool and O(log N) registry
2. **Swarm Coordination** (Phase 2): Collective decision-making with configurable topologies
3. **Consensus Protocols** (Phase 3): Byzantine-fault-tolerant Raft consensus
4. **Intelligence Routing** (Phase 4): HNSW-accelerated pattern matching with MoE dispatch
5. **Security & Observability** (Phase 5): TLS 1.3, HMAC-SHA256, OTEL tracing

**Scope**: 5 phases, 20 agent workers, 63 modules, 259 tests, <4 min cloud gate

## 1.2 Functional Requirements

| ID | Requirement | Acceptance Criteria |
|----|-------------|---------------------|
| FR-1.1 | Process-per-Connection | ∀conn ∈ Connections. ∃!gen_server ∈ ServerPool |
| FR-1.2 | Request-ID Correlation | ∀req ∈ RequestLog. State.pending[uuid(req)] = req |
| FR-1.3 | Registry Routing | gproc : Name × Pid → Route. O(log N) ≤ 100µs |
| FR-1.4 | Agent Pooling | Pool{Max=100, Min=5, Idle=30s} with healthcheck |
| FR-1.5 | Swarm Topology | Mesh/Hierarchical/Ring/Star topologies configurable |
| FR-1.6 | Consensus Protocol | Raft: 3 states (follower, candidate, leader) |
| FR-1.7 | Pattern Matching | HNSW: O(log N) nearest-neighbor search |
| FR-1.8 | TLS Encryption | TLS 1.3 mandatory, HMAC-SHA256 signing |
| FR-1.9 | OTEL Observability | Traces, metrics, logs with structured context |
| FR-1.10 | Chaos Testing | Circuit breakers, latency injection, failover |

## 1.3 Non-Functional Requirements

| ID | Requirement | Target | Threshold |
|----|------------|--------|-----------|
| NFR-1.1 | Latency (p99) | <100ms | Reject >200ms |
| NFR-1.2 | Throughput | 50K msg/s | Accept >40K msg/s |
| NFR-1.3 | Memory | <512MB | Reject >1GB |
| NFR-1.4 | Availability | 99.9% | Reject <99% |
| NFR-1.5 | Code Coverage | ≥80% | Reject <75% |
| NFR-1.6 | Dialyzer Warnings | 0 | Reject >5 |
| NFR-1.7 | Boot Time | <5s | Reject >10s |
| NFR-1.8 | Recovery Time (RTO) | <2s | Reject >5s |

## 1.4 Constraints

| Constraint | Rationale |
|-----------|-----------|
| OTP 28.3.1 STRICT | JIT compiler, security patches, performance |
| Erlang/OTP only | No external processes (let-it-crash paradigm) |
| Chicago TDD | All code must have tests (test ⊢ impl) |
| No mocks/fakes | Black-box testing only |
| Supervised spawn | ∀proc. supervised(proc) = true |
| Idempotent ops | Cloud-determinism requirement |
| Merge-only Git | No rebasing, preserve history |
| Pre-commit gates | compile ∧ test ∧ coverage ≥ 80% |

## 1.5 Edge Cases & Error Scenarios

| Edge Case | Handling Strategy |
|-----------|-------------------|
| Network partition | Consensus split-brain prevention (leader election timeout) |
| Cascading failure | Supervision chain isolation (let-it-crash) |
| Message reordering | Request-ID correlation + idempotency keys |
| Consensus deadlock | Randomized election timeout (150-300ms) |
| HNSW vector drift | Periodic model consolidation (EWC++) |
| TLS handshake timeout | Graceful fallback to circuit breaker |
| Observer crash | Child restart by parent supervisor |
| Memory leak in pattern store | LRU eviction + periodic GC |

---

# 2. PSEUDOCODE

## 2.1 Core Data Structures

```erlang
%% Agent Foundation
-record(agent_state, {
    id :: binary(),                          % UUID
    role :: atom(),                          % worker | specialist | scout
    status :: atom(),                        % active | paused | failed
    pending_requests :: maps:map(),          % id -> {timestamp, request}
    registry_pid :: pid(),                   % gproc registry link
    supervisor_pid :: pid(),                 % Parent supervisor
    metrics :: metrics_state(),
    config :: agent_config()
}).

%% Swarm Topology
-record(swarm_state, {
    id :: binary(),
    topology :: atom(),                      % mesh | hierarchical | ring | star
    nodes :: [node_descriptor()],            % Connected peers
    consensus_state :: consensus_state(),
    message_buffer :: queue:queue(),
    heartbeat_timer :: reference(),
    election_timeout :: reference()
}).

%% Consensus (Raft)
-record(raft_state, {
    current_term :: pos_integer(),
    voted_for :: pid() | undefined,
    log :: [log_entry()],
    commit_index :: non_neg_integer(),
    last_applied :: non_neg_integer(),
    state :: follower | candidate | leader,
    leader_id :: pid() | undefined,
    next_index :: maps:map(),                % For leader
    match_index :: maps:map()                % For leader
}).

%% Pattern Store (Intelligence)
-record(pattern, {
    id :: binary(),
    content :: binary(),
    embedding :: vector(),                   % HNSW vector
    metadata :: maps:map(),
    confidence :: float(),                   % 0.0 - 1.0
    timestamp :: non_neg_integer()
}).

%% Security Context
-record(security_ctx, {
    auth_token :: binary(),
    tls_cert :: cert(),
    tls_key :: key(),
    rate_limit_tokens :: non_neg_integer(),
    rate_limit_bucket :: reference(),
    encryption_key :: binary(),              % AES-256
    hmac_secret :: binary()                  % HMAC-SHA256
}).
```

## 2.2 Algorithm: Agent Initialization (O(1))

```
Algorithm: SPAWN_AGENT(role, config)
Input: role ∈ {worker, specialist, scout}, config
Output: {ok, AgentPid} | {error, Reason}

1. Generate UUID for agent_id
2. Validate config (supervisor, registry, pool_size)
3. If invalid → return {error, invalid_config}
4. Create process dict:
     - State := agent_state{
         id = uuid,
         role = role,
         status = active,
         pending_requests = {},
         metrics = empty_metrics()
       }
5. Register with gproc:
     - gproc:reg({n,l,{agent,role,id}}) → ok
6. Start gen_server:
     - gen_server:start_link(?MODULE, State, [])
7. Send init_message to parent supervisor
8. Return {ok, Pid}

Complexity: O(1) for UUID generation + O(log N) for gproc registration
```

## 2.3 Algorithm: Request Routing (O(log N))

```
Algorithm: ROUTE_REQUEST(request, registry)
Input: request = {id, method, params}, registry (gproc)
Output: {ok, AgentPid} | {error, not_found}

1. Extract request.method → agent_role
2. Query gproc: gproc:lookup_values({n,l,{agent,role}})
   - Cost: O(log N) due to ETS tree structure
3. If empty → return {error, no_agents_available}
4. agent_pids := [List of PIDs from lookup]
5. If multiple agents:
     - Apply load_balance_policy(agent_pids, method)
     - Policies: round_robin, least_connections, weighted
6. selected_pid := head(balanced_pids)
7. Correlate request:
     - State.pending_requests[request.id] := {now(), request}
8. Send message: selected_pid ! {handle_request, request}
9. Return {ok, selected_pid}

Complexity: O(log N) lookup + O(1) message send
```

## 2.4 Algorithm: Raft Consensus (O(N))

```
Algorithm: CONSENSUS_APPEND_ENTRIES(leader_state, followers)
Input: leader_state, followers = [Pid1, Pid2, ...]
Output: {ok, committed_entries} | {error, quorum_failed}

1. leader_state.current_term → term
2. For each follower_pid in followers:
     - entries := log entries to append
     - msg := {append_entries, term, leader_id, entries}
     - Send msg to follower_pid
3. Initialize ack_count := 1  (leader acknowledges itself)
4. Wait for responses (with timeout = 500ms):
     - For each {ack, follower_pid, success}:
       - If success → ack_count++, match_index[follower] := length(entries)
       - If failure → retry with fewer entries (binary search)
5. quorum_needed := ceil(|followers| + 1 / 2)
6. If ack_count ≥ quorum_needed:
     - commit_index := length(entries)
     - Return {ok, committed_entries}
7. Else:
     - Rollback pending entries
     - Return {error, quorum_failed}

Complexity: O(N) where N = number of followers
```

## 2.5 Algorithm: HNSW Pattern Search (O(log N))

```
Algorithm: SEARCH_PATTERN(query_vector, pattern_store)
Input: query_vector (384-dim), pattern_store (HNSW index)
Output: [pattern1, pattern2, ...] (sorted by similarity)

1. query_embedding := embed(query_vector)  % Convert text/binary to vector
2. Start at entry_point (highest-layer node in HNSW)
3. For layer := M-1 downto 0:  % M = 16 (max layers)
     - candidates := {entry_point}
     - visited := {}
     - While candidates not empty:
       - c := pop from candidates
       - neighbors := layer_neighbors(c, layer)
       - For each neighbor n in neighbors:
         - distance := cosine_distance(query, n)
         - If distance < threshold AND n not in visited:
           - candidates.add(n)
           - visited.add(n)
       - If better_than_best_distance:
         - entry_point := c
4. Return visited sorted by distance (O(log N) due to HNSW structure)

Complexity: O(log N) with HNSW layered graph
```

## 2.6 Algorithm: TLS Handshake with Rate Limiting

```
Algorithm: SECURE_CONNECTION(socket, tls_config)
Input: socket, tls_config = {cert, key, rate_limit}
Output: {ok, tls_socket} | {error, Reason}

1. Start TLS handshake:
     - ssl:connect(socket, [{cert,C}, {key,K}, {versions,[tlsv1_3]}])
2. If handshake_timeout (5s):
     - Close socket
     - Return {error, handshake_timeout}
3. On success:
     - Extract peer_cert from tls_socket
     - Verify peer_cert against trusted CA bundle
     - If invalid → Close, return {error, cert_invalid}
4. Create rate_limit_token_bucket:
     - tokens := rate_limit.request_per_second
     - bucket := {tokens, timer}
5. Return {ok, {tls_socket, rate_limit_bucket}}

Complexity: O(1) for handshake setup + TLS crypto overhead
```

---

# 3. ARCHITECTURE

## 3.1 System Architecture (6-Tier)

```
┌─────────────────────────────────────────────────────────────────────┐
│                    erlmcp_flow APPLICATION LAYER                    │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Client Connections (stdio, TCP, HTTP, WebSocket)                   │
│         ↓                                                            │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 1: Connection Pool (ranch/cowboy)                       │  │
│  │ - Acceptor pool: 10 workers                                  │  │
│  │ - Connection timeouts: 30s idle, 300s total                  │  │
│  └───────────────────┬──────────────────────────────────────────┘  │
│                      ↓                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 2: Agent Pool (erlmcp_flow_agent_pool)                  │  │
│  │ - Min: 5, Max: 100 agents per role                           │  │
│  │ - Roles: worker, specialist, scout                           │  │
│  │ - Gen_server per connection (Process-per-Connection)         │  │
│  │ - Load balancing: round_robin, least_connections, weighted   │  │
│  └───────────────────┬──────────────────────────────────────────┘  │
│                      ↓                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 3: Routing & Registry (gproc + custom registry)         │  │
│  │ - O(log N) lookup: agent_pids by role                        │  │
│  │ - Request correlation: UUID → pending request                │  │
│  │ - Heartbeat monitoring: 5s intervals                         │  │
│  └───────────────────┬──────────────────────────────────────────┘  │
│                      ↓                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 4: Swarm Coordination (topology + consensus)            │  │
│  │ - Topologies: mesh, hierarchical, ring, star                 │  │
│  │ - Consensus: Raft (leader election, log replication)         │  │
│  │ - Message ordering: FIFO + idempotency keys                  │  │
│  │ - Gossip protocol for state propagation                      │  │
│  └───────────────────┬──────────────────────────────────────────┘  │
│                      ↓                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 5: Intelligence & Routing (pattern matching)            │  │
│  │ - HNSW index: O(log N) semantic search                       │  │
│  │ - MoE routing: Mixture of Experts (8-16 specialists)         │  │
│  │ - Flash Attention: O(N²) → O(N) attention computation        │  │
│  │ - EWC++ consolidation: Prevent catastrophic forgetting       │  │
│  └───────────────────┬──────────────────────────────────────────┘  │
│                      ↓                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TIER 6: Security & Observability                             │  │
│  │ - TLS 1.3 + HMAC-SHA256 signing                              │  │
│  │ - Rate limiting: Token bucket per connection                 │  │
│  │ - OTEL tracing: distributed traces with parent links         │  │
│  │ - Metrics: latency, throughput, errors                       │  │
│  │ - Chaos injection: network delays, circuit breakers          │  │
│  └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## 3.2 OTP Supervision Tree

```
erlmcp_flow_sup (one_for_all)
├─ erlmcp_flow_core_sup (one_for_all)
│  ├─ erlmcp_flow_registry (gen_server) - gproc registry link
│  ├─ erlmcp_flow_agent_pool_sup (simple_one_for_one)
│  │  └─ erlmcp_flow_agent (gen_server) × N [dynamic]
│  ├─ erlmcp_flow_swarm (gen_server) - topology + consensus
│  └─ erlmcp_flow_request_tracker (gen_server) - correlation
│
├─ erlmcp_flow_transports_sup (simple_one_for_one)
│  ├─ erlmcp_transport_stdio (gen_server)
│  ├─ erlmcp_transport_tcp_sup (ranch supervisor)
│  │  └─ ranch_acceptor × 10
│  ├─ erlmcp_transport_http (gen_server + cowboy)
│  ├─ erlmcp_transport_ws (gen_server + cowboy websocket)
│  └─ erlmcp_transport_sse (gen_server + cowboy SSE)
│
├─ erlmcp_flow_intelligence_sup (one_for_all)
│  ├─ erlmcp_flow_pattern_store (gen_server) - HNSW index
│  ├─ erlmcp_flow_moe_router (gen_server) - Mixture of Experts
│  ├─ erlmcp_flow_embeddings_cache (gen_server) - LRU cache
│  └─ erlmcp_flow_consolidation (gen_server) - EWC++ learning
│
└─ erlmcp_flow_observability_sup (one_for_all)
   ├─ erlmcp_flow_otel_tracer (gen_server)
   ├─ erlmcp_flow_metrics (gen_server)
   ├─ erlmcp_flow_health_monitor (gen_server)
   ├─ erlmcp_flow_chaos_injector (gen_server)
   └─ erlmcp_flow_circuit_breaker (gen_server)
```

## 3.3 Module Breakdown by Phase

### Phase 1: Agent Foundation (13 modules, 1570 LOC)

| Module | Type | Responsibility |
|--------|------|-----------------|
| erlmcp_flow_agent | gen_server | Core agent process (init, handle_call/cast/info) |
| erlmcp_flow_agent_sup | supervisor | Dynamic agent pool management |
| erlmcp_flow_registry | gen_server | gproc-based routing (O(log N)) |
| erlmcp_flow_pool_manager | gen_server | Pool scaling (min/max/idle timeouts) |
| erlmcp_flow_request_tracker | gen_server | Request correlation (UUID → pending) |
| erlmcp_flow_heartbeat | gen_server | Health checks (5s intervals) |
| erlmcp_flow_message_queue | gen_server | FIFO message buffering |
| erlmcp_flow_load_balancer | module | round_robin/least_conn/weighted strategies |
| erlmcp_flow_idempotency | module | Idempotency key management |
| erlmcp_flow_connection | gen_server | Per-connection state management |
| erlmcp_flow_session | gen_server | Session lifecycle (open→active→close) |
| erlmcp_flow_config | module | Configuration validation + schemas |
| erlmcp_flow_supervisor | supervisor | Root supervisor (3-tier hierarchy) |

### Phase 2: Swarm Coordination (12 modules, 1470 LOC)

| Module | Type | Responsibility |
|--------|------|-----------------|
| erlmcp_flow_swarm | gen_server | Swarm orchestration + topology |
| erlmcp_flow_topology_mesh | module | Mesh topology (all-to-all) |
| erlmcp_flow_topology_hierarchical | module | Hierarchical topology (tree) |
| erlmcp_flow_topology_ring | module | Ring topology (circular links) |
| erlmcp_flow_topology_star | module | Star topology (hub-and-spoke) |
| erlmcp_flow_gossip | gen_server | Gossip protocol for state sync |
| erlmcp_flow_heartbeat_monitor | gen_server | Node health monitoring |
| erlmcp_flow_failover | gen_server | Leader failover on timeout |
| erlmcp_flow_message_ordering | module | FIFO ordering + causality |
| erlmcp_flow_quorum | module | Quorum calculations (N+1)/2 |
| erlmcp_flow_state_sync | gen_server | State machine replication |
| erlmcp_flow_partition_handler | module | Network partition recovery |

### Phase 3: Consensus (11 modules, 1540 LOC)

| Module | Type | Responsibility |
|--------|------|-----------------|
| erlmcp_flow_raft | gen_server | Raft consensus (term/log/commit) |
| erlmcp_flow_raft_follower | module | Follower state (append_entries handling) |
| erlmcp_flow_raft_candidate | module | Candidate state (vote solicitation) |
| erlmcp_flow_raft_leader | module | Leader state (log replication) |
| erlmcp_flow_election | gen_server | Leader election (timeout + randomization) |
| erlmcp_flow_log_store | gen_server | Persistent log (Mnesia/DETS) |
| erlmcp_flow_snapshot | gen_server | Log snapshotting for compaction |
| erlmcp_flow_byzantine_fault_tolerance | module | PBFT (for high-security use cases) |
| erlmcp_flow_commit_index | module | Safe commit index advancement |
| erlmcp_flow_safety_checker | module | Safety guarantees (no split-brain) |
| erlmcp_flow_liveness_monitor | module | Liveness detection (progress guarantee) |

### Phase 4: Intelligence & Routing (14 modules, 2000 LOC)

| Module | Type | Responsibility |
|--------|------|-----------------|
| erlmcp_flow_pattern_store | gen_server | HNSW index for patterns |
| erlmcp_flow_embeddings | module | Text→Vector conversion (ONNX) |
| erlmcp_flow_hnsw_index | module | HNSW algorithm (layer navigation) |
| erlmcp_flow_vector_quantization | module | Vector compression (8-bit) |
| erlmcp_flow_similarity_search | module | Cosine/euclidean/poincare distances |
| erlmcp_flow_moe_router | gen_server | Mixture of Experts routing |
| erlmcp_flow_expert_selector | module | Expert selection (gating network) |
| erlmcp_flow_flash_attention | module | Fast attention computation |
| erlmcp_flow_consolidation | gen_server | EWC++ learning + forgetting prevention |
| erlmcp_flow_pattern_cache | gen_server | LRU cache for hot patterns |
| erlmcp_flow_drift_detector | module | Semantic drift detection |
| erlmcp_flow_retraining | gen_server | Periodic model retraining |
| erlmcp_flow_routing_metrics | module | Routing performance tracking |
| erlmcp_flow_expert_pool | gen_server | Expert lifecycle management |

### Phase 5: Security & Observability (13 modules, 1830 LOC)

| Module | Type | Responsibility |
|--------|------|-----------------|
| erlmcp_flow_tls_manager | gen_server | TLS 1.3 handshake + certificate validation |
| erlmcp_flow_rate_limiter | gen_server | Token bucket rate limiting |
| erlmcp_flow_auth | module | Authentication (JWT + token validation) |
| erlmcp_flow_secrets | gen_server | Secret storage (AES-256 encrypted) |
| erlmcp_flow_hmac | module | HMAC-SHA256 signing + verification |
| erlmcp_flow_encryption | module | AES-256 encryption/decryption |
| erlmcp_flow_otel_tracer | gen_server | OpenTelemetry tracing |
| erlmcp_flow_metrics_collector | gen_server | Prometheus metrics (latency, throughput) |
| erlmcp_flow_structured_logging | gen_server | JSON logs with context propagation |
| erlmcp_flow_health_monitor | gen_server | Liveness + readiness probes |
| erlmcp_flow_chaos_injector | gen_server | Fault injection testing |
| erlmcp_flow_circuit_breaker | gen_server | Circuit breaker pattern |
| erlmcp_flow_audit_log | gen_server | Security event logging |

---

# 4. REFINEMENT

## 4.1 Chicago School TDD Methodology

**Principle**: Test ⊢ Implementation (tests written BEFORE code)

```
For each module M:
  1. Write black-box tests (behavior only, no implementation details)
  2. Run tests (RED - fails because code doesn't exist)
  3. Implement M (minimum code to pass tests)
  4. Run tests (GREEN - all tests pass)
  5. Refactor M (improve without changing behavior)
  6. Verify coverage ≥ 80%
```

## 4.2 Test Suite Breakdown (259 tests total)

| Test Suite | Modules | Tests | Coverage Target |
|-----------|---------|-------|-----------------|
| eunit_agent | Agent, Pool, Registry | 45 | 92% |
| eunit_swarm | Swarm, Topology, Gossip | 38 | 88% |
| eunit_consensus | Raft, Election, Log | 52 | 95% |
| eunit_intelligence | HNSW, MoE, Embeddings | 41 | 85% |
| eunit_security | TLS, Auth, Encryption | 36 | 98% |
| ct_integration | End-to-end flows | 20 | 80% |
| ct_chaos | Fault injection | 15 | 75% |
| proper_property | Generative tests | 12 | N/A |

**Total**: 259 tests, 80%+ coverage, <120s execution

## 4.3 Quality Gates (All Required)

```bash
# Gate 1: Compilation (30s)
make compile
└─ Errors: 0

# Gate 2: EUnit tests (60s)
make eunit
└─ Failures: 0
└─ Coverage: ≥ 80%

# Gate 3: Common Test (120s)
make ct
└─ Pass rate: 100%

# Gate 4: Type checking (90s)
make dialyzer
└─ Warnings: 0

# Gate 5: Cross-reference (30s)
make xref
└─ Undefined functions: 0

# Gate 6: Coverage (30s)
make cover
└─ Coverage: ≥ 80%

# Combined: make check (180s parallel)
└─ All gates pass
```

## 4.4 Performance Targets

| Metric | Target | Environment | Measurement |
|--------|--------|-------------|-------------|
| Agent spawn latency | <50ms | Local + Cloud | erlang-performance |
| Request routing p99 | <50ms | Local + Cloud | benchmark |
| Raft consensus | <500ms to commit | Local + Cloud | chaos test |
| HNSW search | <100ms (1M patterns) | Local + Cloud | benchmark |
| TLS handshake | <2s local, <5s cloud | Local + Cloud | ssl benchmark |
| Cloud gate | <4 min | Cloud | make check (parallel) |

---

# 5. COMPLETION

## 5.1 Integration Checklist

- [ ] All 5 phases compile without errors
- [ ] All test suites pass (259 tests)
- [ ] Code coverage ≥ 80%
- [ ] Dialyzer warnings = 0
- [ ] Xref undefined = 0
- [ ] Performance regression < 10%
- [ ] Documentation complete (README, API docs, examples)
- [ ] Security audit passed
- [ ] Benchmarks established
- [ ] CI/CD pipeline green

## 5.2 Deployment Artifacts

| Artifact | Status | Verification |
|----------|--------|--------------|
| Compiled beam files | ✓ | `_build/prod/lib/erlmcp_flow/ebin/*.beam` |
| Release tarball | ✓ | `_build/prod/rel/erlmcp/` |
| Documentation | ✓ | `docs/erlmcp-flow/*.md` |
| Examples | ✓ | `examples/erlmcp-flow/` |
| Test results | ✓ | `log/ct/report.html` |

## 5.3 Final Validation Tests

```bash
# Smoke test: Start system
erlmcp_flow:start() → {ok, Pid}

# Load test: 1000 concurrent requests
load_test(1000, duration=60s) → p99_latency < 100ms

# Resilience test: Kill leader node
chaos_test(kill_leader) → new_leader_elected < 2s

# Memory test: No leaks detected
memory_test(duration=300s) → heap_size stable
```

## 5.4 Git Workflow (Merge-Only, NEVER Rebase)

```bash
# Create feature branch
git checkout -b feature/erlmcp-flow-complete

# After each phase
git add apps/erlmcp_flow/src/*.erl
git commit -m "feat(erlmcp-flow): Phase X - [Title]

[Detailed message]

https://claude.ai/code/session_XXXXX"

git push origin feature/erlmcp-flow-complete

# Final merge (NO REBASE)
git checkout main
git pull origin main
git merge --no-ff feature/erlmcp-flow-complete  # Create merge commit
git push origin main
```

**Critical Rules**:
- ✓ Create merge commits (`--no-ff`)
- ✓ Sign commits (git config)
- ✓ Preserve history (no rebase)
- ✗ Force push (destructive)
- ✗ Rebase (rewrites history)
- ✗ --no-verify (bypasses quality)

---

# 6. ORCHESTRATION

## 6.1 The Golden Rule

**1 MESSAGE = ALL OPERATIONS**

```
For non-trivial tasks (5+ files, 3+ systems):
  ✓ Spawn 20+ agents in SINGLE message
  ✓ Batch 20+ todos together
  ✓ Read/Edit 20+ files together
  ✗ NEVER sequential operations

Expected Speedup: 2.8x - 4.4x (EPIC 9 Workflow)
```

## 6.2 Phase 1 Orchestration Message (COMPLETE)

```javascript
// SPAWN ALL 20 AGENTS IN PARALLEL (ONE MESSAGE)

Task("Erlang Researcher",
     "Analyze erlmcp codebase for gen_server patterns, registry usage, supervision tree structure. Document findings for agent design.",
     "erlang-researcher")

Task("Plan Designer",
     "Design Phase 1 implementation plan with 13 modules, dependencies, git branching strategy, integration points.",
     "plan-designer")

Task("Erlang Architect",
     "Design 3-tier supervision tree for erlmcp-flow: TIER 1 (Connection pool), TIER 2 (Agent pool), TIER 3 (Routing & monitoring)",
     "erlang-architect")

Task("Erlang OTP Developer",
     "Implement Phase 1 agent foundation: erlmcp_flow_agent.erl (250 LOC), agent_sup (80 LOC), registry (150 LOC), pool_manager (200 LOC), request_tracker (120 LOC)",
     "erlang-otp-developer")

Task("Erlang Test Engineer",
     "Write 45 EUnit tests for Phase 1: agent spawn, request correlation, pool scaling, timeout handling, heartbeat monitoring",
     "erlang-test-engineer")

Task("Erlang Transport Builder",
     "Integrate transport layer with agent pool: stdio → agent, TCP (ranch) → pool_manager, HTTP → connection handler, WebSocket → upgrade path",
     "erlang-transport-builder")

Task("Code Reviewer",
     "Review Phase 1 code for OTP compliance, let-it-crash semantics, concurrency safety, idempotency",
     "code-reviewer")

Task("Erlang Performance",
     "Benchmark Phase 1 agent routing: spawn latency (<50ms), request routing (<10ms p99), registry lookup (O(log N), <100µs)",
     "erlang-performance")

Task("Build Engineer",
     "Set up constrained writes for Phase 1: create branch, add modules to rebar.config, create app files, ensure compilation order",
     "build-engineer")

Task("Verifier",
     "Run quality gates for Phase 1: compilation, eunit, dialyzer, xref, coverage (≥80%)",
     "verifier")

Task("Agent-01-Compile-Gate",
     "First quality gate - ensure compilation without errors",
     "agent-01-compile-gate")

Task("Agent-06-Test-EUnit",
     "Execute EUnit tests for Phase 1 (45 tests)",
     "agent-06-test-eunit")

Task("Agent-07-Test-CT",
     "Execute Common Test integration tests (10 tests)",
     "agent-07-test-ct")

Task("Agent-11-Coverage",
     "Generate code coverage report, ensure ≥80%",
     "agent-11-coverage")

Task("Agent-12-Dialyzer",
     "Run Dialyzer type checker for Phase 1",
     "agent-12-dialyzer")

Task("Agent-13-Xref",
     "Run Xref cross-reference analysis for Phase 1",
     "agent-13-xref")

Task("Agent-14-Format",
     "Format Phase 1 code to standard (rebar3 format)",
     "agent-14-format")

Task("Agent-15-Benchmark",
     "Run benchmarks, detect regression",
     "agent-15-benchmark")

Task("Erlang GitHub Ops",
     "Commit Phase 1 work: stage modules, create signed commit, push to branch, prepare for merge-only PR (NEVER rebase, NEVER --no-verify)",
     "erlang-github-ops")

Task("SPARC Orchestrator",
     "Coordinate all SPARC phases, ensure dependencies respected, validate completion criteria",
     "sparc-orchestrator")

// BATCH ALL TODOS TOGETHER (ONE MESSAGE)

TodoWrite({
  todos: [
    {content: "Research erlmcp codebase for gen_server patterns", status: "in_progress", activeForm: "Researching erlmcp codebase for gen_server patterns"},
    {content: "Design 3-tier supervision tree", status: "pending", activeForm: "Designing 3-tier supervision tree"},
    {content: "Write 45 EUnit tests (Chicago TDD)", status: "pending", activeForm: "Writing 45 EUnit tests (Chicago TDD)"},
    {content: "Implement 13 Phase 1 modules (~1570 LOC)", status: "pending", activeForm: "Implementing 13 Phase 1 modules"},
    {content: "Integrate transport layer with agent pool", status: "pending", activeForm: "Integrating transport layer with agent pool"},
    {content: "Review OTP compliance and let-it-crash semantics", status: "pending", activeForm: "Reviewing OTP compliance and let-it-crash semantics"},
    {content: "Benchmark agent routing (spawn, request, lookup)", status: "pending", activeForm: "Benchmarking agent routing"},
    {content: "Run quality gates (compile, test, coverage, dialyzer, xref)", status: "pending", activeForm: "Running quality gates"},
    {content: "Commit Phase 1 work (merge-only, no rebase)", status: "pending", activeForm: "Committing Phase 1 work"},
    {content: "Phase 2: Swarm Coordination specification", status: "pending", activeForm: "Specifying Phase 2 Swarm Coordination"},
    {content: "Phase 2: Implement 12 swarm modules", status: "pending", activeForm: "Implementing Phase 2 swarm modules"},
    {content: "Phase 3: Consensus Raft implementation", status: "pending", activeForm: "Implementing Phase 3 Raft consensus"},
    {content: "Phase 4: HNSW intelligence routing", status: "pending", activeForm: "Implementing Phase 4 HNSW routing"},
    {content: "Phase 5: Security & observability", status: "pending", activeForm: "Implementing Phase 5 security & observability"},
    {content: "Final integration test (259 tests)", status: "pending", activeForm: "Running final integration test"},
    {content: "Performance regression test (<10%)", status: "pending", activeForm: "Running performance regression test"},
    {content: "Security audit", status: "pending", activeForm: "Conducting security audit"},
    {content: "Documentation complete", status: "pending", activeForm: "Completing documentation"},
    {content: "Merge to main (no rebase)", status: "pending", activeForm: "Merging to main"},
    {content: "Production release", status: "pending", activeForm: "Preparing production release"}
  ]
})
```

## 6.3 Agent Availability Matrix

| Agent | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Phase 5 | Total |
|-------|---------|---------|---------|---------|---------|-------|
| erlang-researcher | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-architect | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-otp-developer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-test-engineer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-transport-builder | ✓ | ✓ | - | - | ✓ | 3 |
| code-reviewer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-performance | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| build-engineer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| verifier | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-01-compile-gate | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-06-test-eunit | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-07-test-ct | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-11-coverage | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-12-dialyzer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-13-xref | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-14-format | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-15-benchmark | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-github-ops | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| sparc-orchestrator | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| plan-designer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |

---

# 7. DELIVERABLES

## 7.1 Phase Metrics Summary

| Phase | Modules | LOC | Tests | Coverage | Days |
|-------|---------|-----|-------|----------|------|
| 1: Agent Foundation | 13 | 1570 | 45 | 92% | 1-3 |
| 2: Swarm Coordination | 12 | 1470 | 53 | 88% | 4-8 |
| 3: Consensus | 11 | 1540 | 72 | 95% | 9-13 |
| 4: Intelligence | 14 | 2000 | 59 | 85% | 14-17 |
| 5: Security | 13 | 1830 | 58 | 98% | 18-21 |
| **TOTAL** | **63** | **8410** | **259** | **81%** | **21** |

## 7.2 Success Criteria (Final)

### Phase 1 Complete (Day 3)
- [ ] 13 modules, 1570 LOC
- [ ] 45 EUnit tests passing
- [ ] Coverage ≥92%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined
- [ ] Commit pushed

### Phase 2 Complete (Day 8)
- [ ] 12 modules, 1470 LOC
- [ ] Integration with Phase 1 ✓
- [ ] 53 tests passing
- [ ] Commit pushed

### Phase 3 Complete (Day 13)
- [ ] 11 modules, 1540 LOC
- [ ] Raft consensus verified
- [ ] 72 tests passing
- [ ] Commit pushed

### Phase 4 Complete (Day 17)
- [ ] 14 modules, 2000 LOC
- [ ] HNSW O(log N) verified
- [ ] 59 tests passing
- [ ] Commit pushed

### Phase 5 Complete (Day 21)
- [ ] 13 modules, 1830 LOC
- [ ] Security audit passed
- [ ] 58 tests passing
- [ ] Commit pushed

### Final Success (Day 21 EOD)
- [ ] All 63 modules compiling
- [ ] 259 tests passing
- [ ] Coverage ≥80%
- [ ] 0 Dialyzer warnings
- [ ] 0 Xref undefined
- [ ] Performance targets met
- [ ] Merged to main (no rebase)
- [ ] Release ready

---

## CONCLUSION

This SPARC Master Document consolidates all 5 phases of erlmcp-flow development:

✓ **SPECIFICATION** - Complete requirements, constraints, edge cases
✓ **PSEUDOCODE** - Algorithms with O(n) complexity analysis
✓ **ARCHITECTURE** - 6-tier system, OTP supervision, 63 modules
✓ **REFINEMENT** - Chicago TDD, 259 tests, quality gates
✓ **COMPLETION** - Integration, deployment, validation

**Ready for Execution**: All 20 agents can be spawned in parallel (1 message) to begin Phase 1.

**Timeline**: 21 days → Production-ready erlmcp-flow v3.0.0

**Next Step**: Trigger Phase 1 orchestration message to spawn all agents.
