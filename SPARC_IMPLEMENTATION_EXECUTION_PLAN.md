# SPARC Implementation Execution Plan
## erlmcp-flow: From Specification to Production

**Version**: 3.0.0
**Phases**: 5 (Spec → Code → Architecture → Refinement → Completion)
**Timeline**: 21 days (3 weeks)
**Agents**: 20 autonomous workers
**Expected Output**: 8,410 LOC, 259 tests, <4 min cloud gate

---

## EXECUTIVE FLOWCHART

```
START (Day 0: Specification Approval)
  ↓
PHASE 1: AGENT FOUNDATION (Days 1-3)
  ├─ 1.1: Specification (13 requirements, 0 ambiguity)
  ├─ 1.2: Pseudocode (Agent state machine, O(1) routing)
  ├─ 1.3: Architecture (3-tier supervision tree)
  ├─ 1.4: Refinement - TDD (45 EUnit tests)
  └─ 1.5: Completion (make check passes)
    ↓
PHASE 2: SWARM COORDINATION (Days 4-8)
  ├─ 2.1: Specification (Topology + Gossip + Failover)
  ├─ 2.2: Pseudocode (4 topology algorithms)
  ├─ 2.3: Architecture (Component integration)
  ├─ 2.4: Refinement - TDD (53 tests)
  └─ 2.5: Completion (Integration with Phase 1)
    ↓
PHASE 3: CONSENSUS PROTOCOLS (Days 9-13)
  ├─ 3.1: Specification (Raft + Byzantine FT)
  ├─ 3.2: Pseudocode (Leader election, log replication)
  ├─ 3.3: Architecture (11 modules)
  ├─ 3.4: Refinement - TDD (72 tests)
  └─ 3.5: Completion (Safety + liveness verified)
    ↓
PHASE 4: INTELLIGENCE & ROUTING (Days 14-17)
  ├─ 4.1: Specification (HNSW + MoE + EWC++)
  ├─ 4.2: Pseudocode (Pattern search, expert selection)
  ├─ 4.3: Architecture (14 modules)
  ├─ 4.4: Refinement - TDD (59 tests)
  └─ 4.5: Completion (Semantic routing verified)
    ↓
PHASE 5: SECURITY & OBSERVABILITY (Days 18-21)
  ├─ 5.1: Specification (TLS + Auth + OTEL)
  ├─ 5.2: Pseudocode (Handshake, encryption, metrics)
  ├─ 5.3: Architecture (13 modules)
  ├─ 5.4: Refinement - TDD (58 tests)
  └─ 5.5: Completion (Security audit passed)
    ↓
FINAL INTEGRATION & VALIDATION (Day 21)
  ├─ Full system test (8,410 LOC, 259 tests)
  ├─ Performance regression test
  ├─ Security review
  ├─ Documentation complete
  └─ Merge to main (no rebase, preserve history)
    ↓
END (Production release)
```

---

## PHASE-BY-PHASE EXECUTION

### PHASE 1: AGENT FOUNDATION

#### 1.1 Specification (1 day)

**Deliverables**:
- Requirements document (all 10 FR, 8 NFR from SPARC spec)
- Constraint list (8 items: OTP 28.3.1, Chicago TDD, merge-only, etc.)
- Edge cases identified (8 scenarios: network partition, cascading failure, etc.)
- Acceptance criteria clear (no ambiguity)

**Acceptance Gate**:
```erlang
Specification_Complete =
  AllRequirementsTestabl ∧
  NoAmbiguity ∧
  ConstraintsClear ∧
  EdgeCasesDocumented.
```

**Agent Roles**:
- **Erlang Researcher**: Extract gen_server patterns from erlmcp
- **Plan Designer**: Define 13 modules with dependencies
- **Erlang Architect**: Draw 3-tier supervision tree

#### 1.2 Pseudocode (1 day)

**Deliverables**:
- Agent state machine (init, handle_call, handle_cast, handle_info)
- Request routing algorithm: O(log N) gproc lookup
- Pool scaling algorithm (min/max/idle timeout)
- Request correlation with UUID
- Heartbeat monitoring (5s intervals)

**Example Pseudocode** (from spec):

```erlang
%% Algorithm: SPAWN_AGENT(role, config) → {ok, Pid}
1. Generate UUID
2. Validate config
3. Create gen_server state
4. Register with gproc: {n,l,{agent,role,id}}
5. Start: gen_server:start_link(?MODULE, State, [])
6. Return {ok, Pid}
```

**Algorithm Complexity Analysis**:
- spawn_agent: O(1) (no traversals)
- route_request: O(log N) (gproc ETS tree)
- correlation_lookup: O(1) (maps)

#### 1.3 Architecture (1 day)

**Deliverables**:
- System architecture diagram (6 tiers)
- OTP supervision tree (TIER1-3 hierarchy)
- Component interfaces (5 behaviors)
- Module list (13 modules, ~1570 LOC)
- Integration points (with Phase 2, 4, 5)

**Key Design Decisions**:
1. Process-per-Connection: Each connection gets own gen_server
2. Registry: gproc for O(log N) lookup by role + ID
3. Pool: Dynamic scaling with min/max bounds
4. Idempotency: UUID + state tracking prevents retries
5. Let-it-crash: Supervisor restarts failed agents

#### 1.4 Refinement: TDD (2 days)

**Chicago School TDD**:
```
WRITE TEST (RED) →
  Run (fails because code doesn't exist) →
IMPLEMENT CODE (GREEN) →
  Run (all tests pass) →
REFACTOR (maintain behavior) →
  Run (all tests still pass)
```

**Test Suite** (45 EUnit tests):

| Module | Tests | Scenarios |
|--------|-------|-----------|
| agent | 12 | spawn, request_correlation, timeout, shutdown |
| pool_manager | 10 | scaling (up/down), idle_timeout, limits |
| registry | 8 | register, lookup, deregister, not_found |
| request_tracker | 8 | track, resolve, expire, collision |
| load_balancer | 7 | round_robin, least_conn, weighted |

**Example Test** (from spec):

```erlang
-module(erlmcp_flow_agent_tests).
-include_lib("eunit/include/eunit.hrl").

request_correlation_test() ->
  {setup, fun setup/0, fun teardown/1,
   fun({AgentPid, _}) ->
     RequestId = erlang:make_ref(),
     Request = #{id => RequestId, method => test, params => hello},

     %% Send request
     erlmcp_flow_agent:handle_request(Request, AgentPid),

     %% Verify in pending
     State = erlmcp_flow_agent:get_state(AgentPid),
     ?assertMatch(#{pending_requests := #{RequestId := _}}, State),

     %% Wait for response
     Response = receive {response, RequestId, _} -> ok after 1000 -> timeout end,
     ?assertEqual(ok, Response),

     %% Verify cleared from pending
     State2 = erlmcp_flow_agent:get_state(AgentPid),
     ?assertNotMatch(#{pending_requests := #{RequestId := _}}, State2)
   end}.
```

**Coverage Target**: ≥92% (Phase 1 is foundation, requires high coverage)

#### 1.5 Completion: Quality Gates

```bash
make compile           # Gate 1: errors = 0
make eunit             # Gate 2: failures = 0, coverage ≥80%
make ct                # Gate 3: pass_rate = 100%
make dialyzer          # Gate 4: warnings = 0
make xref              # Gate 5: undefined = ∅
make check             # All gates (180s parallel)
```

**Success Criteria**:
- [ ] 13 modules compiling (1570 LOC)
- [ ] 45 EUnit tests passing
- [ ] 10 CT integration tests passing
- [ ] Coverage ≥92%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined
- [ ] Performance baseline established
- [ ] Benchmarks: agent spawn <50ms, routing <10ms p99
- [ ] Code reviewed + approved
- [ ] Commit pushed to feature branch

**Commit Message** (merge-only, never rebase):
```
feat(erlmcp-flow): Phase 1 - Agent Foundation

Implements process-per-connection architecture with gen_server pool,
gproc-based registry (O(log N) routing), dynamic scaling, and request
correlation with UUID tracking.

Modules:
- erlmcp_flow_agent (250 LOC): core gen_server
- erlmcp_flow_agent_sup (80 LOC): dynamic pool
- erlmcp_flow_registry (150 LOC): gproc wrapper
- erlmcp_flow_pool_manager (200 LOC): scaling logic
- erlmcp_flow_request_tracker (120 LOC): correlation
- ... (8 more modules)

Tests:
- 45 EUnit tests
- 10 Common Test integration tests
- Coverage: 92%
- Dialyzer: 0 warnings
- Xref: 0 undefined

Performance:
- Agent spawn: <50ms
- Request routing: <10ms (p99)
- Registry lookup: O(log N), <100µs

https://claude.ai/code/session_XXXXX
```

---

### PHASE 2: SWARM COORDINATION (Days 4-8)

**Similar structure to Phase 1** but with:
- **13 modules**: 12 (+ 1 from Phase 1 refactoring)
- **1,470 LOC**: swarm orchestration + topology
- **53 tests**: 38 EUnit + 15 CT
- **88% coverage target**

**Key deliverables**:
1. Specification: Topology + gossip + failover requirements
2. Pseudocode: Mesh/hierarchical/ring/star algorithms
3. Architecture: Component integration with Phase 1
4. Refinement: TDD for consensus+messaging
5. Completion: Integrated swarm system

**Critical module**: `erlmcp_flow_swarm.erl` (280 LOC)
```erlang
%% Swarm orchestration
-behaviour(gen_server).

%% State: Topology + Nodes + Consensus + MessageBuffer
-record(swarm_state, {
    id :: binary(),
    topology :: atom(),              % mesh | hierarchical | ring | star
    nodes :: [node_descriptor()],    % Connected peers
    consensus_state :: consensus_state(),
    message_buffer :: queue:queue()
}).

%% Key functions:
init(Args) → {ok, State}.
handle_call({add_node, Node}, From, State) → {reply, ok, NewState}.
handle_cast({broadcast, Msg}, State) → {noreply, NewState}.
handle_info({heartbeat_timeout}, State) → {noreply, NewState}.
```

**Integration with Phase 1**:
```erlang
%% Phase 1 agents notify swarm of state changes
erlmcp_flow_agent:on_state_change(NewState) →
  erlmcp_flow_swarm:gossip(state_change, NewState) →
    broadcast_to_neighbors().

%% Phase 2 swarm manages agent topology
erlmcp_flow_swarm:get_neighbors(AgentId) → [Pid1, Pid2, ...].
```

---

### PHASE 3: CONSENSUS (Days 9-13)

**Key modules** (11 total, 1,540 LOC):
- `erlmcp_flow_raft.erl` (300 LOC): main consensus engine
- `erlmcp_flow_raft_follower.erl` (150 LOC): follower state
- `erlmcp_flow_raft_candidate.erl` (140 LOC): candidate state
- `erlmcp_flow_raft_leader.erl` (160 LOC): leader state
- `erlmcp_flow_election.erl` (180 LOC): leader election
- `erlmcp_flow_log_store.erl` (200 LOC): persistent log (Mnesia/DETS)

**Raft State Machine**:
```
Follower ←→ Candidate ← Leader
  ↑           ↓ (timeout)   ↓
  └─ election_timeout: 150-300ms (random)

States:
  follower:   accepts append_entries, responds to votes
  candidate:  requests votes, counts responses
  leader:     replicates log, maintains quorum
```

**Safety Properties**:
```erlang
%% Election Safety: At most one leader per term
?assert(length([L || L ← nodes, state(L) == leader, term(L) == T]) =< 1).

%% Log Matching: If logs match at index, history is identical
log_match(Leader, Follower, Index) →
  (leader_log(Index) == follower_log(Index)) →
    log_history_match(Index - 1).

%% Leader Completeness: Leader's log contains all committed entries
leader_has_all_committed_entries(Leader) →
  ∀entry ∈ committed. leader_log contains entry.
```

**Consensus algorithm** (from spec):
```erlang
%% APPEND_ENTRIES: Leader → Followers
for_each(follower) {
  entries := log_entries_to_append();
  send({append_entries, term, leader_id, entries});
}
ack_count := 1; %% leader counts itself

receive_responses(timeout = 500ms) {
  for_each(response {success, follower_pid}) {
    ack_count++;
    match_index[follower] := length(entries);
  }
}

quorum_needed := ceil((num_followers + 1) / 2);
if (ack_count >= quorum_needed) {
  commit_index := length(entries);
  notify_committed(entries);
  return {ok, committed_entries};
} else {
  rollback_pending_entries();
  return {error, quorum_failed};
}
```

**Test coverage** (72 tests):
- Leader election (10 tests)
- Log replication (15 tests)
- Safety checks (20 tests)
- Edge cases (27 tests)

---

### PHASE 4: INTELLIGENCE & ROUTING (Days 14-17)

**Key modules** (14 total, 2,000 LOC):
- `erlmcp_flow_pattern_store.erl` (250 LOC): HNSW index
- `erlmcp_flow_hnsw_index.erl` (300 LOC): hierarchy navigation
- `erlmcp_flow_moe_router.erl` (220 LOC): expert selection
- `erlmcp_flow_embeddings.erl` (180 LOC): text→vector

**HNSW Algorithm** (O(log N) search):
```erlang
%% Build phase: Insert patterns
for_each(pattern) {
  embedding := embed(pattern);  %% text → 384-dim vector
  add_to_hnsw(embedding, pattern);  %% O(log N)
}

%% Search phase: Find nearest neighbors
search(query, topK=10) {
  query_embedding := embed(query);
  entry_point := get_entry_point();  %% top layer

  for(layer = M-1; layer >= 0; layer--) {
    candidates := {entry_point};
    visited := {};

    while (candidates not empty) {
      c := pop(candidates);
      neighbors := layer_neighbors(c, layer);

      for_each(neighbor in neighbors) {
        distance := cosine_distance(query, neighbor);
        if (distance < threshold && not visited.contains(neighbor)) {
          candidates.add(neighbor);
          visited.add(neighbor);
        }
      }

      if (better_than_best) {
        entry_point := c;
      }
    }
  }

  return visited sorted by distance;  %% O(log N)
}
```

**MoE Routing** (select 8-16 experts):
```erlang
%% Gating network: selects best experts
{expert_scores} := gating_network(request);
sorted_experts := sort_by_score(expert_scores);
selected := take(topK=8, sorted_experts);

%% Load balance across experts
[parallel_invoke(expert, request) || expert ← selected].
```

**EWC++ Consolidation** (prevent catastrophic forgetting):
```erlang
%% After learning from new patterns:
consolidation() {
  for_each(parameter in model) {
    %% Fisher information matrix: importance of param
    fisher := compute_fisher_information(param);

    %% EWC loss: penalize change to important params
    ewc_loss := λ/2 * Σ(fisher * (θ_new - θ_old)²);

    total_loss := task_loss + ewc_loss;
  }

  %% Update model while preserving old knowledge
  optimizer.update(total_loss);
}.
```

**Test coverage** (59 tests):
- HNSW insertion/search (20 tests)
- MoE expert selection (15 tests)
- Embeddings generation (12 tests)
- EWC++ consolidation (12 tests)

---

### PHASE 5: SECURITY & OBSERVABILITY (Days 18-21)

**Key modules** (13 total, 1,830 LOC):
- `erlmcp_flow_tls_manager.erl` (200 LOC): TLS 1.3 handshake
- `erlmcp_flow_rate_limiter.erl` (150 LOC): token bucket
- `erlmcp_flow_auth.erl` (140 LOC): JWT validation
- `erlmcp_flow_otel_tracer.erl` (220 LOC): distributed tracing

**TLS 1.3 Handshake**:
```erlang
secure_connection(socket, tls_config) {
  %% Strict: TLS 1.3 only
  ssl:connect(socket, [
    {versions, [tlsv1_3]},
    {cert_file, CertPath},
    {key_file, KeyPath},
    {verify, verify_peer},
    {ca_file, CABundlePath},
    {ciphers, [aes_256_gcm, chacha20_poly1305, aes_128_gcm]}
  ]);

  %% Verify peer certificate
  {ok, Cert} = ssl:peercert(TLSSocket);
  verify_certificate(Cert, TrustedCA) → ok;

  %% Extract peer info
  {peer, Subject} := ssl:peername(TLSSocket);

  return {ok, {tls_socket, rate_limit_bucket}};
}
```

**HMAC-SHA256 Signing**:
```erlang
sign(data, secret) →
  crypto:mac(hmac, sha256, secret, data).

verify(data, signature, secret) →
  expected := crypto:mac(hmac, sha256, secret, data),
  crypto:hash_equals(signature, expected) → ok | {error, invalid}.
```

**OTEL Tracing**:
```erlang
%% Distributed trace with parent link
trace_request(request, parent_trace_id) {
  trace_id := uuid();
  span := otel:start_span(trace_id, operation_name, [
    {attributes, #{
        http_method => Method,
        http_url => URL,
        span_kind => server
      }},
    {parent_link, parent_trace_id}
  ]);

  otel:add_event(span, 'request_start', #{timestamp => now()});

  try
    result := handle_request(request),
    otel:add_event(span, 'request_complete', #{status => ok})
  catch E:R:S
    otel:add_event(span, 'request_error', #{error => E, reason => R}),
    otel:set_status(span, error)
  end,

  otel:end_span(span).
}
```

**Rate Limiting** (token bucket):
```erlang
rate_limiter:check(connection) {
  bucket := get_bucket(connection);

  if (bucket.tokens > 0) {
    bucket.tokens--;
    bucket.last_refill := now();
    return ok;
  } else {
    return {error, rate_limit_exceeded};
  }
}

%% Refill at regular intervals
refill_bucket() {
  tokens_to_add := (now() - last_refill) * rate_per_second;
  bucket.tokens := min(max_tokens, bucket.tokens + tokens_to_add);
}
```

**Test coverage** (58 tests):
- TLS handshake (15 tests)
- HMAC signing (10 tests)
- OTEL tracing (12 tests)
- Rate limiting (10 tests)
- Circuit breaker (11 tests)

---

## FINAL INTEGRATION & VALIDATION

### Day 21: Full System Test

```bash
# Compile all phases
make compile                  # 30s
# Expected: 0 errors, 8410 LOC

# Run all test suites
make test                     # 180s
# Expected: 259 tests pass, coverage ≥80%

# Quality gates
make dialyzer               # 90s
# Expected: 0 warnings

make xref                   # 30s
# Expected: 0 undefined functions

# Performance regression
make benchmark              # 300s
# Expected: regression < 10%

# Combined: Full check (180s parallel)
make check
```

### Integration Verification

```erlang
%% Phase 1 → Phase 2: Agents → Swarm
erlmcp_flow_agent:on_state_change(NewState) →
  erlmcp_flow_swarm:gossip(state_change, NewState) ✓

%% Phase 2 → Phase 3: Swarm → Consensus
erlmcp_flow_swarm:consensus_commit(entries) →
  erlmcp_flow_raft:append_entries(entries) ✓

%% Phase 3 → Phase 4: Consensus → Intelligence
erlmcp_flow_raft:committed_entries(entries) →
  erlmcp_flow_pattern_store:index_patterns(entries) ✓

%% Phase 4 → Phase 5: Intelligence → Security
erlmcp_flow_moe_router:select_experts(request) →
  erlmcp_flow_tls_manager:verify_auth(request) ✓

%% Phase 5: Observability
erlmcp_flow_request_tracker:record_request(req) →
  erlmcp_flow_otel_tracer:trace_request(req) ✓
```

### Documentation Completion

- [ ] API documentation (public functions)
- [ ] Architecture guide (with diagrams)
- [ ] Deployment guide (configuration, tuning)
- [ ] Examples (3-5 real-world use cases)
- [ ] Troubleshooting guide (common issues)

### Security Audit

- [ ] TLS configuration review (1.3 only, no old protocols)
- [ ] HMAC key rotation policy
- [ ] Encryption key storage (no hardcoding)
- [ ] Rate limiting effectiveness
- [ ] Authentication bypass tests
- [ ] Chaos injection (fault tolerance)

### Performance Benchmarks

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Agent spawn | <50ms | ? | TBD |
| Request routing | <10ms p99 | ? | TBD |
| Raft consensus | <500ms | ? | TBD |
| HNSW search (1M patterns) | <100ms | ? | TBD |
| TLS handshake | <2s | ? | TBD |
| Memory usage | <512MB | ? | TBD |
| CPU under load | <60% | ? | TBD |

---

## GIT WORKFLOW (MERGE-ONLY)

### Branch Strategy

```bash
# Main development branch
git checkout -b feature/erlmcp-flow-complete

# Phase 1 commit
git add apps/erlmcp_flow/src/erlmcp_flow_agent*.erl
git commit -m "feat(erlmcp-flow): Phase 1 - Agent Foundation

[Full commit message from spec]

https://claude.ai/code/session_XXXXX"

# Phase 2 commit (same pattern)
git add apps/erlmcp_flow/src/erlmcp_flow_swarm*.erl
git commit -m "feat(erlmcp-flow): Phase 2 - Swarm Coordination"

# ... repeat for phases 3, 4, 5 ...

# Final merge to main (NO REBASE)
git checkout main
git pull origin main
git merge --no-ff feature/erlmcp-flow-complete
git push origin main
```

**Critical Rules**:
- NO `git rebase` (preserve history)
- NO `git push --force` (destructive)
- NO `git commit --amend` after first push (rewrites history)
- NO `--no-verify` on commits (quality gates must pass)

---

## SUCCESS CRITERIA (CHECKLIST)

### Phase 1 Completion
- [ ] 13 modules (1570 LOC)
- [ ] 45 EUnit tests passing
- [ ] 10 CT tests passing
- [ ] Coverage ≥92%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined
- [ ] Code reviewed + approved
- [ ] Commit pushed

### Phase 2 Completion
- [ ] 12 modules (1470 LOC)
- [ ] Integration with Phase 1
- [ ] 38 EUnit + 15 CT tests passing
- [ ] Coverage ≥88%
- [ ] Commit pushed

### Phase 3 Completion
- [ ] 11 modules (1540 LOC)
- [ ] Raft consensus verified
- [ ] Safety + liveness proofs
- [ ] 52 EUnit + 20 CT tests passing
- [ ] Coverage ≥95%
- [ ] Commit pushed

### Phase 4 Completion
- [ ] 14 modules (2000 LOC)
- [ ] HNSW index O(log N) verified
- [ ] MoE routing tested
- [ ] 41 EUnit + 18 CT tests passing
- [ ] Coverage ≥85%
- [ ] Commit pushed

### Phase 5 Completion
- [ ] 13 modules (1830 LOC)
- [ ] TLS 1.3 verified
- [ ] OTEL tracing working
- [ ] 36 EUnit + 22 CT tests passing
- [ ] Coverage ≥98% (security critical)
- [ ] Security audit passed
- [ ] Commit pushed

### Final Integration
- [ ] All 63 modules compiling
- [ ] 259 tests passing
- [ ] 80%+ average coverage
- [ ] 0 Dialyzer warnings
- [ ] 0 Xref undefined
- [ ] All benchmarks passing
- [ ] Documentation complete
- [ ] Merged to main (no rebase)
- [ ] Release ready

---

## RISK MITIGATION

### Critical Paths

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Phase 1 gen_server design flawed | Blocks all phases | Architecture review before code |
| Raft consensus bugs | System unstable | Byzantine FT module + property tests |
| HNSW O(log N) not achieved | Performance fails | Benchmarking during Phase 4 |
| TLS compatibility issues | Security gap | TLS 1.3-only, no downgrade |
| Memory leaks in pattern store | Operational failure | ErlangGC + LRU monitoring |

### Escalation Paths

If **Quality Gate Fails**:
1. Do NOT use `--no-verify` (forbidden)
2. Analyze root cause
3. Fix issue in code
4. Re-stage files
5. Create NEW commit (not amend)

If **Test Suite Fails**:
1. Run test with verbose output
2. Identify failing test
3. Fix implementation
4. Re-run test suite
5. Verify coverage threshold

If **Performance Regression**:
1. Profile bottleneck
2. Optimize critical path
3. Re-benchmark
4. Verify regression < 10%

---

## CONCLUSION

This execution plan provides a systematic, phase-by-phase approach to implementing erlmcp-flow with:

✓ **Clear deliverables** per phase
✓ **Measurable acceptance criteria**
✓ **Parallel agent orchestration** (20 workers)
✓ **Continuous quality verification**
✓ **Joe Armstrong AGI principles** (let-it-crash, supervision)
✓ **Merge-only git workflow** (no rebase, preserve history)

**Expected outcome**: Production-ready erlmcp-flow in 21 days with <4-minute cloud gate.

