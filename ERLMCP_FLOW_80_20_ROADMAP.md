# erlmcp-flow: 80/20 Implementation Roadmap (4 Weeks)

**Focus**: 20% of effort → 80% of value
**Scope**: Working multi-agent orchestration MVP (v0.1.0-alpha)
**Skip**: Advanced optimization, Byzantine consensus, Gossip, fancy observability

---

## The 80/20 Insight

From 12-week roadmap, identify critical path:
- ✅ Agent gen_server (foundation)
- ✅ Swarm coordinator (orchestration)
- ✅ Raft consensus (leader election only, 1 leader per swarm)
- ✅ Message routing (gproc registry)
- ✅ Error recovery (task requeue, agent restart)

**Skip (Do Later)**:
- ❌ Byzantine PBFT (Raft is sufficient for internal agents)
- ❌ Gossip protocol (Raft is better for consistency)
- ❌ Vector quantization/HNSW (premature optimization)
- ❌ Advanced observability (log to stdout is fine)
- ❌ Full chaos testing (happy path first)
- ❌ Performance benchmarks (validate at 10K msg/s)
- ❌ Semantic versioning CI/CD (MVP phase)

---

## Week 1: Foundation (4 Days)

### Day 1-2: Agent Framework (gen_server)
**Output**: `erlmcp_flow_agent.erl` + tests

```erlang
-module(erlmcp_flow_agent).
-behaviour(gen_server).

% State: idle | assigned | executing | done
% init/1 non-blocking
% handle_call/3 for result queries
% handle_cast/2 for task assignments
% handle_info/2 for timeouts
```

**Code**:
- State record: `{id, status, task, result, stats, health}`
- Callbacks: `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`
- Task queue (simple list, max 100 items)
- Error recovery: max 3 retries, exponential backoff (100ms → 500ms)
- Health checks: 10s heartbeat to swarm coordinator

**Tests**: 8 EUnit cases
- Agent spawn/shutdown
- Task assignment → execution → result
- Task timeout (5s default)
- Concurrent tasks (load handling)
- Agent crash recovery
- Health check interval
- Invalid task handling
- State machine transitions

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ EUnit (8 tests)

---

### Day 3-4: Swarm Coordinator (gen_server)
**Output**: `erlmcp_flow_swarm.erl` + tests

```erlang
-module(erlmcp_flow_swarm).
-behaviour(gen_server).

% State: idle | coordinating | executing
% init/1 spawns Raft + registry
% handle_call/3 for task submission
% handle_cast/2 for agent heartbeats
```

**Code**:
- State: `{id, agents (set), tasks (queue), leader_pid, raft_state}`
- Supervision: `simple_one_for_one` for agents
- Task queue: FIFO, max 10K tasks
- Agent health tracking: 3 missed heartbeats = dead agent, remove from set
- Leader election via Raft (minimal: single term, no persistence)
- Task assignment: round-robin to healthy agents

**Tests**: 6 EUnit cases
- Swarm creation with 3+ agents
- Task submission → assignment → execution
- Agent health monitoring (heartbeat tracking)
- Agent removal on 3 missed heartbeats
- Concurrent task handling (100 tasks)
- Swarm shutdown

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ EUnit (6 tests)

---

## Week 2: Consensus & Routing (4 Days)

### Day 1-2: Raft Consensus (Minimal)
**Output**: `erlmcp_flow_raft.erl` (pure module) + tests

**Scope**: Leader election only (no log replication for MVP)

```erlang
-module(erlmcp_flow_raft).

% Functions:
% - start_election(Nodes) → Leader | none (after timeout)
% - heartbeat(Leader, Nodes) → ok | leader_dead
% - is_leader(Pid) → true | false
```

**Code** (100 lines):
- 3 states: Follower, Candidate, Leader
- Election: Random timeout (150-300ms), request votes from quorum
- Heartbeat: Leader sends every 100ms
- Quorum: N/2 + 1 votes required
- No log replication (single term only)
- Term tracking for conflict detection

**Tests**: 5 EUnit cases
- Single node election
- 3-node quorum (1 leader emerges)
- Leader detection via heartbeat
- Candidate timeout → election restart
- Split quorum (> 1 node down)

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ EUnit (5 tests)

---

### Day 3-4: Message Routing (gproc)
**Output**: `erlmcp_flow_router.erl` + tests

**Scope**: Registry-based routing (reuse erlmcp patterns)

```erlang
-module(erlmcp_flow_router).

% Functions:
% - register_agent(Id, Pid) → ok
% - lookup_agent(Id) → Pid | not_found
% - route_task(AgentId, Task) → ok | error
% - agent_list() → [Pid]
```

**Code** (80 lines):
- gproc global registry (n:AgentId key-value)
- Load balancing: least-used-first (query agent task queue size)
- Agent discovery: agent_list() for swarm coordinator
- Failure: timeout on route_task (5s), return error

**Tests**: 4 EUnit cases
- Register/lookup agent
- Route task to agent
- Multi-agent discovery
- Routing timeout on dead agent

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ EUnit (4 tests)

---

## Week 3: Error Recovery & Integration (4 Days)

### Day 1-2: Error Recovery (gen_server)
**Output**: `erlmcp_flow_error_handler.erl` + tests

```erlang
-module(erlmcp_flow_error_handler).
-behaviour(gen_server).

% Handles:
% - Task timeouts (5s)
% - Agent crashes
% - Raft leader down
% - Routing failures
```

**Code**:
- Task failure handler: requeue task (max 3 attempts)
- Agent crash detection: supervisor notifies handler
- Leader down: trigger new election, reassign tasks
- Routing failure: fallback to any healthy agent

**Tests**: 4 EUnit cases
- Task timeout recovery (requeue)
- Agent crash → supervisor restart
- Leader election triggered on leader crash
- Routing fallback on primary failure

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ EUnit (4 tests)

---

### Day 3-4: Integration & Supervision Tree
**Output**: `erlmcp_flow_sup.erl` (supervisor structure)

**Scope**: Wire all components together

```erlang
erlmcp_flow_sup
├── erlmcp_flow_registry (gen_server, gproc init)
├── erlmcp_flow_raft (gen_server, leader election)
├── erlmcp_flow_swarm_sup (simple_one_for_one)
│   └── erlmcp_flow_swarm (per-swarm instance)
│       └── erlmcp_flow_agent_sup (simple_one_for_one)
│           └── erlmcp_flow_agent (per-agent)
└── erlmcp_flow_error_handler (gen_server, monitors tasks)
```

**Code**:
- TIER 1: root supervisor (one_for_all, registry + raft + swarms)
- TIER 2: swarm supervisors (one_for_one, per-swarm isolation)
- TIER 3: agent supervisors (simple_one_for_one, per-agent isolation)
- Error handler: monitors via `supervisor:which_children/1`

**Tests**: 3 Integration Test cases
- Supervision tree startup
- Agent spawning under swarm
- Cascade restart on root failure
- Child isolation (1 agent crash doesn't affect others)

**Quality Gate**: ✅ Compile + ✅ Xref + ✅ CT (3 integration tests)

---

## Week 4: Testing & Polish (4 Days)

### Day 1: EUnit Consolidation
**Target**: 80% coverage of core modules

**Code to test**:
- `erlmcp_flow_agent.erl` (10 tests)
- `erlmcp_flow_swarm.erl` (8 tests)
- `erlmcp_flow_raft.erl` (5 tests)
- `erlmcp_flow_router.erl` (4 tests)
- `erlmcp_flow_error_handler.erl` (4 tests)

**Total**: 31 EUnit test cases

**Quality Gate**: `rebar3 eunit` → all pass, ≥80% coverage

---

### Day 2: Integration Tests
**Target**: Happy path + single failures

**Tests** (6 Common Test cases):
1. Task creation → assignment → execution → completion
2. Agent crash recovery
3. Swarm coordinator election
4. Task timeout + requeue
5. Multi-swarm isolation
6. Leader change during task execution

**Quality Gate**: `rebar3 ct --suite=test/erlmcp_flow_integration_SUITE`

---

### Day 3: Documentation & Examples
**Output**:
- `apps/erlmcp_flow/README.md` - Quick start
- `docs/ERLMCP_FLOW_MVP_ARCHITECTURE.md` - Design overview
- `examples/erlmcp_flow_demo.erl` - Working example (spawn 3 agents, execute 10 tasks)

**Quality Gate**: ✅ Docs reviewed, examples compile

---

### Day 4: Version & Commit
**Output**: v0.1.0-alpha

**Checklist**:
- [ ] All code compiles (0 errors)
- [ ] All tests pass (31 EUnit + 6 CT)
- [ ] Xref clean (0 undefined)
- [ ] Dialyzer clean (0 warnings on core modules)
- [ ] Coverage ≥80% (core modules)
- [ ] No format errors (`rebar3 format --verify`)
- [ ] Git commits with clear messages
- [ ] Tag v0.1.0-alpha

**Quality Gate**: `make check` passes

---

## Module Count (Minimal)

| Module | Type | Lines | Status |
|--------|------|-------|--------|
| erlmcp_flow_agent | gen_server | 150 | Week 1 |
| erlmcp_flow_swarm | gen_server | 200 | Week 1 |
| erlmcp_flow_raft | module | 100 | Week 2 |
| erlmcp_flow_router | module | 80 | Week 2 |
| erlmcp_flow_error_handler | gen_server | 120 | Week 3 |
| erlmcp_flow_sup | supervisor | 80 | Week 3 |
| erlmcp_flow | API facade | 50 | Week 4 |
| **Total** | | **780** | **✅** |

**Compare to 12-week roadmap**: 63 modules → 7 modules (88% reduction)

---

## Testing (Minimal)

| Suite | Cases | LOC | Status |
|-------|-------|-----|--------|
| erlmcp_flow_agent_tests.erl | 10 | 200 | Week 1 |
| erlmcp_flow_swarm_tests.erl | 8 | 160 | Week 1 |
| erlmcp_flow_raft_tests.erl | 5 | 100 | Week 2 |
| erlmcp_flow_router_tests.erl | 4 | 80 | Week 2 |
| erlmcp_flow_error_tests.erl | 4 | 80 | Week 3 |
| erlmcp_flow_integration_SUITE.erl | 6 | 150 | Week 4 |
| **Total** | **37** | **770** | **✅** |

**Coverage Target**: ≥80% (core modules only)

---

## Performance Targets (MVP)

| Metric | Target | Note |
|--------|--------|------|
| Task throughput | 10K msg/s | Scale to 500K later |
| Agent latency | <500ms p99 | Simple, not optimized |
| Memory | <500MB | Process overhead only |
| Task loss | 0% | Retries handle failures |
| Agent recovery | <2s | Supervisor restart |

**Not in scope**: Benchmarks, optimization, stress testing

---

## Known Limitations (Document for Later)

1. **No Byzantine PBFT**: Raft only (sufficient for trusted agents)
2. **No Gossip**: Raft is deterministic (no eventual consistency needed)
3. **No Log Persistence**: Single term, no durability (restart = election)
4. **No Vector Quantization**: HNSW deferred
5. **No Advanced Monitoring**: stdout logging, no OTEL
6. **No Chaos Testing**: Happy path + single failures only
7. **No CI/CD Matrix**: OTP 28 only (remove 26/27 matrix)
8. **No Release Pipeline**: Manual tag, no automated releases

---

## 80/20 Outcome

**In 4 Weeks**:
- ✅ 7 core modules (vs 63 in 12-week plan)
- ✅ 37 test cases (vs 343+ in 12-week plan)
- ✅ 780 LOC of code (vs 5,500+ in 12-week plan)
- ✅ Working multi-agent system
- ✅ Raft consensus for leader election
- ✅ Error recovery with retries
- ✅ gproc registry routing
- ✅ v0.1.0-alpha ready to ship

**Deferred to v0.2.0+**:
- Advanced optimization (week 11-12 of original plan)
- Byzantine PBFT consensus
- Gossip protocol
- Observability/OTEL
- Full chaos testing
- Performance benchmarking

---

## Success Criteria

**Week 1**: Agent + Swarm running, tasks execute
**Week 2**: Raft election working, routing stable
**Week 3**: Error recovery tested, supervision tree complete
**Week 4**: All tests pass, v0.1.0-alpha tagged

**Gate**: `git log --oneline | head -20` shows clean commits
**Gate**: `rebar3 do eunit, ct` runs cleanly
**Gate**: `git tag v0.1.0-alpha` created

---

## Parallelization Strategy

**Weeks 1-2**: Can parallelize (independent modules)
- Day 1: Agent + Raft (parallel)
- Day 2: Swarm + Router (parallel)
- Day 3: Error handler + Integration (sequential - depends on others)

**Estimated Speedup**: 1.8x actual time (vs 2.8x EPIC 9 potential)
**Realistic Timeline**: 3 weeks actual + 1 week buffer = **4 weeks**

---

## Git Strategy

**Branch**: `claude/erlmcp-flow-implementation` (off this branch)
**Commits**: Daily atomic commits per module
**Pattern**: `feat(erlmcp_flow): Implement <module_name> with tests`

**No rebase**: Merge commits preserve history
**Quality Gate**: Every commit must pass `make check`

---

## Conclusion

**The 80/20 principle applied**:
- Focus: Agent framework + Swarm + Raft + Router
- Skip: Optimization + Byzantine + Gossip + Observability
- Result: v0.1.0-alpha in 4 weeks (2 weeks faster than 12-week plan)
- Value: 80% of core functionality, 20% of effort

**This is the MVP**. Later versions (0.2, 0.3) add optimization, advanced consensus, observability.

---

## Next Action

1. ✅ Merge remote main (done)
2. ⏳ Create branch `claude/erlmcp-flow-implementation`
3. ⏳ Begin Week 1: Day 1 (Agent framework)
4. ⏳ Push daily commits
5. ⏳ Tag v0.1.0-alpha on Day 28

**Estimated start**: Immediate
**Estimated v0.1.0-alpha release**: 4 weeks
