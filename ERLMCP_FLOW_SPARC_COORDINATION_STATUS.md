# erlmcp-flow SPARC Coordination Status

**Version**: 1.0.0
**Date**: 2026-02-02
**Roadmap**: 80/20 Lean Implementation (4 Weeks, 7 Modules)
**Status**: Ready to Begin

---

## SPARC Phase Definitions

| Phase | Activities | Deliverables |
|-------|-----------|--------------|
| **S**pecification | Define requirements, state machines, interfaces, edge cases | Spec document, FSM diagrams, API contracts |
| **P**seudocode | Sketch algorithms in comments, data structures, flow control | Pseudocode files, algorithm design |
| **A**rchitecture | Process structure, supervision tree, message flow, OTP patterns | Architecture diagrams, module design |
| **R**efinement | Chicago TDD implementation, tests, performance tuning | Working code, passing tests, benchmarks |
| **C**ompletion | Quality validation, code review, documentation, integration | Merged code, docs, quality gates passed |

---

## Module Overview (7 Modules)

| # | Module | Week | LOC | Tests | Owner |
|---|--------|------|-----|-------|-------|
| 1 | erlmcp_flow_agent | W1 | 150 | 10 | erlang-otp-developer |
| 2 | erlmcp_flow_swarm | W1 | 200 | 8 | erlang-otp-developer |
| 3 | erlmcp_flow_raft | W2 | 100 | 5 | erlang-otp-developer |
| 4 | erlmcp_flow_router | W2 | 80 | 4 | erlang-otp-developer |
| 5 | erlmcp_flow_error_handler | W3 | 120 | 4 | erlang-otp-developer |
| 6 | erlmcp_flow_sup | W3 | 80 | 3 | erlang-architect |
| 7 | erlmcp_flow (API) | W4 | 50 | 3 | erlang-otp-developer |
| **Total** | | | **780** | **37** | |

---

## Week 1: Foundation (Agent + Swarm)

### Module 1: erlmcp_flow_agent

**Purpose**: Individual agent gen_server with task execution, health monitoring, retries

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | FSM (idle→assigned→executing→done), API spec, error cases | None |
| **Pseudocode** | ⚪ Not Started | 0% | Algorithm comments for task queue, retry logic, health | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | gen_server callbacks, supervision, message flow | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 10 EUnit tests, coverage ≥80% | Depends on Architecture |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, code reviewed, documented | Depends on Refinement |

**State Machine**:
```
idle → assigned (task) → executing → done → idle
  ↓                         ↓
  error ← retry_failed ← timeout
```

**Interfaces**:
```erlang
start_link(Id, Opts) -> {ok, Pid} | {error, Reason}
assign_task(Pid, Task) -> ok | {error, Reason}
get_status(Pid) -> {Status, TaskInfo}
get_result(Pid) -> {ok, Result} | {error, Reason}
healthcheck(Pid) -> {ok, Stats} | timeout
```

**Dependencies**: None (foundation module)

---

### Module 2: erlmcp_flow_swarm

**Purpose**: Swarm coordinator, manages agents, task distribution, leader election

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | FSM (idle→coordinating→executing), agent registry, task queue | None |
| **Pseudocode** | ⚪ Not Started | 0% | Round-robin assignment, heartbeat tracking, agent spawn | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | gen_server, simple_one_for_one supervisor for agents | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 8 EUnit tests, coverage ≥80% | **Depends on Module 1** |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, integration with Module 1 | Depends on Refinement |

**State Machine**:
```
idle → coordinating (agents spawning) → executing (tasks running) → idle
  ↓                                        ↓
  error ← leader_lost ← agent_failure
```

**Interfaces**:
```erlang
start_link(SwarmId, Opts) -> {ok, Pid} | {error, Reason}
add_agents(Pid, Count) -> {ok, [AgentPid]}
submit_task(Pid, Task) -> {ok, TaskId} | {error, Reason}
get_swarm_status(Pid) -> #{agents, tasks, leader}
```

**Dependencies**:
- **Module 1** (erlmcp_flow_agent) - must be in Refinement phase

---

## Week 2: Consensus & Routing (Raft + Router)

### Module 3: erlmcp_flow_raft

**Purpose**: Minimal Raft consensus for leader election (no log replication in MVP)

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | FSM (follower→candidate→leader), election algorithm, quorum | None |
| **Pseudocode** | ⚪ Not Started | 0% | Vote request, heartbeat, term tracking, timeout logic | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | Pure module (no gen_server), called by swarm | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 5 EUnit tests, election validated | **Depends on Module 2** |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, integrated with swarm | Depends on Refinement |

**State Machine**:
```
follower → (timeout) → candidate → (quorum votes) → leader
  ↑                       ↓                           ↓
  └──── (higher term) ────┘         (heartbeat) → follower
```

**Interfaces**:
```erlang
start_election(Nodes) -> {ok, Leader} | {error, no_quorum}
heartbeat(Leader, Nodes) -> ok | {error, leader_dead}
is_leader(Pid) -> true | false
get_current_term() -> Term
```

**Dependencies**:
- **Module 2** (erlmcp_flow_swarm) - must be in Completion phase

---

### Module 4: erlmcp_flow_router

**Purpose**: gproc-based routing, agent lookup, load balancing

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | Registry interface, lookup API, load balancing strategy | None |
| **Pseudocode** | ⚪ Not Started | 0% | gproc key-value, least-used-first algorithm | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | Pure module, stateless routing functions | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 4 EUnit tests, routing validated | Can run parallel with Module 3 |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, integrated with swarm | Depends on Refinement |

**Interfaces**:
```erlang
register_agent(Id, Pid) -> ok | {error, Reason}
lookup_agent(Id) -> {ok, Pid} | {error, not_found}
route_task(AgentId, Task) -> ok | {error, Reason}
agent_list() -> [Pid]
```

**Dependencies**: None (can develop in parallel with Module 3)

---

## Week 3: Error Recovery & Integration (Error Handler + Supervisor)

### Module 5: erlmcp_flow_error_handler

**Purpose**: Task retry, agent crash recovery, leader election triggers

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | Error scenarios (task timeout, agent crash, leader loss), recovery strategies | None |
| **Pseudocode** | ⚪ Not Started | 0% | Retry logic (max 3), exponential backoff, event handling | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | gen_server, supervisor notifications, recovery coordination | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 4 EUnit tests, recovery scenarios | **Depends on Modules 2, 3** |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, integrated with all components | Depends on Refinement |

**Interfaces**:
```erlang
start_link(Opts) -> {ok, Pid}
handle_task_timeout(TaskId) -> ok
handle_agent_crash(AgentPid) -> ok
handle_leader_lost(SwarmId) -> ok
```

**Dependencies**:
- **Module 2** (erlmcp_flow_swarm) - must be in Completion phase
- **Module 3** (erlmcp_flow_raft) - must be in Completion phase

---

### Module 6: erlmcp_flow_sup

**Purpose**: 3-tier supervision tree (root → swarm → agent)

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | Supervision strategies (one_for_all, one_for_one, simple_one_for_one) | None |
| **Pseudocode** | ⚪ Not Started | 0% | Child specs, restart strategies, tier structure | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | 3-tier supervision diagram, restart semantics | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 3 integration tests (CT), cascade validation | **Depends on Modules 1-5** |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, full system supervised | Depends on Refinement |

**Supervision Tree**:
```
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server)
├── erlmcp_flow_raft (gen_server wrapper)
├── erlmcp_flow_swarm_sup (simple_one_for_one)
│   └── erlmcp_flow_swarm (gen_server)
│       └── erlmcp_flow_agent_sup (simple_one_for_one)
│           └── erlmcp_flow_agent (gen_server)
└── erlmcp_flow_error_handler (gen_server)
```

**Dependencies**:
- **All Modules 1-5** must be in Completion phase

---

## Week 4: Testing & Polish (API + Final Validation)

### Module 7: erlmcp_flow (API Facade)

**Purpose**: Public API, user-facing functions, application entry point

| Phase | Status | Progress | Deliverables | Blockers |
|-------|--------|----------|--------------|----------|
| **Specification** | 🟡 Pending | 0% | Public API functions, usage examples, error handling | None |
| **Pseudocode** | ⚪ Not Started | 0% | API call flow, validation, delegation to internal modules | Depends on Spec |
| **Architecture** | ⚪ Not Started | 0% | Facade pattern, clean API surface | Depends on Pseudocode |
| **Refinement** | ⚪ Not Started | 0% | Implementation + 3 EUnit tests, API validated | **Depends on Module 6** |
| **Completion** | ⚪ Not Started | 0% | Quality gates pass, examples documented, v0.1.0-alpha | Depends on Refinement |

**Interfaces**:
```erlang
start() -> ok
start_swarm(SwarmId, Opts) -> {ok, Pid}
submit_task(SwarmId, Task) -> {ok, TaskId}
get_status(SwarmId) -> #{agents, tasks, status}
stop_swarm(SwarmId) -> ok
```

**Dependencies**:
- **Module 6** (erlmcp_flow_sup) - must be in Completion phase

---

## Weekly SPARC Coordination Summary

### Week 1 Goals (Day 1-5)
- **Modules**: 1 (agent), 2 (swarm)
- **SPARC Focus**: S→P→A→R phases for both modules
- **Target**: Module 1 in Completion by Day 2, Module 2 in Completion by Day 5
- **Blockers**: None (foundation week)
- **Quality Gate**: `rebar3 compile && rebar3 eunit` passing for both modules

### Week 2 Goals (Day 6-10)
- **Modules**: 3 (raft), 4 (router)
- **SPARC Focus**: Modules 3-4 through S→P→A→R→C
- **Target**: Both modules in Completion by Day 10
- **Blockers**: Module 3 depends on Module 2 completion
- **Quality Gate**: Leader election working, routing tests pass

### Week 3 Goals (Day 11-15)
- **Modules**: 5 (error handler), 6 (supervisor)
- **SPARC Focus**: Error recovery + supervision integration
- **Target**: Full supervision tree operational by Day 15
- **Blockers**: Module 5 depends on Modules 2-3, Module 6 depends on all 1-5
- **Quality Gate**: `rebar3 ct` integration tests passing

### Week 4 Goals (Day 16-20)
- **Modules**: 7 (API facade)
- **SPARC Focus**: API design, documentation, v0.1.0-alpha
- **Target**: Release-ready by Day 20
- **Blockers**: Depends on Module 6 completion
- **Quality Gate**: `make check` fully passing, examples working, tag v0.1.0-alpha

---

## SPARC Phase Transition Criteria

### Specification → Pseudocode
- ✅ Requirements documented
- ✅ State machine defined
- ✅ Interface specification complete
- ✅ Edge cases identified
- ✅ Dependencies mapped

### Pseudocode → Architecture
- ✅ Algorithm sketched in comments
- ✅ Data structures defined
- ✅ Control flow documented
- ✅ Agent assigned to implementation

### Architecture → Refinement
- ✅ OTP behavior selected (gen_server/gen_statem/supervisor)
- ✅ Supervision strategy defined
- ✅ Message flow designed
- ✅ Module structure approved

### Refinement → Completion
- ✅ Code implemented with Chicago TDD
- ✅ All tests passing (EUnit/CT)
- ✅ Coverage ≥ 80%
- ✅ No regressions in dependent modules
- ✅ Benchmarks within target (if applicable)

### Completion Checklist
- ✅ Compile: 0 errors
- ✅ EUnit: 0 failures
- ✅ Dialyzer: 0 warnings
- ✅ Xref: 0 undefined
- ✅ Code reviewed
- ✅ Documentation complete
- ✅ Integrated with dependent modules

---

## Dependency Graph

```
Module 1 (agent)
  ↓
Module 2 (swarm) ────┐
  ↓                   ↓
Module 3 (raft) ──→ Module 5 (error_handler)
                      ↓
Module 4 (router) ──→ Module 6 (supervisor)
                      ↓
                    Module 7 (API)
```

**Critical Path**: Module 1 → Module 2 → Module 3 → Module 5 → Module 6 → Module 7

**Parallel Opportunities**:
- Week 2: Module 3 and Module 4 can be developed in parallel (both depend on Module 2)
- Week 1: Module 1 Specification/Pseudocode/Architecture can overlap with Module 2 Specification

---

## Agent Assignments

| Module | Lead Agent | Supporting Agents | Phase Coverage |
|--------|-----------|-------------------|----------------|
| 1 | erlang-otp-developer | erlang-test-engineer, code-reviewer | All SPARC phases |
| 2 | erlang-otp-developer | erlang-test-engineer, code-reviewer | All SPARC phases |
| 3 | erlang-researcher | erlang-otp-developer, erlang-test-engineer | S→P, then handoff |
| 4 | erlang-otp-developer | - | All SPARC phases |
| 5 | erlang-otp-developer | erlang-test-engineer | All SPARC phases |
| 6 | erlang-architect | erlang-otp-developer, erlang-test-engineer | S→P→A, then handoff |
| 7 | plan-designer | erlang-otp-developer, code-reviewer | All SPARC phases |

---

## Risk Register

| Risk | Impact | Probability | Mitigation | Owner |
|------|--------|-------------|------------|-------|
| Module 2 (swarm) complexity underestimated | High | Medium | Add 1-day buffer to Week 1 | erlang-architect |
| Raft consensus edge cases | Medium | High | Research existing Erlang Raft libs (ra, partisan) | erlang-researcher |
| Integration issues Week 3 | High | Medium | Daily integration tests from Week 2 | verifier |
| Coverage not reaching 80% | Medium | Low | Prioritize testability in Architecture phase | erlang-test-engineer |
| Dependency blocking progress | High | Low | Parallel work where possible (Module 3+4) | sparc-orchestrator |

---

## Communication Plan

### Daily Standups (async)
- **Time**: Start of each work session
- **Format**: Status update via receipt generation
- **Topics**: Yesterday's progress, today's plan, blockers

### Weekly SPARC Review
- **Time**: End of each week (Day 5, 10, 15, 20)
- **Format**: SPARC coordination status update
- **Topics**:
  - Modules completed this week
  - SPARC phase transitions
  - Blockers resolved/new
  - Next week preview

### Phase Gate Reviews
- **Trigger**: Module transitioning between SPARC phases
- **Attendees**: Lead agent + code-reviewer
- **Checklist**: Transition criteria (see above)

---

## Metrics & KPIs

### Weekly Targets

| Week | Modules in Completion | Tests Passing | Coverage | Blockers Resolved |
|------|-----------------------|---------------|----------|-------------------|
| 1 | 2 (agent, swarm) | 18/18 | ≥80% | 0 (none expected) |
| 2 | 4 (raft, router) | 9/9 | ≥80% | 1 (raft complexity) |
| 3 | 6 (error, sup) | 7/7 | ≥80% | 1 (integration) |
| 4 | 7 (API) | 3/3 + 6 CT | ≥80% | 0 |

### Velocity Tracking

| Metric | Week 1 | Week 2 | Week 3 | Week 4 |
|--------|--------|--------|--------|--------|
| LOC written | 350 | 180 | 200 | 50 |
| Tests written | 18 | 9 | 7 | 9 |
| Modules completed | 2 | 2 | 2 | 1 |
| Rework incidents | 0 | __ | __ | __ |

---

## Current Status (2026-02-02)

### Overall Progress
- **Phase**: Pre-implementation (Week 0)
- **Modules Completed**: 0/7
- **Tests Passing**: 0/37
- **Blockers**: None

### This Week (Week 1)
- **Focus**: Module 1 (agent) + Module 2 (swarm)
- **SPARC Phases Active**: Specification for both modules
- **Agents Deployed**: erlang-otp-developer (lead)
- **Risks**: None

### Next Week (Week 2)
- **Focus**: Module 3 (raft) + Module 4 (router)
- **Prerequisites**: Module 2 must reach Completion
- **Agents Needed**: erlang-researcher (raft), erlang-otp-developer (router)

---

## Appendix: SPARC Workflow Templates

### Specification Template
```markdown
## Module: <name>

### Purpose
<One-sentence description>

### Requirements
- FR-1: <Functional requirement>
- NFR-1: <Non-functional requirement>

### State Machine
<ASCII diagram or Mermaid syntax>

### Interface
<Function signatures with types>

### Edge Cases
- Case 1: <description>
- Case 2: <description>

### Dependencies
- Module X (Phase Y)
```

### Pseudocode Template
```erlang
%% Module: <name>
%% Phase: Pseudocode
%% Date: <date>

%% ALGORITHM: <name>
%% INPUT: <params>
%% OUTPUT: <return>
%% STEPS:
%%   1. <step>
%%   2. <step>
%%   3. <step>

%% DATA STRUCTURES:
%% - State: #{field => type}
%% - Queue: queue:queue()

%% CONTROL FLOW:
%% init/1:
%%   - Initialize state
%%   - Return {ok, State}
%%
%% handle_call/3:
%%   - Match request
%%   - Process
%%   - Reply
```

### Architecture Template
```markdown
## Module: <name>

### OTP Behavior
<gen_server | gen_statem | supervisor>

### Callbacks
- init/1: <purpose>
- handle_call/3: <purpose>
- handle_cast/2: <purpose>

### Supervision
- Strategy: <one_for_one | simple_one_for_one | one_for_all>
- Parent: <supervisor name>
- Children: <child specs>

### Message Flow
<Sequence diagram or description>
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-02-02 | Initial SPARC coordination status document |

---

## Next Actions

1. ✅ Review this coordination status with team
2. ⏳ Begin Module 1 Specification phase
3. ⏳ Assign erlang-otp-developer to Module 1
4. ⏳ Schedule Week 1 kickoff
5. ⏳ Set up daily receipt generation

---

**Status**: Ready to Begin Week 1
**Next Review**: End of Week 1 (Day 5)
**Document Owner**: sparc-orchestrator

---

**End of SPARC Coordination Status**
