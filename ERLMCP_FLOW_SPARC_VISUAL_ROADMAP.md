# erlmcp-flow SPARC Visual Roadmap

**Version**: 1.0.0
**Date**: 2026-02-02
**Timeline**: 4 Weeks (20 Days)
**Modules**: 7 (80/20 Lean Implementation)

---

## SPARC Phase Flow Across 4 Weeks

```
LEGEND:
S = Specification
P = Pseudocode
A = Architecture
R = Refinement
C = Completion
```

---

## Week 1: Foundation (Agent + Swarm)

```
Day 1              Day 2              Day 3              Day 4              Day 5
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Module 1    │    │ Module 1    │    │ Module 2    │    │ Module 2    │    │ Module 2    │
│ (Agent)     │    │ (Agent)     │    │ (Swarm)     │    │ (Swarm)     │    │ (Swarm)     │
│             │    │             │    │             │    │             │    │             │
│ S ████████  │    │ R ████████  │    │ S ████████  │    │ A ████████  │    │ R ████████  │
│ P ████████  │    │ C ████████  │    │ P ████████  │    │ R ████████  │    │ C ████████  │
│ A ████████  │    │             │    │             │    │             │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘

Deliverables:      Deliverables:      Deliverables:      Deliverables:      Deliverables:
- Agent spec       - Agent code       - Swarm spec       - Swarm arch       - Swarm code
- Agent FSM        - 10 tests         - Swarm FSM        - Supervision      - 8 tests
- API design       - Coverage 80%     - API design       - Integration      - Integration
                   - Module 1 ✅                          - Tests drafted    - Module 2 ✅
```

**Agents Active**:
- erlang-otp-developer (lead, Module 1+2)
- erlang-test-engineer (tests, Module 1+2)
- code-reviewer (quality gates)

**Outputs**:
- 2 modules in Completion
- 18/18 tests passing
- 350 LOC written
- Coverage ≥80%

---

## Week 2: Consensus & Routing (Raft + Router)

```
Day 6              Day 7              Day 8              Day 9              Day 10
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Module 3    │    │ Module 3    │    │ Module 3    │    │ Module 4    │    │ Module 4    │
│ (Raft)      │    │ (Raft)      │    │ (Raft)      │    │ (Router)    │    │ (Router)    │
│             │    │             │    │             │    │             │    │             │
│ S ████████  │    │ A ████████  │    │ R ████████  │    │ S ████████  │    │ R ████████  │
│ P ████████  │    │ R ████████  │    │ C ████████  │    │ P ████████  │    │ C ████████  │
│             │    │             │    │             │    │ A ████████  │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘

Deliverables:      Deliverables:      Deliverables:      Deliverables:      Deliverables:
- Raft spec        - Raft arch        - Raft code        - Router spec      - Router code
- Election FSM     - Pure module      - 5 tests          - gproc design     - 4 tests
- Research done    - Message flow     - Leader working   - Load balancing   - Routing ✅
                   - Quorum logic     - Module 3 ✅      - Pseudocode       - Module 4 ✅
```

**Agents Active**:
- erlang-researcher (Raft specification, Day 6)
- erlang-otp-developer (Raft + Router implementation)
- erlang-test-engineer (tests)
- code-reviewer (quality gates)

**Outputs**:
- 2 modules in Completion (cumulative: 4/7)
- 9/9 tests passing
- 180 LOC written
- Raft election validated

**Note**: Module 4 (Router) can start Day 9 in parallel with Module 3 Refinement

---

## Week 3: Error Recovery & Supervision (Error Handler + Supervisor)

```
Day 11             Day 12             Day 13             Day 14             Day 15
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Module 5    │    │ Module 5    │    │ Module 5    │    │ Module 6    │    │ Module 6    │
│ (Error)     │    │ (Error)     │    │ (Error)     │    │ (Supervisor)│    │ (Supervisor)│
│             │    │             │    │             │    │             │    │             │
│ S ████████  │    │ A ████████  │    │ R ████████  │    │ S ████████  │    │ R ████████  │
│ P ████████  │    │ R ████████  │    │ C ████████  │    │ P ████████  │    │ C ████████  │
│             │    │             │    │             │    │ A ████████  │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘

Deliverables:      Deliverables:      Deliverables:      Deliverables:      Deliverables:
- Error spec       - Error arch       - Error code       - Sup spec         - Sup code
- Recovery FSM     - gen_server       - 4 tests          - 3-tier tree      - 3 CT tests
- Retry logic      - Supervisor       - Integration      - Child specs      - Full system
- Edge cases       - Event handling   - Module 5 ✅      - Strategies       - Module 6 ✅
```

**Agents Active**:
- erlang-architect (supervision tree design, Day 14)
- erlang-otp-developer (implementation)
- erlang-test-engineer (integration tests)
- verifier (daily integration tests)

**Outputs**:
- 2 modules in Completion (cumulative: 6/7)
- 7/7 tests passing (4 EUnit + 3 CT)
- 200 LOC written
- Full supervision tree operational

**Critical**: Module 5 requires Modules 2+3 in Completion, Module 6 requires Modules 1-5 in Completion

---

## Week 4: API & Final Polish (API Facade + Validation)

```
Day 16             Day 17             Day 18             Day 19             Day 20
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Module 7    │    │ Module 7    │    │ Module 7    │    │ Final       │    │ Release     │
│ (API)       │    │ (API)       │    │ (API)       │    │ Validation  │    │ v0.1.0-alpha│
│             │    │             │    │             │    │             │    │             │
│ S ████████  │    │ A ████████  │    │ R ████████  │    │ make check  │    │ Tag +       │
│ P ████████  │    │ R ████████  │    │ C ████████  │    │ Docs        │    │ PR (if req) │
│             │    │             │    │ Examples    │    │ Benchmarks  │    │ Archive     │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘

Deliverables:      Deliverables:      Deliverables:      Deliverables:      Deliverables:
- API spec         - API arch         - API code         - All 37 tests ✅  - v0.1.0-alpha
- Facade design    - Facade pattern   - 3 tests          - Coverage ≥80%    - Git tag
- Usage examples   - Validation       - 6 CT tests       - Dialyzer clean   - README
- Error handling   - Documentation    - Module 7 ✅      - Xref clean       - Examples
```

**Agents Active**:
- plan-designer (API design, examples)
- erlang-otp-developer (implementation)
- code-reviewer (final review)
- verifier (quality gates)
- erlang-github-ops (release prep)

**Outputs**:
- 1 module in Completion (cumulative: 7/7)
- 9/9 tests passing (3 EUnit + 6 CT)
- 50 LOC written
- v0.1.0-alpha released

---

## Dependency Flow

```
Week 1                Week 2                Week 3                Week 4
┌──────────┐          ┌──────────┐          ┌──────────┐          ┌──────────┐
│ Module 1 │──────────┤ Module 3 │──────────┤ Module 5 │──────────┤ Module 7 │
│ (Agent)  │          │ (Raft)   │          │ (Error)  │          │ (API)    │
└──────────┘          └──────────┘          └──────────┘          └──────────┘
     │                                            │                     │
     │                                            │                     │
     ▼                                            ▼                     │
┌──────────┐          ┌──────────┐          ┌──────────┐              │
│ Module 2 │          │ Module 4 │          │ Module 6 │◄─────────────┘
│ (Swarm)  │──────────┤ (Router) │──────────┤  (Sup)   │
└──────────┘          └──────────┘          └──────────┘

Dependencies:
Module 2 depends on Module 1
Module 3 depends on Module 2
Module 5 depends on Modules 2+3
Module 6 depends on Modules 1-5
Module 7 depends on Module 6
Module 4 is independent (can parallel with Module 3)
```

---

## SPARC Phase Cascade

```
Module 1: S─P─A───R───C
          │ │ │   │   │
Module 2: │ │ └─→ S─P─A───R───C
          │ │         │   │   │
Module 3: │ │         │   └─→ S─P─A───R───C
          │ │         │               │   │
Module 4: │ │         └──────────────→S─P─A───R───C
          │ │                                 │   │
Module 5: │ │                                 └─→ S─P─A───R───C
          │ │                                             │   │
Module 6: │ │                                             └─→ S─P─A───R───C
          │ │                                                         │   │
Module 7: │ │                                                         └─→ S─P─A───R───C
          │ │                                                                     │   │
Release:  │ │                                                                     └─→ v0.1.0-alpha
```

**Legend**:
- `─` = Sequential dependency
- `─→` = Triggers next module
- Each letter represents ~0.5-2 days depending on module complexity

---

## Critical Path Analysis

**Longest Path**: Module 1 → Module 2 → Module 3 → Module 5 → Module 6 → Module 7

```
Module 1: 2 days (S+P+A: 1 day, R+C: 1 day)
  ↓
Module 2: 3 days (S+P: 0.5 day, A: 0.5 day, R+C: 2 days)
  ↓
Module 3: 3 days (S+P: 1 day, A+R: 1.5 days, C: 0.5 day)
  ↓
Module 5: 3 days (S+P: 0.5 day, A+R: 2 days, C: 0.5 day)
  ↓
Module 6: 2 days (S+P+A: 0.5 day, R+C: 1.5 days)
  ↓
Module 7: 3 days (S+P+A: 1 day, R+C: 1.5 days, validation: 0.5 day)
  ↓
Validation: 2 days (quality gates, documentation, release)
─────────────
Total: 18 days (with 2-day buffer = 20 days / 4 weeks)
```

---

## Parallelization Opportunities

```
Day 1-2:   Module 1 (S+P+A+R+C)
Day 3-5:   Module 2 (S+P+A+R+C)
Day 6-8:   Module 3 (S+P+A+R+C)
Day 9-10:  Module 4 (S+P+A+R+C) ← Can start during Module 3 Refinement
Day 11-13: Module 5 (S+P+A+R+C)
Day 14-15: Module 6 (S+P+A+R+C)
Day 16-18: Module 7 (S+P+A+R+C)
Day 19-20: Final validation + release
```

**Speedup**: Module 4 runs in parallel with Module 3 tail, saving ~1 day

---

## SPARC Phase Time Allocation (Per Module)

| Phase | Typical Duration | % of Module Time | Activities |
|-------|------------------|------------------|------------|
| Specification | 0.25 - 0.5 days | 15-20% | Requirements, FSM, API, edge cases |
| Pseudocode | 0.25 - 0.5 days | 10-15% | Algorithm comments, data structures |
| Architecture | 0.25 - 0.5 days | 15-20% | OTP design, supervision, message flow |
| Refinement | 1.0 - 2.0 days | 40-50% | Chicago TDD implementation, tests |
| Completion | 0.25 - 0.5 days | 10-15% | Quality gates, review, documentation |

**Total per module**: 2-4 days depending on complexity

---

## Quality Gate Schedule

```
Day  Module Phase Gate          Command                           Criteria
─────────────────────────────────────────────────────────────────────────────
2    1      C     Compile+EUnit  rebar3 compile && rebar3 eunit   0 errors, 10/10 tests
5    2      C     Compile+EUnit  rebar3 eunit                     0 errors, 8/8 tests
8    3      C     Compile+EUnit  rebar3 eunit                     0 errors, 5/5 tests
10   4      C     Compile+EUnit  rebar3 eunit                     0 errors, 4/4 tests
13   5      C     Compile+EUnit  rebar3 eunit                     0 errors, 4/4 tests
15   6      C     Compile+CT     rebar3 ct                        3/3 CT tests
18   7      C     Compile+CT     rebar3 ct                        6/6 CT tests
19   All    -     Coverage       rebar3 cover                     ≥80% all modules
19   All    -     Dialyzer       rebar3 dialyzer                  0 warnings
19   All    -     Xref           rebar3 xref                      0 undefined
19   All    -     Format         rebar3 format --verify           Pass
20   All    -     Full Check     make check                       All gates pass
```

---

## Agent Orchestration Timeline

```
Week 1:
┌───────────────────────────────────────────────────────────────┐
│ erlang-otp-developer  : ████████████████████████████████████  │ (Module 1+2, lead)
│ erlang-test-engineer  : ──────────████████████████████████    │ (Tests, starting Day 2)
│ code-reviewer         : ────────────────────────████──────██  │ (Reviews, Day 5, Day 7)
└───────────────────────────────────────────────────────────────┘

Week 2:
┌───────────────────────────────────────────────────────────────┐
│ erlang-researcher     : ████──────────────────────────────    │ (Raft spec, Day 6)
│ erlang-otp-developer  : ────████████████████████████████████  │ (Raft+Router impl)
│ erlang-test-engineer  : ──────────████████████████████████    │ (Tests)
│ code-reviewer         : ────────────────────────────────████  │ (Reviews)
└───────────────────────────────────────────────────────────────┘

Week 3:
┌───────────────────────────────────────────────────────────────┐
│ erlang-architect      : ──────────────────────████────────    │ (Sup tree, Day 14)
│ erlang-otp-developer  : ████████████████████████████████████  │ (Error+Sup impl)
│ erlang-test-engineer  : ──────████████████████████████████    │ (Tests+Integration)
│ verifier              : ────────────────────────────████████  │ (Daily integration)
└───────────────────────────────────────────────────────────────┘

Week 4:
┌───────────────────────────────────────────────────────────────┐
│ plan-designer         : ████──────────────────────────────    │ (API design, Day 16)
│ erlang-otp-developer  : ────████████████──────────────────    │ (API impl)
│ verifier              : ────────────────████████████████████  │ (Quality gates)
│ code-reviewer         : ────────────────────────████────────  │ (Final review)
│ erlang-github-ops     : ────────────────────────────────████  │ (Release, Day 20)
└───────────────────────────────────────────────────────────────┘
```

---

## Risk Heatmap

```
             Low Impact    Medium Impact    High Impact
           │              │                │              │
High Prob  │              │ Raft edge      │              │
           │              │ cases (W2)     │              │
           ├──────────────┼────────────────┼──────────────┤
Medium Prob│              │ Coverage gap   │ Swarm complex│
           │              │ (W4)           │ (W1)         │
           ├──────────────┼────────────────┼──────────────┤
Low Prob   │              │                │ Integration  │
           │              │                │ issues (W3)  │
           └──────────────┴────────────────┴──────────────┘

Mitigation Strategy:
- Raft edge cases: Research phase (erlang-researcher, Day 6)
- Swarm complexity: Add 1-day buffer to Week 1
- Coverage gap: Prioritize testability in Architecture phase
- Integration issues: Daily integration tests from Week 2
```

---

## Success Metrics Progression

```
                Module    Tests     LOC      Coverage
Week 0 (Start): 0/7       0/37      0/780    0%
Week 1 (End):   2/7       18/37     350/780  ≥80% (2 modules)
Week 2 (End):   4/7       27/37     530/780  ≥80% (4 modules)
Week 3 (End):   6/7       34/37     730/780  ≥80% (6 modules)
Week 4 (End):   7/7 ✅    37/37 ✅  780/780 ✅ ≥80% ✅

                Dialyzer  Xref      Format   Release
Week 4 (End):   0 warn ✅ 0 undef ✅ Pass ✅  v0.1.0-alpha ✅
```

---

## Communication Checkpoints

```
Day 1:  Kickoff (Module 1 Specification begins)
Day 2:  Module 1 complete (first milestone)
Day 5:  Week 1 Review (2 modules done)
Day 8:  Module 3 complete (Raft validated)
Day 10: Week 2 Review (4 modules done)
Day 13: Module 5 complete (error recovery working)
Day 15: Week 3 Review (6 modules done, full supervision)
Day 18: Module 7 complete (API ready)
Day 20: Final Review + Release (v0.1.0-alpha)
```

---

## Next Steps (Week 1 Kickoff)

1. **Day 1 Morning (Module 1 Specification)**:
   - Spawn erlang-otp-developer
   - Define agent FSM (idle→assigned→executing→done→error)
   - Document API: `start_link/2`, `assign_task/2`, `get_status/1`, `get_result/1`
   - Identify edge cases: task timeout, retry exhaustion, health check failure

2. **Day 1 Afternoon (Module 1 Pseudocode)**:
   - Sketch task queue algorithm (FIFO, max 100 items)
   - Sketch retry logic (max 3, exponential backoff 100ms→500ms)
   - Sketch health check (10s heartbeat)

3. **Day 2 Morning (Module 1 Architecture)**:
   - Design gen_server structure
   - Define callbacks: `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`
   - Design state record: `{id, status, task, result, stats, health}`

4. **Day 2 Afternoon (Module 1 Refinement)**:
   - Implement gen_server with Chicago TDD
   - Write 10 EUnit tests (spawn, assign, execute, timeout, crash, health, invalid, state transitions, concurrent, result)
   - Run `rebar3 eunit --module=erlmcp_flow_agent_tests`

5. **Day 2 EOD (Module 1 Completion)**:
   - Run quality gates: `rebar3 compile && rebar3 eunit`
   - Code review with code-reviewer agent
   - Document API and usage
   - Mark Module 1 ✅ Complete

---

**Roadmap Status**: Ready to Execute
**Start Date**: Day 1 (TBD)
**End Date**: Day 20 (v0.1.0-alpha release)
**Total Effort**: 18 days + 2-day buffer = 4 weeks

---

**End of Visual Roadmap**
