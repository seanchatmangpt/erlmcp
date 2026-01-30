# SPARC v3 Rewrite - Visual Summary

**erlmcp v0.6.0 → v0.7.0**: Full MCP 2025-11-25 Compliance Journey

---

## Progress Overview (40% Complete)

```
┌─────────────────────────────────────────────────────────────────────┐
│                    SPARC METHODOLOGY PIPELINE                       │
│                    erlmcp v3 Rewrite Project                        │
└─────────────────────────────────────────────────────────────────────┘

  WEEK 1          │   WEEK 2          │   WEEK 3
  Design Phase    │   Implement       │   Test & Release
──────────────────┼───────────────────┼────────────────────────────────
                  │                   │
┌─────────────┐   │                   │
│ Phase 1:    │✅  │                   │
│ SPEC        │   │                   │
│ (DONE)      │   │                   │
│             │   │                   │
│ Requirements│   │                   │
│ API Design  │   │                   │
│ Edge Cases  │   │                   │
└─────────────┘   │                   │
       ↓          │                   │
┌─────────────┐   │                   │
│ Phase 2:    │🔄  │                   │
│ PSEUDOCODE  │   │                   │
│ (40% DONE)  │   │                   │
│             │   │                   │
│ Algorithms  │   │                   │
│ Data Struct │   │                   │
│ Flow Logic  │   │ ← YOU ARE HERE    │
└─────────────┘   │                   │
       ↓          │                   │
┌─────────────┐   │                   │
│ Phase 3:    │✅  │                   │
│ ARCHITECTURE│   │                   │
│ (DONE)      │   │                   │
│             │   │                   │
│ Supervision │   │                   │
│ Modules     │   │                   │
│ Failures    │   │                   │
└─────────────┘   │                   │
       ↓          │                   │
                  │ ┌─────────────┐   │
                  │ │ Phase 4:    │⏳  │
                  │ │ REFINEMENT  │   │
                  │ │ (PENDING)   │   │
                  │ │             │   │
                  │ │ TDD Code    │   │
                  │ │ Tests       │   │
                  │ │ Benchmarks  │   │
                  │ └─────────────┘   │
                  │        ↓          │
                  │                   │ ┌─────────────┐
                  │                   │ │ Phase 5:    │⏳
                  │                   │ │ COMPLETION  │
                  │                   │ │ (PENDING)   │
                  │                   │ │             │
                  │                   │ │ Quality     │
                  │                   │ │ Review      │
                  │                   │ │ Release     │
                  │                   │ └─────────────┘
                  │                   │        ↓
                  │                   │    v0.7.0
                  │                   │  100% Compliant
```

---

## Compliance Progression

```
┌────────────────────────────────────────────────────────────────┐
│                    MCP COMPLIANCE JOURNEY                      │
└────────────────────────────────────────────────────────────────┘

Current (v0.6.0)          Target (v0.7.0)
78% Compliance            100% Compliance
─────────────────────────────────────────────────────

 0%  ░░░░░░░░░░░░░░░░░░░░  100%
     ███████████████▌░░░░░  78% ← Current
     ████████████████████  100% ← Target

┌────────────────────────────────────────────────────────────────┐
│ CAPABILITY STATUS                                              │
├────────────────────────────────────────────────────────────────┤
│ ✅ Resources                  100% (fully compliant)           │
│ ✅ Tools                      100% (fully compliant)           │
│ ✅ Prompts                    100% (fully compliant)           │
│ ✅ Roots                      100% (fully compliant)           │
│ ✅ Logging                    100% (fully compliant)           │
│ ✅ JSON-RPC 2.0               100% (fully compliant)           │
│ ✅ Transport Layer            95%  (minor WebSocket issues)    │
│ ✅ Error Handling             100% (fully compliant)           │
│ ✅ Sampling                   100% (fully compliant)           │
│                                                                │
│ ⚠️  Progress Tokens           60%  (needs _meta extraction)    │
│ ⚠️  Capability Negotiation    40%  (needs version support)     │
│ ⚠️  Pagination                70%  (needs metadata)            │
│ ⚠️  Tasks                     50%  (partially implemented)     │
│                                                                │
│ ❌ Cancellation               0%   (not wired)                 │
│ ❌ Completions API            0%   (not implemented)           │
│ ❌ Elicitation                0%   (not implemented)           │
└────────────────────────────────────────────────────────────────┘

GAP ANALYSIS:
  9 Fully Compliant ✅
  4 Partially Compliant ⚠️  ← Need enhancement
  3 Not Implemented ❌      ← Need creation

WORK REQUIRED:
  3 New Modules (task_manager, completion, elicitation)
  3 Enhanced Modules (cancellation, progress, sampling)
  2 Integration Modules (server, client)
```

---

## 6 Feature Gaps (What We're Building)

```
┌──────────────────────────────────────────────────────────────────┐
│ #1 TASK MANAGEMENT (CRITICAL - NEW MODULE)                      │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_task_manager.erl ⏳ TO CREATE                   │
│ Methods:  tasks/create, tasks/list, tasks/get,                  │
│           tasks/cancel, tasks/result                             │
│ State:    pending → working → completed/failed/cancelled         │
│ Storage:  ETS (ephemeral) or Mnesia (persistent)                │
│ Perf:     <10ms create, <50ms retrieve, 10K+ concurrent          │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│ #2 COMPLETIONS API (HIGH - NEW MODULE)                          │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_completion.erl ⏳ TO CREATE                     │
│ Methods:  completion/complete                                    │
│ Use Case: Resource paths, tool arguments, command names         │
│ Ranking:  Frequency 40%, Recency 30%, Context 30%               │
│ Cache:    ETS with LRU eviction, 5-minute TTL                   │
│ Perf:     <100ms response time (p99)                             │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│ #3 ELICITATION (MEDIUM - NEW MODULE)                            │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_elicitation.erl ⏳ TO CREATE                    │
│ Methods:  elicitation/create,                                    │
│           notifications/elicitation/complete                     │
│ Use Case: OAuth flows, file selection, configuration            │
│ Security: HTTPS validation, rate limiting, audit logging         │
│ Lifecycle: Create → waiting → completed/expired (5-min TTL)     │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│ #4 CANCELLATION (HIGH - ENHANCE EXISTING)                       │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_cancellation.erl ✅ EXISTS (needs wiring)       │
│ Methods:  requests/cancel                                        │
│ Notify:   notifications/cancelled                                │
│ Work:     Wire to erlmcp_server, emit from erlmcp_client         │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│ #5 PROGRESS TOKENS (HIGH - ENHANCE EXISTING)                    │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_progress.erl ✅ EXISTS (needs extraction)       │
│ Work:     Extract _meta.progressToken from request params        │
│           Send notifications during long operations              │
│           Clean up tokens on completion                          │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│ #6 SAMPLING (MEDIUM - ENHANCE EXISTING)                         │
├──────────────────────────────────────────────────────────────────┤
│ Module:   erlmcp_sampling.erl ✅ EXISTS (needs preferences)      │
│ Methods:  sampling/createMessage                                 │
│ Missing:  Model preferences (cost/speed/intelligence)            │
│           Multimodal content support                             │
│           Provider abstraction                                   │
└──────────────────────────────────────────────────────────────────┘
```

---

## Supervision Tree (New Architecture)

```
┌────────────────────────────────────────────────────────────────┐
│          ERLMCP SUPERVISION TREE (v0.7.0)                      │
└────────────────────────────────────────────────────────────────┘

erlmcp_core_sup (one_for_all)
│
├─┬─ erlmcp_server_sup (simple_one_for_one)
│ ├── erlmcp_server (per connection)
│ └── erlmcp_connection_manager
│
├─┬─ erlmcp_task_manager_sup (one_for_one) ⏳ NEW
│ └── erlmcp_task_manager (singleton)
│     │
│     ├─ Task 1 Process ─┐
│     ├─ Task 2 Process  ├─ Spawned Dynamically
│     └─ Task N Process ─┘
│
├─┬─ erlmcp_completion_sup (one_for_one) ⏳ NEW
│ └── erlmcp_completion (singleton)
│     │
│     └─ ETS Cache (read_concurrency)
│
├─┬─ erlmcp_elicitation_sup (one_for_one) ⏳ NEW
│ └── erlmcp_elicitation (singleton)
│     │
│     ├─ Timer Wheel (efficient expiry)
│     └─ Active Elicitations Map
│
├─┬─ erlmcp_cancellation_sup (one_for_one) ✅ EXISTS
│ └── erlmcp_cancellation (singleton)
│     │
│     └─ Cancellation Token Registry
│
├─┬─ erlmcp_progress_sup (one_for_one) ✅ EXISTS
│ └── erlmcp_progress (singleton)
│     │
│     └─ Progress Token Registry
│
└─┬─ erlmcp_sampling_sup (one_for_one) ✅ EXISTS
  └── erlmcp_sampling (singleton)
      │
      ├─ Provider Abstraction
      └─ Model Preferences Validator

ISOLATION STRATEGY:
  ✅ Each supervisor uses one_for_one (failures isolated)
  ✅ No cascading restarts (bulkhead pattern)
  ✅ Observability failures don't affect protocol
```

---

## Data Flow (Task Creation Example)

```
┌────────────────────────────────────────────────────────────────┐
│         TASK CREATION DATA FLOW (NEW IN v0.7.0)                │
└────────────────────────────────────────────────────────────────┘

Client Code                  erlmcp_client            Transport
    │                             │                       │
    │─ create_task("deploy")─────>│                       │
    │                             │                       │
    │                             │─ JSON-RPC request ───>│
    │                             │   {method: "tasks/create"}
    │                             │                       │
                                                          │
                                                          ▼
erlmcp_server          erlmcp_task_manager          Task Process
    │                       │                            │
    │<─ JSON-RPC request ───│                            │
    │                       │                            │
    │─ create_task() ──────>│                            │
    │                       │                            │
    │                       │─ spawn_task() ────────────>│
    │                       │   [execute async]          │
    │                       │                            │
    │<─ {ok, TaskId} ───────│                            │
    │                       │                            │
    │─ JSON-RPC response ───>                            │
    │   {result: {taskId, status}}                       │
                            │                            │
                            │                        ┌───▼────┐
                            │                        │ pending│
                            │                        │   ↓    │
                            │                        │working │
                            │                        │   ↓    │
                            │                        │complete│
                            │                        └────────┘
                            │                            │
                            │<─ progress notifications ──│
                            │<─ task completed ──────────│
                            │                            │
                            │─ notify_client() ──────────>
                                (via erlmcp_server)

LEGEND:
  → Request flow
  ← Response flow
  ⏳ New component
  ✅ Existing component
```

---

## Testing Strategy (65+ Test Cases)

```
┌────────────────────────────────────────────────────────────────┐
│                    TEST PYRAMID (v0.7.0)                       │
└────────────────────────────────────────────────────────────────┘

                    ┌─────────┐
                    │Property │  5+ Property-Based Tests (Proper)
                    │  Tests  │  - Task ID uniqueness
                    │   (5+)  │  - Ranking stability
                    └─────────┘  - State machine invariants
                        ▲
                   ┌────┴────┐
                   │Integration│  15+ End-to-End Tests (CT)
                   │   Tests   │  - Full MCP workflows
                   │   (15+)   │  - Cross-module integration
                   └───────────┘  - Transport integration
                        ▲
                 ┌──────┴──────┐
                 │  Unit Tests  │  45+ Unit Tests (EUnit)
                 │    (45+)     │  - Per-function tests
                 │   EUnit      │  - Edge cases
                 │              │  - Error handling
                 └──────────────┘

TEST COVERAGE TARGETS:
  ✅ Overall: ≥80% (per CLAUDE.md)
  ✅ New Modules: ≥85% (task_manager, completion, elicitation)
  ✅ Enhanced Modules: ≥75% (server, client, json_rpc)
  ✅ Pass Rate: 100% (no failures tolerated)

BENCHMARK SUITES (3 New):
  1. erlmcp_bench_task_manager.erl
     - task_creation_throughput: >1000 tasks/sec
     - task_listing_pagination: <50ms p99
     - concurrent_task_execution: 10K concurrent

  2. erlmcp_bench_completion.erl
     - completion_generation: <100ms p99
     - cache_hit_performance: <5ms p99
     - ranking_algorithm: <50ms for 1000 candidates

  3. erlmcp_bench_elicitation.erl
     - elicitation_creation: <20ms p99
     - timer_wheel_efficiency: 100K timers, <10% overhead
```

---

## Timeline Gantt Chart

```
┌────────────────────────────────────────────────────────────────┐
│                    3-WEEK TIMELINE                             │
└────────────────────────────────────────────────────────────────┘

WEEK 1: DESIGN PHASE (Days 1-7)
Day │ Phase          │ Status │ Deliverable
────┼────────────────┼────────┼─────────────────────────────
 1  │ Specification  │   ✅   │ Requirements + API contracts
 2  │ Specification  │   ✅   │ Edge cases + error codes
────┼────────────────┼────────┼─────────────────────────────
 3  │ Pseudocode     │   🔄   │ Task algorithm design
 4  │ Pseudocode     │   ⏳   │ Completion + elicitation
────┼────────────────┼────────┼─────────────────────────────
 5  │ Architecture   │   ✅   │ Supervision tree
 6  │ Architecture   │   ✅   │ Module decomposition
 7  │ Architecture   │   ✅   │ Failure modes analysis
────┴────────────────┴────────┴─────────────────────────────

WEEK 2: IMPLEMENTATION PHASE (Days 8-14)
Day │ Task                        │ Module               │ Hours
────┼─────────────────────────────┼──────────────────────┼──────
 8  │ Task manager impl (TDD)     │ task_manager.erl     │  4-5
 9  │ Task manager impl (cont.)   │ task_manager.erl     │  4-5
────┼─────────────────────────────┼──────────────────────┼──────
10  │ Completion impl (TDD)       │ completion.erl       │  3-4
11  │ Completion impl (cont.)     │ completion.erl       │  3-4
────┼─────────────────────────────┼──────────────────────┼──────
12  │ Elicitation impl (TDD)      │ elicitation.erl      │  2-3
13  │ Module integration          │ server/client        │  2-3
14  │ Integration testing         │ All modules          │  2-3
────┴─────────────────────────────┴──────────────────────┴──────

WEEK 3: TESTING + RELEASE PHASE (Days 15-21)
Day │ Task                        │ Owner                │ Hours
────┼─────────────────────────────┼──────────────────────┼──────
15  │ Unit test suite             │ test-engineer        │  3-4
16  │ Integration tests           │ test-engineer        │  3-4
────┼─────────────────────────────┼──────────────────────┼──────
17  │ Benchmarks (new suites)     │ performance          │  2-3
18  │ Performance validation      │ performance          │  2-3
────┼─────────────────────────────┼──────────────────────┼──────
19  │ Quality validation          │ code-reviewer        │  1-2
20  │ Code review + PR creation   │ github-ops           │  1-2
21  │ Release v0.7.0              │ github-ops           │  1-2
────┴─────────────────────────────┴──────────────────────┴──────

TOTAL EFFORT: 30-42 hours
REMAINING:    18-25 hours (Phases 2, 4, 5)
PROGRESS:     40% complete
```

---

## Quality Gates Dashboard

```
┌────────────────────────────────────────────────────────────────┐
│              QUALITY GATES (Pre-Release Checklist)             │
└────────────────────────────────────────────────────────────────┘

GATE 1: COMPILATION
  Command: TERM=dumb rebar3 compile
  ⏳ Status: PENDING
  🎯 Target: 0 errors, 0 warnings
  📊 Current: Not yet run

GATE 2: UNIT TESTS
  Command: rebar3 eunit
  ⏳ Status: PENDING
  🎯 Target: 100% pass rate
  📊 Current: Not yet run
  📈 Coverage: ≥80% required

GATE 3: INTEGRATION TESTS
  Command: rebar3 ct
  ⏳ Status: PENDING
  🎯 Target: 15+ scenarios pass
  📊 Current: 0/15

GATE 4: PROPERTY TESTS
  Command: rebar3 proper
  ⏳ Status: PENDING
  🎯 Target: 5+ properties verified
  📊 Current: 0/5

GATE 5: DIALYZER
  Command: rebar3 dialyzer
  ⏳ Status: PENDING
  🎯 Target: 0 warnings
  📊 Current: Not yet run

GATE 6: XREF
  Command: rebar3 xref
  ⏳ Status: PENDING
  🎯 Target: 0 undefined calls
  📊 Current: Not yet run

GATE 7: BENCHMARKS
  Command: make benchmark-quick
  ⏳ Status: PENDING
  🎯 Target: <10% regression
  📊 Current: Baseline: 2.69M ops/sec

GATE 8: CODE REVIEW
  Process: GitHub PR review
  ⏳ Status: PENDING
  🎯 Target: 2+ approvals
  📊 Current: 0/2

─────────────────────────────────────────────────────────────────
OVERALL STATUS: ⏳ PENDING (0/8 gates passed)
BLOCKING ISSUE: Implementation not started (Phase 4 pending)
NEXT GATE:      Complete Phase 2 (Pseudocode)
```

---

## Key Metrics Tracking

```
┌────────────────────────────────────────────────────────────────┐
│                    PROJECT METRICS DASHBOARD                   │
└────────────────────────────────────────────────────────────────┘

COMPLIANCE METRICS
┌──────────────────────┬─────────┬─────────┬──────────┐
│ Metric               │ Current │ Target  │ Progress │
├──────────────────────┼─────────┼─────────┼──────────┤
│ MCP Compliance       │   78%   │  100%   │ ████▌░░░ │
│ Capabilities         │  15/19  │  19/19  │ ████▌░░░ │
│ New Modules          │   0/3   │   3/3   │ ░░░░░░░░ │
│ Enhanced Modules     │   0/3   │   3/3   │ ░░░░░░░░ │
└──────────────────────┴─────────┴─────────┴──────────┘

QUALITY METRICS
┌──────────────────────┬─────────┬─────────┬──────────┐
│ Metric               │ Current │ Target  │ Progress │
├──────────────────────┼─────────┼─────────┼──────────┤
│ Test Coverage        │   72%   │   80%   │ ████▌░░░ │
│ Unit Tests           │  120+   │  165+   │ ████▌░░░ │
│ Integration Tests    │   30+   │   45+   │ ███▌░░░░ │
│ Dialyzer Warnings    │    5    │    0    │ ████▌░░░ │
│ Xref Issues          │    3    │    0    │ ████▌░░░ │
└──────────────────────┴─────────┴─────────┴──────────┘

PERFORMANCE METRICS
┌──────────────────────┬─────────┬─────────┬──────────┐
│ Metric               │ Current │ Target  │ Progress │
├──────────────────────┼─────────┼─────────┼──────────┤
│ Task Create (p99)    │   N/A   │  <10ms  │ ⏳       │
│ Completion (p99)     │   N/A   │ <100ms  │ ⏳       │
│ Elicitation (p99)    │   N/A   │  <20ms  │ ⏳       │
│ Baseline Regression  │   N/A   │  <10%   │ ⏳       │
└──────────────────────┴─────────┴─────────┴──────────┘

DELIVERY METRICS
┌──────────────────────┬─────────┬─────────┬──────────┐
│ Metric               │ Current │ Target  │ Progress │
├──────────────────────┼─────────┼─────────┼──────────┤
│ SPARC Phases         │   2/5   │   5/5   │ ██░░░░░░ │
│ Timeline Progress    │  Week 1 │ Week 3  │ ██░░░░░░ │
│ Effort Spent         │ 12-17h  │ 30-42h  │ ██░░░░░░ │
│ Remaining Effort     │ 18-25h  │   0h    │ ██░░░░░░ │
└──────────────────────┴─────────┴─────────┴──────────┘
```

---

## Next Action Checklist

```
┌────────────────────────────────────────────────────────────────┐
│                  IMMEDIATE NEXT STEPS                          │
└────────────────────────────────────────────────────────────────┘

CURRENT FOCUS: Complete Phase 2 (Pseudocode)
OWNER: plan-designer agent
ESTIMATED TIME: 2-4 hours

☐ Task 1: Complete Task Management Algorithm (1 hour)
  ☐ Document task creation with ID generation
  ☐ Define state machine transitions (pending→working→complete)
  ☐ Specify concurrent limit checking
  ☐ Design pagination cursor logic
  ☐ Handle cancellation during execution

☐ Task 2: Design Completion Ranking Algorithm (1 hour)
  ☐ Define fuzzy matching with Levenshtein distance
  ☐ Specify ranking weights (frequency, recency, context)
  ☐ Design LRU cache eviction strategy
  ☐ Document result batching logic

☐ Task 3: Document Elicitation Lifecycle (1 hour)
  ☐ Define URL generation with secure token
  ☐ Specify timer wheel for expiry management
  ☐ Design HTTPS validation rules
  ☐ Document rate limiting strategy
  ☐ Specify notification flow

☐ Task 4: Review and Validation (30 minutes)
  ☐ Ensure all algorithms have clear pseudocode
  ☐ Validate data structures are well-defined
  ☐ Check edge cases are documented
  ☐ Verify integration points are clear

DELIVERABLE: Complete /home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md

COMMAND TO START:
  vim /home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md

OR DELEGATE:
  /swarm spawn plan-designer "Complete Phase 2 pseudocode per SPARC_V3_ROADMAP.md"
```

---

## File Navigation Map

```
PROJECT ROOT: /home/user/erlmcp/

ROADMAP FILES (Start Here)
├── SPARC_V3_ROADMAP.md             ← Master roadmap (this project)
├── SPARC_V3_QUICK_START.md         ← Quick start guide
└── SPARC_V3_VISUAL_SUMMARY.md      ← This file

PHASE DOCUMENTS
├── docs/
│   ├── MCP_2025-11-25_SPECIFICATION_GAPS.md      ✅ Phase 1 (Done)
│   ├── MCP_2025-11-25_PSEUDOCODE_DESIGN.md       🔄 Phase 2 (40%)
│   ├── MCP_2025-11-25_ARCHITECTURE_DESIGN.md     ✅ Phase 3 (Done)
│   └── MCP_COMPLIANCE_ROADMAP.md                 ✅ Reference

SOURCE FILES (To Be Created in Phase 4)
├── src/
│   ├── erlmcp_task_manager.erl     ⏳ New module
│   ├── erlmcp_completion.erl       ⏳ New module
│   ├── erlmcp_elicitation.erl      ⏳ New module
│   ├── erlmcp_server.erl           ✅ Enhance (wire handlers)
│   ├── erlmcp_client.erl           ✅ Enhance (add API)
│   └── erlmcp_json_rpc.erl         ✅ Enhance (add encoders)

TEST FILES (To Be Created in Phase 4)
├── test/
│   ├── erlmcp_task_manager_tests.erl      ⏳ Unit tests
│   ├── erlmcp_completion_tests.erl        ⏳ Unit tests
│   ├── erlmcp_elicitation_tests.erl       ⏳ Unit tests
│   └── erlmcp_mcp_2025_SUITE.erl          ⏳ Integration

BENCHMARK FILES (To Be Created in Phase 4)
└── bench/
    ├── erlmcp_bench_task_manager.erl      ⏳ Benchmarks
    ├── erlmcp_bench_completion.erl        ⏳ Benchmarks
    └── erlmcp_bench_elicitation.erl       ⏳ Benchmarks
```

---

**STATUS**: Ready to proceed with Phase 2 (Pseudocode completion)

**NEXT STEP**: Complete task management, completion, and elicitation algorithms in pseudocode

**OWNER**: plan-designer agent (or you can complete manually)

**TIME ESTIMATE**: 2-4 hours

**COMMAND**:
```bash
vim /home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md
```

**OR DELEGATE**:
```bash
/swarm spawn plan-designer "Complete Phase 2 pseudocode design per SPARC_V3_ROADMAP.md"
```
