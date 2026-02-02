# erlmcp-flow SPARC Workflow Diagrams

**Version**: 1.0.0
**Date**: 2026-02-02

---

## 1. Main Orchestrator State Machine

```
┌─────────────────────────────────────────────────────────────────┐
│                    SPARC WORKFLOW FSM                            │
│                 (erlmcp_flow_sparc_orchestrator)                 │
└─────────────────────────────────────────────────────────────────┘

                    ┌──────────────┐
         ┌─────────▶│     IDLE     │◀──────────┐
         │          └──────┬───────┘           │
         │                 │                   │
         │                 │ start_workflow    │
         │                 │                   │
         │                 ▼                   │
         │       ┌──────────────────┐          │
         │       │  SPECIFICATION   │          │
         │       │                  │          │
         │       │ Parse & Validate │          │
         │       └─────────┬────────┘          │
         │                 │                   │
         │          spec_valid                 │
         │                 │                   │
         │                 ▼                   │
         │       ┌──────────────────┐          │
         │       │   PSEUDOCODE     │          │
         │       │                  │          │
         │       │  Plan & Assign   │          │
         │       └─────────┬────────┘          │
         │                 │                   │
         │         plan_complete               │
         │                 │                   │
         │                 ▼                   │
         │       ┌──────────────────┐          │
         │       │  ARCHITECTURE    │          │
         │       │                  │          │
         │       │ Topology & Route │          │
         │       └─────────┬────────┘          │
         │                 │                   │
         │        design_complete              │
         │                 │                   │
         │                 ▼                   │
         │       ┌──────────────────┐          │
         │       │   REFINEMENT     │──────┐   │
         │       │                  │      │   │
         │       │ Execute & Monitor│      │   │
         │       └─────────┬────────┘      │   │
         │                 │               │   │
         │      execution_complete         │   │
         │                 │               │   │
         │                 ▼               │   │
         │       ┌──────────────────┐      │   │
         │       │   COMPLETION     │      │   │
         │       │                  │      │   │
         │       │ Receipt & Audit  │      │   │
         │       └─────────┬────────┘      │   │
         │                 │               │   │
         │      receipt_generated          │   │
         │                 │               │   │
         └─────────────────┘               │   │
                                           │   │
            ┌──────────────┐               │   │
            │    ERROR     │◀──────────────┘   │
            │              │                   │
            │ • task_failed│                   │
            │ • byzantine  │                   │
            │ • partition  │                   │
            └──────┬───────┘                   │
                   │                           │
                   │ replan                    │
                   │                           │
                   └───────────────────────────┘

```

---

## 2. Component Interaction Diagram

```
┌─────────────┐
│    USER     │
└──────┬──────┘
       │
       │ "erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s"
       │
       ▼
┌─────────────────────────────────────────────────────────────┐
│  erlmcp_flow:start_workflow(Input)                          │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌───────────────────────────────────────────────────────────────┐
│            erlmcp_flow_sparc_orchestrator                     │
│  State: idle → specification                                  │
└──────┬────────────────────────────────────────────────────────┘
       │
       │ {parse, Input}
       ▼
┌──────────────────────┐
│ erlmcp_flow_spec_    │───┐
│        parser        │   │
│                      │   │
│ States:              │   │
│  idle → parsing →    │   │
│  validating →        │   │
│  complete            │   │ {spec_valid, SpecResult}
└──────────────────────┘   │
                           │
                           ▼
┌───────────────────────────────────────────────────────────────┐
│            erlmcp_flow_sparc_orchestrator                     │
│  State: specification → pseudocode                            │
└──────┬────────────────────────────────────────────────────────┘
       │
       │ {plan, SpecResult}
       ▼
┌──────────────────────┐
│ erlmcp_flow_planner  │───┐
│                      │   │
│ States:              │   │
│  idle → analyzing →  │   │
│  assigning →         │   │
│  routing →           │   │
│  complete            │   │ {plan_complete, PlanResult}
└──────────────────────┘   │
                           │
                           ▼
┌───────────────────────────────────────────────────────────────┐
│            erlmcp_flow_sparc_orchestrator                     │
│  State: pseudocode → architecture → refinement                │
└──────┬────────────────────────────────────────────────────────┘
       │
       │ {execute, PlanResult}
       ▼
┌──────────────────────┐
│ erlmcp_flow_executor │
│                      │
│ States:              │
│  idle → dispatching →│────┐
│  executing →         │    │
│  monitoring →        │    │ Task dispatch
│  complete            │    │
└──────────────────────┘    │
                            │
                            ▼
┌────────────────────────────────────────────────────────┐
│           Agent Pool (via gproc registry)              │
│                                                        │
│  ┌─────────────────┐    ┌─────────────────┐          │
│  │ erlang-otp-     │    │ erlang-test-    │          │
│  │   developer     │    │   engineer      │          │
│  │   (pid1)        │    │   (pid2)        │          │
│  └────────┬────────┘    └────────┬────────┘          │
└───────────│──────────────────────│────────────────────┘
            │                      │
            │ Task results         │
            │                      │
            └──────────┬───────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────┐
│         erlmcp_flow_monitor                              │
│                                                          │
│  • Collect latency, throughput, failure_rate            │
│  • Trigger alerts                                        │
│  • Adjust routing                                        │
└──────────────────────┬───────────────────────────────────┘
                       │
                       │ {metrics, MetricsData}
                       │
                       ▼
┌───────────────────────────────────────────────────────────────┐
│            erlmcp_flow_sparc_orchestrator                     │
│  State: refinement → completion                               │
└──────┬────────────────────────────────────────────────────────┘
       │
       │ {generate_receipt, Results}
       ▼
┌──────────────────────┐
│ erlmcp_flow_receipt  │
│                      │
│ • Aggregate results  │
│ • Calculate metrics  │
│ • Sign with HMAC     │
│ • Store in ETS       │
└──────┬───────────────┘
       │
       │ {ok, Receipt}
       │
       ▼
┌───────────────────────────────────────────────────────────────┐
│            erlmcp_flow_sparc_orchestrator                     │
│  State: completion → idle                                     │
│  Return: {ok, WorkflowId}                                     │
└───────────────────────────────────────────────────────────────┘
```

---

## 3. Error Recovery State Machine

```
┌─────────────────────────────────────────────────────────────┐
│              erlmcp_flow_error_recovery                      │
│                    State Machine                             │
└─────────────────────────────────────────────────────────────┘

                   ┌──────────────┐
        ┌─────────▶│  MONITORING  │◀──────────┐
        │          └──────┬───────┘           │
        │                 │                   │
        │          ┌──────┴──────┬────────┐   │
        │          │             │        │   │
        │   task_failure   byzantine  partition│
        │          │             │        │   │
        │          ▼             ▼        ▼   │
        │   ┌──────────┐  ┌──────────┐ ┌──────────┐
        │   │   TASK   │  │BYZANTINE │ │PARTITION │
        │   │  FAILED  │  │ DETECTED │ │ DETECTED │
        │   └────┬─────┘  └────┬─────┘ └────┬─────┘
        │        │             │            │
        │        │replan       │switch_     │wait_
        │        │             │consensus   │heal
        │        │             │            │
        │        └─────────────┴────────────┘
        │                      │
        │                      ▼
        │             ┌──────────────┐
        │             │  RECOVERING  │
        │             │              │
        │             │ • Execute    │
        │             │   strategy   │
        │             │ • Backoff    │
        │             │   retry      │
        │             └──────┬───────┘
        │                    │
        │             ┌──────┴────────┐
        │             │               │
        │    recovery_ok│             │recovery_failed
        │             │               │
        │             ▼               ▼
        │      ┌───────────┐    ┌─────────┐
        └──────│ RECOVERED │    │ FAILED  │
               └───────────┘    │         │
                                │ Escalate│
                                └─────────┘

Recovery Strategies:

1. TASK_FAILED → REPLAN
   ├─ Suspend failed agent (60s)
   ├─ Query alternative agents
   ├─ Re-assign task
   └─ Resume monitoring

2. BYZANTINE_DETECTED → SWITCH_CONSENSUS
   ├─ Verify Byzantine behavior
   ├─ Exclude from quorum
   ├─ Switch Raft → PBFT
   ├─ Reconfigure consensus
   └─ Resume execution

3. PARTITION_DETECTED → WAIT_HEAL
   ├─ Halt writes (prevent split-brain)
   ├─ Continue reads from majority
   ├─ Poll for heal (exponential backoff)
   ├─ Reconcile with vector clocks
   └─ Resume writes
```

---

## 4. Consensus Protocol Selection

```
┌─────────────────────────────────────────────────────────────┐
│              Consensus Protocol Selector                     │
└─────────────────────────────────────────────────────────────┘

                    User Input
                        │
                        ▼
             ┌──────────────────┐
             │  Assess Risk      │
             │                   │
             │ • Agent trust     │
             │ • Environment     │
             │ • Latency req     │
             └─────────┬─────────┘
                       │
        ┌──────────────┴──────────────┐
        │                             │
   Low Risk                      High Risk
        │                             │
        ▼                             ▼
┌───────────────┐            ┌───────────────┐
│     RAFT      │            │     PBFT      │
└───────────────┘            └───────────────┘

RAFT                          PBFT
────────────────              ─────────────────
• Internal agents             • External agents
• Trusted environment         • Untrusted environment
• 150-300ms latency          • 300-500ms latency
• Simple leader election      • f Byzantine faults
• N nodes tolerate            • 3f+1 nodes required
  (N-1)/2 crash faults       • Pre-prepare → Prepare
• Follower → Candidate        → Commit phases
  → Leader states            • View change on timeout


RAFT State Machine            PBFT State Machine
──────────────────            ──────────────────
     ┌──────────┐                 ┌──────────┐
 ┌──▶│ FOLLOWER │                 │ PRIMARY  │──┐
 │   └────┬─────┘                 └────┬─────┘  │
 │        │timeout                     │        │
 │        │                            │        │
 │        ▼                            ▼        │
 │   ┌───────────┐               ┌──────────┐  │
 │   │ CANDIDATE │               │ REPLICA  │◀─┘
 │   └────┬──────┘               └──────────┘
 │        │majority                    │
 │        │votes                       │timeout
 │        │                            │
 │        ▼                            ▼
 │   ┌────────┐                ┌─────────────┐
 └───│ LEADER │                │ VIEW_CHANGE │
     └────────┘                └─────────────┘
```

---

## 5. Topology Selection

```
┌─────────────────────────────────────────────────────────────┐
│                Topology Selector                             │
└─────────────────────────────────────────────────────────────┘

                 Agent Count
                      │
         ┌────────────┴────────────┐
         │                         │
     ≤ 10 agents              > 10 agents
         │                         │
         ▼                         ▼
    ┌─────────┐              ┌──────────────┐
    │  MESH   │              │ HIERARCHICAL │
    └─────────┘              └──────────────┘


MESH TOPOLOGY                 HIERARCHICAL TOPOLOGY
─────────────                 ─────────────────────

  A1 ─────── A2                     Coordinator
   │ \     / │                          │
   │  \   /  │                    ┌─────┼─────┐
   │   \ /   │                    │     │     │
   │    X    │                   C1    C2    C3
   │   / \   │                   / \   / \   / \
   │  /   \  │                  A1 A2 A3 A4 A5 A6
   │ /     \ │
  A3 ─────── A4

Edges: O(N²)                  Edges: O(N)
Latency: Low                  Latency: Medium
Redundancy: High              Scalability: High
Best for: ≤10 agents          Best for: >10 agents


Example: 4 agents             Example: 12 agents
─────────────────             ──────────────────
Edges = 4*(4-1)/2 = 6         Edges = 11 (tree)
Full connectivity              3 levels deep
Each knows all                 Coordinator routing
```

---

## 6. Adaptive Routing Flow

```
┌─────────────────────────────────────────────────────────────┐
│                Adaptive Routing System                       │
└─────────────────────────────────────────────────────────────┘

    ┌────────────────┐
    │  Task Dispatch │
    └────────┬───────┘
             │
             ▼
    ┌────────────────┐
    │  Agent Executes│
    └────────┬───────┘
             │
             ▼
    ┌────────────────────────────────┐
    │  Collect Metrics               │
    │  • Latency (p99)               │
    │  • Failure rate                │
    │  • Throughput                  │
    └────────┬───────────────────────┘
             │
             ▼
    ┌────────────────────────────────┐
    │  Calculate Health Score        │
    │                                │
    │  health = 0.4 * latency_factor│
    │         + 0.4 * success_factor│
    │         + 0.2 * throughput_f  │
    │                                │
    │  where:                        │
    │  latency_factor = max(0,      │
    │    1 - (p99/threshold))       │
    │  success_factor = 1 - failure │
    │  throughput_factor = min(1,   │
    │    throughput/target)         │
    └────────┬───────────────────────┘
             │
             ▼
    ┌────────────────────────────────┐
    │  Evaluate Health               │
    │                                │
    │  ┌──────────────┬──────────┐  │
    │  │ health ≥ 0.5 │health<0.5│  │
    │  │     OK       │   BAD    │  │
    │  └──────┬───────┴─────┬────┘  │
    │         │             │       │
    │         ▼             ▼       │
    │  ┌──────────┐  ┌──────────┐  │
    │  │  Keep    │  │  Re-route│  │
    │  │  Route   │  │  to Alt  │  │
    │  └──────────┘  └──────────┘  │
    └────────┬───────────────────────┘
             │
             ▼
    ┌────────────────────────────────┐
    │  Update Routing Table          │
    │                                │
    │  RoutingTable[TaskId] = #{     │
    │    agent_pid => NewPid,       │
    │    weight => HealthScore,     │
    │    last_updated => Now        │
    │  }                            │
    └────────┬───────────────────────┘
             │
             ▼
    ┌────────────────┐
    │  Next Task     │
    └────────────────┘
```

---

## 7. Supervision Tree Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    erlmcp_flow_sup                               │
│                   (one_for_all)                                  │
└─────────┬───────────────────────────────────────────────────────┘
          │
          ├─────┬─────────────┬──────────────────┬─────────────────┐
          │     │             │                  │                 │
          ▼     ▼             ▼                  ▼                 ▼
     ┌─────┐ ┌──────┐  ┌──────────┐     ┌───────────┐  ┌──────────┐
     │Reg  │ │SPARC │  │Receipt   │     │Error      │  │Other     │
     │istry│ │Sup   │  │Server    │     │Recovery   │  │Services  │
     └─────┘ └──┬───┘  └──────────┘     │Sup        │  └──────────┘
                │                        └───────────┘
                │ (one_for_one)
                │
    ┌───────────┼───────────┬───────────┬───────────┐
    │           │           │           │           │
    ▼           ▼           ▼           ▼           ▼
┌──────┐  ┌─────────┐  ┌────────┐  ┌────────┐  ┌────────┐
│Orch  │  │Parser   │  │Planner │  │Executor│  │Monitor │
│Sup   │  │Sup      │  │Sup     │  │Sup     │  │Sup     │
└──┬───┘  └────┬────┘  └───┬────┘  └───┬────┘  └───┬────┘
   │           │            │           │           │
   │ (simple_one_for_one)   │           │           │
   │           │            │           │           │
   ▼           ▼            ▼           ▼           ▼
┌────┐     ┌──────┐     ┌──────┐   ┌──────┐   ┌──────┐
│Orch│     │Parser│     │Planner   │Exec  │   │Monitor
│1..N│     │1..N  │     │1..N  │   │1..N  │   │1..N  │
└────┘     └──────┘     └──────┘   └──────┘   └──────┘
(gen_      (gen_        (gen_      (gen_      (gen_
statem)    statem)      statem)    statem)    statem)


Legend:
───────
one_for_all: If any child dies, restart all
one_for_one: If child dies, restart only that child
simple_one_for_one: Dynamic children, all same spec
```

---

## 8. Receipt Chain Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Receipt Chain                             │
│              (Audit Trail & Verification)                    │
└─────────────────────────────────────────────────────────────┘

Workflow                Receipt Components
────────                ──────────────────

Phase 1: Spec           ┌─────────────────┐
  Input parsed       ──▶│ SpecResult      │
  Agents validated      │ • agents        │
                        │ • parallelism   │
                        │ • timeout_ms    │
                        └─────────┬───────┘
Phase 2: Pseudocode              │
  Tasks planned      ──▶┌─────────▼───────┐
  Agents assigned       │ PlanResult      │
                        │ • assignments   │
                        │ • routing_table │
                        └─────────┬───────┘
Phase 3: Architecture            │
  Topology built     ──▶┌─────────▼───────┐
  Consensus init        │ DesignResult    │
                        │ • topology      │
                        │ • consensus     │
                        └─────────┬───────┘
Phase 4: Refinement              │
  Tasks executed     ──▶┌─────────▼───────┐
  Monitoring active     │ ExecutionResult │
                        │ • completed     │
                        │ • failed        │
                        │ • metrics       │
                        └─────────┬───────┘
Phase 5: Completion              │
  Aggregate results  ──▶┌─────────▼───────┐
  Sign receipt          │ Receipt         │
                        │ • workflow_id   │
                        │ • phases        │
                        │ • tasks         │
                        │ • metrics       │
                        │ • signature     │───┐
                        └─────────────────┘   │
                                              │
                                              ▼
                                    ┌──────────────────┐
                                    │ HMAC-SHA256      │
                                    │                  │
                                    │ Verifies:        │
                                    │ • Integrity      │
                                    │ • Authenticity   │
                                    │ • Non-repudiation│
                                    └──────────────────┘
```

---

**End of Diagrams**
