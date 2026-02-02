# erlmcp-flow SPARC Workflow Architecture v1.0.0

**Document Version**: 1.0.0
**Status**: Architecture Phase
**Date**: 2026-02-02
**Repository**: /home/user/erlmcp

---

## TABLE OF CONTENTS

1. [System Architecture](#1-system-architecture)
2. [State Machine Diagrams](#2-state-machine-diagrams)
3. [Supervision Trees](#3-supervision-trees)
4. [Module Design](#4-module-design)
5. [Data Flow](#5-data-flow)
6. [Consensus Protocols](#6-consensus-protocols)
7. [Error Recovery](#7-error-recovery)
8. [Performance Optimization](#8-performance-optimization)

---

## 1. SYSTEM ARCHITECTURE

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      USER INPUT                              │
│  "erlang-otp-developer, erlang-test-engineer parallel 2"   │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│           erlmcp_flow_sparc_orchestrator                     │
│                 (Main Coordinator)                           │
│                                                              │
│  States: idle → specification → pseudocode →                │
│          architecture → refinement → completion → idle      │
└────┬───────┬─────────┬───────────┬──────────┬──────────────┘
     │       │         │           │          │
     ▼       ▼         ▼           ▼          ▼
┌─────┐ ┌────────┐ ┌──────┐ ┌─────────┐ ┌─────────┐
│Spec │ │Planner │ │Exec  │ │Monitor  │ │Receipt  │
│Parse│ │        │ │      │ │         │ │         │
└──┬──┘ └───┬────┘ └──┬───┘ └────┬────┘ └────┬────┘
   │        │         │          │           │
   │        │         │          │           │
   │        │         │          │           │
   ▼        ▼         ▼          ▼           ▼
┌─────────────────────────────────────────────────────┐
│         erlmcp_flow_error_recovery                   │
│  (Handles: task_failed, byzantine, partition)      │
└─────────────────────────────────────────────────────┘
```

### 1.2 Component Interaction

```
┌──────────────┐
│ Orchestrator │
└──────┬───────┘
       │ 1. start_workflow(input)
       ▼
┌──────────────┐
│ Spec Parser  │  ──────┐
└──────┬───────┘        │ 2. {spec_valid, Spec}
       │                │
       │                ▼
       │         ┌──────────────┐
       │         │ Orchestrator │
       │         └──────┬───────┘
       │                │ 3. {plan, Spec}
       │                ▼
       │         ┌──────────────┐
       │         │   Planner    │  ──────┐
       │         └──────┬───────┘        │ 4. {plan_complete, Plan}
       │                │                │
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │ Orchestrator │
       │                │         └──────┬───────┘
       │                │                │ 5. {execute, Plan}
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │   Executor   │
       │                │         └──────┬───────┘
       │                │                │ 6. Task dispatch
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │ Agent Pool   │
       │                │         └──────┬───────┘
       │                │                │ 7. Task results
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │   Monitor    │
       │                │         └──────┬───────┘
       │                │                │ 8. Metrics & alerts
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │ Orchestrator │
       │                │         └──────┬───────┘
       │                │                │ 9. {generate_receipt, Results}
       │                │                ▼
       │                │         ┌──────────────┐
       │                │         │   Receipt    │
       │                │         └──────────────┘
       │                │
       └────────────────┴─── Error Recovery ───────────────────┘
```

---

## 2. STATE MACHINE DIAGRAMS

### 2.1 Orchestrator State Machine

```
                          ┌─────────────┐
                          │    IDLE     │
                          └──────┬──────┘
                                 │ start_workflow(input)
                                 │
                                 ▼
                   ┌─────────────────────────┐
                   │    SPECIFICATION        │
                   │  • Call spec_parser     │
                   │  • Validate agents      │
                   │  • Extract capabilities │
                   └──────┬────────┬─────────┘
                          │        │
                 spec_valid│        │spec_invalid
                          │        │
                          ▼        ▼
               ┌─────────────┐  ┌───────┐
               │ PSEUDOCODE  │  │ ERROR │
               │ • Call      │  └───┬───┘
               │   planner   │      │
               │ • Assign    │      │replan
               │   agents    │      │
               └──────┬──────┘      │
                      │             │
           plan_complete│            │
                      │             │
                      ▼             │
           ┌──────────────────┐    │
           │  ARCHITECTURE    │    │
           │  • Topology      │    │
           │  • Consensus     │    │
           │  • Routing       │    │
           └──────┬───────────┘    │
                  │                │
      design_complete│             │
                  │                │
                  ▼                │
           ┌──────────────────┐   │
           │   REFINEMENT     │   │
           │   • Execute      │   │
           │   • Monitor      │   │
           │   • Adaptive     │   │
           └──────┬───────────┘   │
                  │                │
   execution_complete│             │
                  │                │
                  ▼                │
           ┌──────────────────┐   │
           │   COMPLETION     │   │
           │   • Receipt      │   │
           │   • Audit log    │   │
           │   • Metrics      │   │
           └──────┬───────────┘   │
                  │                │
      receipt_generated│           │
                  │                │
                  ▼                │
           ┌─────────────┐         │
           │    IDLE     │◄────────┘
           └─────────────┘
```

### 2.2 Spec Parser State Machine

```
         ┌─────────────┐
         │    IDLE     │
         └──────┬──────┘
                │ {parse, Input}
                │
                ▼
         ┌─────────────┐
         │   PARSING   │
         │  • Tokenize │
         │  • Extract  │
         │    agents   │
         │  • Extract  │
         │    params   │
         └──────┬──────┘
                │
                │ tokens_extracted
                │
                ▼
         ┌─────────────┐
         │ VALIDATING  │
         │  • Check    │
         │    agents   │
         │    exist    │
         │  • Validate │
         │    params   │
         └──────┬──────┘
                │
         ┌──────┴────────┐
         │               │
  validation_ok│         │validation_failed
         │               │
         ▼               ▼
  ┌───────────┐    ┌─────────┐
  │ COMPLETE  │    │  ERROR  │
  └───────────┘    └─────────┘
```

### 2.3 Planner State Machine

```
         ┌─────────────┐
         │    IDLE     │
         └──────┬──────┘
                │ {plan, Spec}
                │
                ▼
         ┌─────────────┐
         │  ANALYZING  │
         │  • Build    │
         │    dep DAG  │
         │  • Query    │
         │    agents   │
         │  • Estimate │
         │    cost     │
         └──────┬──────┘
                │
                │ analysis_complete
                │
                ▼
         ┌─────────────┐
         │  ASSIGNING  │
         │  • Map      │
         │    tasks    │
         │  • Load     │
         │    balance  │
         │  • Priority │
         └──────┬──────┘
                │
                │ assignments_complete
                │
                ▼
         ┌─────────────┐
         │   ROUTING   │
         │  • Build    │
         │    routes   │
         │  • Select   │
         │    topology │
         │  • Select   │
         │    consensus│
         └──────┬──────┘
                │
         ┌──────┴────────┐
         │               │
  routing_ok│            │routing_failed
         │               │
         ▼               ▼
  ┌───────────┐    ┌─────────┐
  │ COMPLETE  │    │  ERROR  │
  └───────────┘    └─────────┘
```

### 2.4 Executor State Machine

```
         ┌─────────────┐
         │    IDLE     │
         └──────┬──────┘
                │ {execute, Plan}
                │
                ▼
         ┌─────────────┐
         │ DISPATCHING │
         │  • Send     │
         │    tasks    │
         │  • Monitor  │
         │    setup    │
         │  • Set      │
         │    timeouts │
         └──────┬──────┘
                │
                │ all_dispatched
                │
                ▼
         ┌─────────────┐
         │  EXECUTING  │
         │  • Track    │
         │    progress │
         │  • Collect  │
         │    results  │
         │  • Handle   │
         │    timeouts │
         └──────┬──────┘
                │
                │ execution_in_progress
                │
                ▼
         ┌─────────────┐
         │ MONITORING  │
         │  • Collect  │
         │    metrics  │
         │  • Adjust   │
         │    routes   │
         │  • Detect   │
         │    failures │
         └──────┬──────┘
                │
         ┌──────┴────────┐
         │               │
  all_complete│          │any_failed
         │               │
         ▼               ▼
  ┌───────────┐    ┌─────────┐
  │ COMPLETE  │    │  ERROR  │
  └───────────┘    └─────────┘
```

### 2.5 Monitor State Machine

```
         ┌─────────────┐
         │  OBSERVING  │◄─┐
         │  • Collect  │  │
         │    metrics  │  │
         │  • Watch    │  │
         │    alerts   │  │
         └──────┬──────┘  │
                │         │
                │ alert_triggered
                │         │
                ▼         │
         ┌─────────────┐  │
         │  ALERTING   │  │
         │  • Eval     │  │
         │    rules    │  │
         │  • Notify   │  │
         │  • Log      │  │
         └──────┬──────┘  │
                │         │
                │ recovery_needed
                │         │
                ▼         │
         ┌─────────────┐  │
         │ RECOVERING  │  │
         │  • Trigger  │  │
         │    recovery │  │
         │  • Wait for │  │
         │    result   │  │
         └──────┬──────┘  │
                │         │
         ┌──────┴──────┐  │
         │             │  │
  recovery_ok│         │recovery_failed
         │             │  │
         ▼             ▼  │
  ┌───────────┐    ┌──────────┐
  │ COMPLETE  │    │   ALERT  │──┘
  └───────────┘    │ PERSISTS │
                   └──────────┘
```

### 2.6 Error Recovery State Machine

```
         ┌─────────────┐
         │ MONITORING  │◄─┐
         │  • Watch    │  │
         │    for      │  │
         │    errors   │  │
         └──────┬──────┘  │
                │         │
         ┌──────┴────┬────┴─────┬──────────┐
         │           │          │          │
  task_failure│  byzantine│  partition│   │
         │           │          │          │
         ▼           ▼          ▼          │
  ┌───────────┐ ┌──────────┐ ┌──────────┐ │
  │   TASK    │ │BYZANTINE │ │PARTITION │ │
  │  FAILED   │ │ DETECTED │ │ DETECTED │ │
  └─────┬─────┘ └────┬─────┘ └────┬─────┘ │
        │            │             │       │
        │replan      │switch_      │wait_  │
        │            │consensus    │heal   │
        │            │             │       │
        └────────────┴─────────────┴───────┘
                     │
                     ▼
              ┌─────────────┐
              │ RECOVERING  │
              │  • Execute  │
              │    strategy │
              │  • Retry    │
              │    logic    │
              └──────┬──────┘
                     │
              ┌──────┴────────┐
              │               │
    recovery_ok│              │recovery_failed
              │               │
              ▼               ▼
       ┌───────────┐    ┌─────────┐
       │ RECOVERED │    │ FAILED  │
       └───────────┘    └─────────┘
```

---

## 3. SUPERVISION TREES

### 3.1 Top-Level Supervision Tree

```erlang
%% erlmcp_flow_sup.erl
-module(erlmcp_flow_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,  % Consistent state after crash
        intensity => 5,           % Max 5 restarts
        period => 60              % Within 60 seconds
    },

    Children = [
        %% Registry (gproc for O(log N) lookups)
        #{id => erlmcp_flow_registry,
          start => {erlmcp_flow_registry, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker},

        %% SPARC component supervisors
        #{id => erlmcp_flow_sparc_sup,
          start => {erlmcp_flow_sparc_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,  % Wait for all children
          type => supervisor},

        %% Receipt server (singleton)
        #{id => erlmcp_flow_receipt_server,
          start => {erlmcp_flow_receipt, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker},

        %% Error recovery supervisor
        #{id => erlmcp_flow_error_recovery_sup,
          start => {erlmcp_flow_error_recovery_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor}
    ],

    {ok, {SupFlags, Children}}.
```

### 3.2 SPARC Components Supervision Tree

```erlang
%% erlmcp_flow_sparc_sup.erl
-module(erlmcp_flow_sparc_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent children
        intensity => 10,
        period => 60
    },

    Children = [
        %% Orchestrator supervisor (simple_one_for_one)
        #{id => erlmcp_flow_sparc_orchestrator_sup,
          start => {erlmcp_flow_sparc_orchestrator_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor},

        %% Spec parser supervisor (simple_one_for_one)
        #{id => erlmcp_flow_spec_parser_sup,
          start => {erlmcp_flow_spec_parser_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor},

        %% Planner supervisor (simple_one_for_one)
        #{id => erlmcp_flow_planner_sup,
          start => {erlmcp_flow_planner_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor},

        %% Executor supervisor (simple_one_for_one)
        #{id => erlmcp_flow_executor_sup,
          start => {erlmcp_flow_executor_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor},

        %% Monitor supervisor (simple_one_for_one)
        #{id => erlmcp_flow_monitor_sup,
          start => {erlmcp_flow_monitor_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor}
    ],

    {ok, {SupFlags, Children}}.
```

### 3.3 Dynamic Child Supervision

```erlang
%% erlmcp_flow_sparc_orchestrator_sup.erl
-module(erlmcp_flow_sparc_orchestrator_sup).
-behaviour(supervisor).

-export([start_link/0, start_orchestrator/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_orchestrator(WorkflowId) ->
    supervisor:start_child(?MODULE, [WorkflowId]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 3,
        period => 60
    },

    ChildSpec = #{
        id => erlmcp_flow_sparc_orchestrator,
        start => {erlmcp_flow_sparc_orchestrator, start_link, []},
        restart => transient,  % Only restart on abnormal termination
        shutdown => 5000,
        type => worker
    },

    {ok, {SupFlags, [ChildSpec]}}.
```

---

## 4. MODULE DESIGN

### 4.1 Core Modules

```erlang
%% Module hierarchy
erlmcp_flow/
├── erlmcp_flow.erl                      % Public API
├── erlmcp_flow_app.erl                  % Application callback
├── erlmcp_flow_sup.erl                  % Top-level supervisor
│
├── erlmcp_flow_sparc_orchestrator.erl   % Main coordinator (gen_statem)
├── erlmcp_flow_spec_parser.erl          % Input parser (gen_statem)
├── erlmcp_flow_planner.erl              % Task planner (gen_statem)
├── erlmcp_flow_executor.erl             % Task executor (gen_statem)
├── erlmcp_flow_monitor.erl              % Monitoring (gen_statem)
├── erlmcp_flow_receipt.erl              % Receipt server (gen_server)
├── erlmcp_flow_error_recovery.erl       % Error recovery (gen_statem)
│
├── erlmcp_flow_registry.erl             % Agent registry (gen_server + gproc)
├── erlmcp_flow_router.erl               % Routing logic (pure)
├── erlmcp_flow_topology.erl             % Topology builder (pure)
├── erlmcp_flow_consensus.erl            % Consensus interface (behavior)
├── erlmcp_flow_raft.erl                 % Raft implementation
├── erlmcp_flow_pbft.erl                 % PBFT implementation
│
└── erlmcp_flow_sparc_sup.erl            % SPARC components supervisor
    ├── erlmcp_flow_sparc_orchestrator_sup.erl
    ├── erlmcp_flow_spec_parser_sup.erl
    ├── erlmcp_flow_planner_sup.erl
    ├── erlmcp_flow_executor_sup.erl
    └── erlmcp_flow_monitor_sup.erl
```

### 4.2 Module Responsibilities

| Module | Responsibility | Type |
|--------|----------------|------|
| `erlmcp_flow` | Public API facade | Module |
| `erlmcp_flow_sparc_orchestrator` | Coordinate 5 SPARC phases | gen_statem |
| `erlmcp_flow_spec_parser` | Parse & validate input | gen_statem |
| `erlmcp_flow_planner` | Assign agents, build DAG | gen_statem |
| `erlmcp_flow_executor` | Dispatch & monitor tasks | gen_statem |
| `erlmcp_flow_monitor` | Collect metrics, trigger alerts | gen_statem |
| `erlmcp_flow_receipt` | Generate receipts, audit log | gen_server |
| `erlmcp_flow_error_recovery` | Handle failures | gen_statem |
| `erlmcp_flow_registry` | Agent lookup (gproc) | gen_server |
| `erlmcp_flow_router` | Routing algorithms | Pure |
| `erlmcp_flow_topology` | Topology selection | Pure |
| `erlmcp_flow_consensus` | Consensus behavior | Behavior |
| `erlmcp_flow_raft` | Raft protocol | gen_statem |
| `erlmcp_flow_pbft` | PBFT protocol | gen_statem |

---

## 5. DATA FLOW

### 5.1 Request Flow

```
User Input
    │
    ├─> "erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s"
    │
    ▼
┌────────────────────────────────────────────────────────┐
│  erlmcp_flow:start_workflow(Input)                     │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: idle → specification                    │
│  • Generate WorkflowId (UUID)                          │
│  • Store input                                         │
│  • Transition to specification phase                   │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  SpecParser: idle → parsing → validating → complete   │
│  • Tokenize input                                      │
│  • Extract: agents=[erlang-otp-developer,              │
│              erlang-test-engineer]                     │
│  • Extract: parallelism=2                              │
│  • Extract: timeout_ms=300000                          │
│  • Validate agents exist in registry                   │
│  • Return spec_result                                  │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: specification → pseudocode              │
│  • Receive spec_result                                 │
│  • Transition to pseudocode phase                      │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Planner: idle → analyzing → assigning → routing       │
│  • Query agent pool availability                       │
│  • Build dependency DAG                                │
│  • Assign tasks:                                       │
│    - task_1 → erlang-otp-developer (pid1)             │
│    - task_2 → erlang-test-engineer (pid2)             │
│  • Select topology: mesh (only 2 agents)               │
│  • Select consensus: raft (internal agents)            │
│  • Build routing table                                 │
│  • Return plan_result                                  │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: pseudocode → architecture               │
│  • Receive plan_result                                 │
│  • Transition to architecture phase                    │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Topology & Consensus: Architecture Design             │
│  • Build mesh topology: pid1 ↔ pid2                   │
│  • Initialize Raft: leader=pid1, follower=pid2         │
│  • Return design_result                                │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: architecture → refinement               │
│  • Receive design_result                               │
│  • Transition to refinement phase                      │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Executor: idle → dispatching → executing              │
│  • Dispatch task_1 to pid1                             │
│  • Dispatch task_2 to pid2                             │
│  • Set timeouts (300s each)                            │
│  • Monitor progress                                    │
│  • Collect results                                     │
│  • Return execution_result                             │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Monitor: observing → complete                         │
│  • Collect latency, throughput, failure_rate           │
│  • No alerts triggered                                 │
│  • Return metrics                                      │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: refinement → completion                 │
│  • Receive execution_result + metrics                  │
│  • Transition to completion phase                      │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Receipt: Generate audit log                           │
│  • Aggregate all phase results                         │
│  • Calculate total metrics                             │
│  • Sign receipt with HMAC-SHA256                       │
│  • Store in ETS                                        │
│  • Return receipt_result                               │
└────────────────┬───────────────────────────────────────┘
                 │
                 ▼
┌────────────────────────────────────────────────────────┐
│  Orchestrator: completion → idle                       │
│  • Return {ok, WorkflowId} to user                     │
│  • Transition to idle (ready for next workflow)        │
└────────────────────────────────────────────────────────┘
```

---

## 6. CONSENSUS PROTOCOLS

### 6.1 Raft Consensus

**Use Case**: Internal agents, trusted environment, low latency required

```erlang
-module(erlmcp_flow_raft).
-behaviour(gen_statem).
-behaviour(erlmcp_flow_consensus).

-export([start_link/1, append_entry/2, read_log/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% States: follower | candidate | leader

-record(raft_data, {
    current_term = 0 :: non_neg_integer(),
    voted_for :: pid() | undefined,
    log = [] :: [log_entry()],
    commit_index = 0 :: non_neg_integer(),
    last_applied = 0 :: non_neg_integer(),

    %% Leader-specific
    next_index :: #{pid() => non_neg_integer()},
    match_index :: #{pid() => non_neg_integer()},

    %% Timeouts
    election_timeout_ms :: 150..300,  % Randomized
    heartbeat_interval_ms = 50 :: pos_integer(),

    %% Peers
    peers :: [pid()],
    votes_received :: [pid()]
}).

%% Raft guarantees:
%% - Election safety: At most one leader per term
%% - Leader append-only: Leader never overwrites/deletes entries
%% - Log matching: Identical index+term → identical logs
%% - Leader completeness: Committed entry in leader's log
%% - State machine safety: Apply same command at same index
```

### 6.2 PBFT Consensus

**Use Case**: External agents, untrusted environment, Byzantine faults

```erlang
-module(erlmcp_flow_pbft).
-behaviour(gen_statem).
-behaviour(erlmcp_flow_consensus).

-export([start_link/1, request/2, view_change/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% States: pre_prepare | prepare | commit | reply

-record(pbft_data, {
    view = 0 :: non_neg_integer(),
    sequence = 0 :: non_neg_integer(),
    primary :: pid() | undefined,
    replicas :: [pid()],

    %% PBFT requires 3f+1 nodes to tolerate f Byzantine faults
    f :: pos_integer(),  % fault_tolerance

    %% Message logs
    pre_prepares :: #{sequence() => pre_prepare_msg()},
    prepares :: #{sequence() => [prepare_msg()]},
    commits :: #{sequence() => [commit_msg()]},

    %% Certificates
    prepared :: #{sequence() => boolean()},
    committed :: #{sequence() => boolean()},

    %% View change
    view_change_msgs :: [view_change_msg()],
    new_view_msg :: new_view_msg() | undefined,

    %% Checkpoints
    last_checkpoint = 0 :: non_neg_integer(),
    checkpoint_interval = 100 :: pos_integer()
}).

%% PBFT phases:
%% 1. Request: Client → Primary
%% 2. Pre-prepare: Primary → Replicas (sequence assignment)
%% 3. Prepare: Replicas → All (2f+1 prepares = prepared)
%% 4. Commit: Replicas → All (2f+1 commits = committed)
%% 5. Reply: Replicas → Client (f+1 matching replies)
%%
%% View change: Timeout → elect new primary
```

---

## 7. ERROR RECOVERY

### 7.1 Task Failure Recovery

```erlang
%% Task failure detected → replan

handle_event(cast, {task_failed, TaskId, Reason}, monitoring, Data) ->
    %% 1. Mark task as failed
    NewData = mark_task_failed(TaskId, Reason, Data),

    %% 2. Remove failed agent from pool temporarily
    FailedAgent = get_task_agent(TaskId, Data),
    erlmcp_flow_registry:suspend_agent(FailedAgent, 60000),  % 1 minute

    %% 3. Trigger replanning
    {ok, NewPlan} = erlmcp_flow_planner:replan(
        Data#data.plan,
        [{exclude_agent, FailedAgent}]
    ),

    %% 4. Re-dispatch failed task to alternative agent
    AltAgent = maps:get(TaskId, NewPlan#plan.assignments),
    ok = dispatch_task(TaskId, AltAgent, NewData),

    %% 5. Continue monitoring
    {keep_state, NewData#data{plan = NewPlan}}.
```

### 7.2 Byzantine Fault Recovery

```erlang
%% Byzantine behavior detected → switch consensus

handle_event(cast, {byzantine_detected, AgentPid, Evidence}, executing, Data) ->
    %% 1. Verify Byzantine behavior
    case verify_byzantine(AgentPid, Evidence) of
        true ->
            %% 2. Exclude from quorum
            NewPeers = lists:delete(AgentPid, Data#data.peers),

            %% 3. Check if still have quorum
            case length(NewPeers) >= quorum_size(Data) of
                true ->
                    %% 4. Switch from Raft to PBFT for higher tolerance
                    {ok, PBFT} = erlmcp_flow_pbft:start_link(#{
                        replicas => NewPeers,
                        f => calculate_f(length(NewPeers))
                    }),

                    %% 5. Migrate state to PBFT
                    ok = erlmcp_flow_pbft:import_state(PBFT, Data#data.consensus_state),

                    %% 6. Continue with PBFT
                    {keep_state, Data#data{
                        consensus_protocol = pbft,
                        consensus_pid = PBFT,
                        peers = NewPeers
                    }};
                false ->
                    %% Not enough nodes, escalate
                    {next_state, error, Data#data{
                        error_reason = {quorum_loss, length(NewPeers)}
                    }}
            end;
        false ->
            %% False positive, continue
            {keep_state_and_data, []}
    end.
```

### 7.3 Network Partition Recovery

```erlang
%% Partition detected → wait for heal

handle_event(cast, partition_detected, executing, Data) ->
    %% 1. Halt writes (prevent split-brain)
    ok = erlmcp_flow_consensus:halt_writes(Data#data.consensus_pid),

    %% 2. Continue reads from majority partition
    MajorityPartition = detect_majority_partition(Data#data.peers),

    case in_majority_partition(self(), MajorityPartition) of
        true ->
            %% 3. Poll for partition heal (exponential backoff)
            Ref = start_partition_heal_poller(
                #{initial_delay_ms => 1000,
                  max_delay_ms => 60000,
                  backoff_factor => 2.0}
            ),

            {keep_state, Data#data{partition_heal_ref = Ref}};
        false ->
            %% Minority partition, wait passively
            {keep_state, Data#data{in_minority = true}}
    end;

%% Partition healed → reconcile state

handle_event(info, partition_healed, executing, Data) ->
    %% 1. Reconcile state using vector clocks
    {ok, ReconciledState} = reconcile_with_vector_clocks(
        Data#data.consensus_state,
        Data#data.peers
    ),

    %% 2. Resume writes
    ok = erlmcp_flow_consensus:resume_writes(Data#data.consensus_pid),

    %% 3. Continue execution
    {keep_state, Data#data{
        consensus_state = ReconciledState,
        partition_heal_ref = undefined,
        in_minority = false
    }}.
```

---

## 8. PERFORMANCE OPTIMIZATION

### 8.1 Registry Optimization

```erlang
%% Use gproc for O(log N) lookups

-spec lookup_agent(AgentRole :: atom()) -> {ok, pid()} | {error, not_found}.
lookup_agent(AgentRole) ->
    case gproc:lookup_pids({p, l, {agent_role, AgentRole}}) of
        [] -> {error, not_found};
        Pids -> {ok, select_least_loaded(Pids)}
    end.

%% Load balancing: select agent with fewest pending tasks
select_least_loaded(Pids) ->
    LoadScores = [{Pid, get_load_score(Pid)} || Pid <- Pids],
    {BestPid, _Score} = lists:min(fun({_, S1}, {_, S2}) -> S1 =< S2 end, LoadScores),
    BestPid.
```

### 8.2 Adaptive Routing

```erlang
%% Adjust routing based on performance metrics

-spec adjust_routing(Metrics :: map(), RoutingTable :: map()) -> map().
adjust_routing(Metrics, RoutingTable) ->
    %% Calculate health scores for all agents
    HealthScores = maps:map(fun(AgentPid, AgentMetrics) ->
        calculate_health_score(AgentMetrics)
    end, Metrics),

    %% Re-weight routes based on health
    maps:map(fun(TaskId, Route) ->
        AgentPid = maps:get(agent_pid, Route),
        Health = maps:get(AgentPid, HealthScores, 1.0),

        case Health < 0.5 of
            true ->
                %% Health too low, find alternative
                find_alternative_route(TaskId, AgentPid, HealthScores);
            false ->
                %% Keep current route with adjusted weight
                Route#{weight => Health}
        end
    end, RoutingTable).

calculate_health_score(#{latency_p99_ms := Latency,
                         failure_rate := FailureRate,
                         throughput := Throughput}) ->
    LatencyFactor = max(0, 1 - (Latency / 1000)),  % Threshold: 1000ms
    SuccessFactor = 1 - FailureRate,
    ThroughputFactor = min(1, Throughput / 100),  % Target: 100 tasks/s

    0.4 * LatencyFactor + 0.4 * SuccessFactor + 0.2 * ThroughputFactor.
```

### 8.3 Circuit Breaker Integration

```erlang
%% Use circuit breaker per agent to prevent cascading failures

-spec dispatch_task_with_circuit_breaker(TaskId, AgentPid, Data) -> ok | {error, term()}.
dispatch_task_with_circuit_breaker(TaskId, AgentPid, Data) ->
    CircuitBreaker = get_or_create_circuit_breaker(AgentPid),

    erlmcp_circuit_breaker:call(
        CircuitBreaker,
        fun() ->
            dispatch_task(TaskId, AgentPid, Data)
        end,
        fun() ->
            %% Fallback: dispatch to alternative agent
            AltAgent = find_alternative_agent(AgentPid, Data),
            dispatch_task(TaskId, AltAgent, Data)
        end
    ).
```

---

**End of Architecture Document**
