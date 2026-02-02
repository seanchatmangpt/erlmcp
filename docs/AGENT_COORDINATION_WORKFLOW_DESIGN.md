# Agent Coordination Workflow Design for MCP Implementation
**Version:** 1.0.0
**Status:** Design Specification
**Date:** 2026-02-02
**Architecture:** Claude-Flow + SPARC + TCPS + Erlang/OTP

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Agent Roles and Specialization](#agent-roles-and-specialization)
3. [Work Decomposition Strategy](#work-decomposition-strategy)
4. [Dependency Management](#dependency-management)
5. [Communication Patterns](#communication-patterns)
6. [Consensus Mechanisms](#consensus-mechanisms)
7. [Progress Tracking](#progress-tracking)
8. [Escalation and Conflict Resolution](#escalation-and-conflict-resolution)
9. [Coordination Tooling](#coordination-tooling)
10. [Implementation Protocols](#implementation-protocols)
11. [Quality Gates and Governance](#quality-gates-and-governance)
12. [Performance and Scaling](#performance-and-scaling)

---

## Executive Summary

This document defines a comprehensive agent coordination workflow for MCP (Model Context Protocol) implementation in the erlmcp project, leveraging claude-flow principles, SPARC methodology, and Toyota Production System (TPS) concepts adapted for software development.

### Core Principles

1. **Process-per-Agent Isolation**: Each agent operates independently with supervised execution
2. **Message-Based Coordination**: Asynchronous communication via work orders and events
3. **Pull-Based Demand**: Work is pulled from prioritized queues, not pushed
4. **Quality Built-In (Jidoka)**: Automated quality gates with stop-the-line capability
5. **Continuous Improvement (Kaizen)**: Metrics-driven optimization of workflows
6. **Fail-Fast with Recovery**: Let-it-crash philosophy with supervisor-based recovery

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                   Orchestrator Layer                         │
│  (sparc-orchestrator + plan-designer)                       │
└────────────────────┬────────────────────────────────────────┘
                     │
        ┌────────────┼────────────┐
        ↓            ↓            ↓
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ Specification│ │ Pseudocode   │ │ Architecture │
│    Phase     │ │    Phase     │ │    Phase     │
└──────────────┘ └──────────────┘ └──────────────┘
        ↓            ↓            ↓
┌──────────────────────────────────────────────────┐
│           Work Order Distribution Layer          │
│     (Pull queues by priority/bucket/deps)        │
└──────────────┬───────────────────────────────────┘
               │
    ┌──────────┼──────────┐
    ↓          ↓          ↓
┌─────────┐ ┌─────────┐ ┌─────────┐
│ Builder │ │  Test   │ │ Quality │
│  Agents │ │ Agents  │ │  Agents │
└─────────┘ └─────────┘ └─────────┘
    ↓          ↓          ↓
┌──────────────────────────────────────────────────┐
│         Progress Tracking & Reporting            │
│   (Dashboard, Receipts, Metrics, Andon)          │
└──────────────────────────────────────────────────┘
```

---

## Agent Roles and Specialization

### 1. Strategic Layer (Orchestration)

#### 1.1 sparc-orchestrator
**Role**: SPARC methodology coordinator
**Specialization**: Phase sequencing and workflow management
**Responsibilities**:
- Coordinate all 5 SPARC phases (Spec → Pseudocode → Architecture → Refinement → Completion)
- Ensure phase dependencies are satisfied before progression
- Delegate to phase-specific agents
- Monitor phase completion and quality gates

**Input**: Epic-level requirements
**Output**: Phase-decomposed work orders
**Communication**: Broadcasts phase transitions, collects phase completion signals

**Supervision Strategy**: `one_for_one` (failure doesn't affect other orchestrators)

#### 1.2 plan-designer
**Role**: Implementation planning specialist
**Specialization**: Research → Plan → Execute pattern
**Responsibilities**:
- Design implementation approach based on research
- Make architectural decisions
- Create step-by-step implementation plans
- Identify agent delegations

**Input**: Requirements from orchestrator or user
**Output**: Detailed implementation plan with agent assignments
**Communication**: Delegates to erlang-researcher for context, then assigns work

**Supervision Strategy**: `temporary` (only runs during planning phase)

#### 1.3 erlang-researcher
**Role**: Codebase exploration and pattern analysis
**Specialization**: Context gathering and pattern identification
**Responsibilities**:
- Explore existing codebase patterns
- Identify relevant modules and APIs
- Document architectural conventions
- Provide research summaries for planning

**Input**: Research queries from plan-designer
**Output**: Research summary with code examples and patterns
**Communication**: Read-only operations, returns research artifacts

**Supervision Strategy**: `temporary` (spawned per research task)

### 2. Design Layer (Architecture)

#### 2.1 erlang-architect
**Role**: System architecture and OTP design
**Specialization**: Supervision trees, behavior selection, module decomposition
**Responsibilities**:
- Design supervision strategies
- Choose OTP behaviors (gen_server, gen_statem, supervisor)
- Plan module boundaries and APIs
- Review system design for OTP compliance

**Input**: Architecture requirements from plan-designer
**Output**: Architecture document with supervision tree and child specs
**Communication**: Collaborates with erlang-otp-developer for feasibility

**Supervision Strategy**: `temporary` (architecture phase only)

### 3. Implementation Layer (Development)

#### 3.1 erlang-otp-developer
**Role**: OTP implementation specialist
**Specialization**: gen_server, supervisor, OTP behaviors
**Responsibilities**:
- Implement gen_server/gen_statem modules
- Follow OTP best practices (init/1 non-blocking, proper timeouts)
- Implement supervision trees
- Handle process lifecycle

**Input**: Architecture document + implementation plan
**Output**: Working .erl modules with proper OTP behaviors
**Communication**: Coordinates with erlang-test-engineer for TDD cycle

**Supervision Strategy**: `permanent` (long-running development agent)

**WIP Limit**: 1 module at a time (focus and quality)

#### 3.2 erlang-transport-builder
**Role**: Transport layer implementation
**Specialization**: stdio, TCP, HTTP, WebSocket, SSE transports
**Responsibilities**:
- Implement erlmcp_transport behavior callbacks
- Use gun (client) and ranch (server) appropriately
- Handle connection lifecycle and errors
- Implement backpressure and flow control

**Input**: Transport specifications and API contracts
**Output**: Transport module implementing erlmcp_transport behavior
**Communication**: Reports progress to orchestrator

**Supervision Strategy**: `permanent`

**WIP Limit**: 1 transport implementation at a time

#### 3.3 build-engineer
**Role**: Constrained source file writes
**Specialization**: Safe code editing with validation
**Responsibilities**:
- Write/edit .erl and .hrl files
- Validate syntax before writing
- Ensure compilation succeeds
- Coordinate with verifier for validation

**Input**: Code from implementation agents
**Output**: Written source files
**Communication**: Synchronous validation with verifier

**Supervision Strategy**: `permanent`

### 4. Testing Layer (Quality Assurance)

#### 4.1 erlang-test-engineer
**Role**: Test implementation (Chicago TDD)
**Specialization**: EUnit, Common Test, PropEr
**Responsibilities**:
- Write tests BEFORE implementation (Chicago TDD)
- Create EUnit unit tests
- Create Common Test integration tests
- Write PropEr property-based tests
- Ensure ≥80% coverage

**Input**: API contracts and specifications
**Output**: Test suites (.erl files in test/)
**Communication**: Collaborates with erlang-otp-developer in TDD cycle

**Supervision Strategy**: `permanent`

**WIP Limit**: 1 test suite at a time

#### 4.2 agent-06-test-eunit
**Role**: EUnit test execution
**Specialization**: Unit test validation
**Responsibilities**:
- Run `rebar3 eunit --module=M_tests`
- Report pass/fail status
- Collect coverage data
- Trigger Andon on failures

**Input**: Module name
**Output**: Test results + coverage report
**Communication**: Reports to jidoka quality system

**Supervision Strategy**: `transient` (restarts on abnormal exit)

#### 4.3 agent-07-test-ct
**Role**: Common Test execution
**Specialization**: Integration test validation
**Responsibilities**:
- Run `rebar3 ct --suite=test/S`
- Report pass/fail status
- Collect test logs
- Trigger Andon on failures

**Input**: Suite name
**Output**: Test results + logs
**Communication**: Reports to jidoka quality system

**Supervision Strategy**: `transient`

#### 4.4 agent-10-test-proper
**Role**: PropEr property test execution
**Specialization**: Property-based testing
**Responsibilities**:
- Run property-based tests
- Report counterexamples
- Shrink failing cases
- Validate invariants

**Input**: Property specifications
**Output**: Property test results
**Communication**: Reports to jidoka quality system

**Supervision Strategy**: `transient`

### 5. Validation Layer (Quality Gates)

#### 5.1 agent-01-compile-gate
**Role**: Compilation validation
**Specialization**: Entry gate for all code
**Responsibilities**:
- Run `TERM=dumb rebar3 compile`
- Ensure errors = 0
- Stop-the-line on failure
- Trigger Andon alert

**Input**: Source files
**Output**: Compilation status
**Communication**: Blocks all downstream agents until pass

**Supervision Strategy**: `permanent` (critical gate)

**Quality Gate**: BLOCKING (nothing proceeds until compilation succeeds)

#### 5.2 agent-11-coverage
**Role**: Code coverage validation
**Specialization**: 80% coverage enforcement
**Responsibilities**:
- Run coverage analysis
- Report coverage percentages
- Identify uncovered code
- Trigger warning at <80%

**Input**: Test results
**Output**: Coverage report
**Communication**: Reports to dashboard

**Supervision Strategy**: `permanent`

**Quality Gate**: WARNING at <80%, BLOCKING at <70%

#### 5.3 agent-12-dialyzer
**Role**: Type checking validation
**Specialization**: Dialyzer analysis
**Responsibilities**:
- Run `rebar3 dialyzer`
- Report type warnings
- Track warning trends
- Trigger Andon on regressions

**Input**: Compiled modules
**Output**: Dialyzer report
**Communication**: Reports to quality dashboard

**Supervision Strategy**: `permanent`

**Quality Gate**: WARNING (not blocking initially)

#### 5.4 agent-13-xref
**Role**: Cross-reference analysis
**Specialization**: Undefined function detection
**Responsibilities**:
- Run `rebar3 xref`
- Detect undefined function calls
- Detect unused functions
- Report dead code

**Input**: Compiled modules
**Output**: Xref report
**Communication**: Reports to quality dashboard

**Supervision Strategy**: `permanent`

**Quality Gate**: BLOCKING on undefined calls

#### 5.5 agent-14-format
**Role**: Code formatting validation
**Specialization**: Consistent code style
**Responsibilities**:
- Run `rebar3 format --check` (check mode)
- Run `rebar3 format` (fix mode)
- Ensure consistent style
- Auto-fix formatting issues

**Input**: Source files
**Output**: Formatted code
**Communication**: Auto-commits formatting fixes

**Supervision Strategy**: `permanent`

**Quality Gate**: AUTO-FIX (not blocking)

### 6. Performance Layer (Optimization)

#### 6.1 erlang-performance
**Role**: Performance analysis and optimization
**Specialization**: Benchmarking and profiling
**Responsibilities**:
- Write benchmark scenarios
- Run performance tests
- Detect regressions (>10%)
- Profile bottlenecks

**Input**: Implementation code
**Output**: Benchmark results + optimization recommendations
**Communication**: Reports to dashboard

**Supervision Strategy**: `permanent`

**WIP Limit**: 1 benchmark suite at a time

#### 6.2 agent-15-benchmark
**Role**: Benchmark execution
**Specialization**: Performance regression detection
**Responsibilities**:
- Run `make benchmark-quick`
- Compare against baselines
- Report regressions
- Trigger warning on >10% regression

**Input**: Benchmark modules
**Output**: Performance metrics
**Communication**: Reports to dashboard

**Supervision Strategy**: `transient`

**Quality Gate**: WARNING at >10% regression, BLOCKING at >25%

### 7. Review Layer (Code Quality)

#### 7.1 code-reviewer
**Role**: Pre-completion quality validation
**Specialization**: OTP compliance and best practices
**Responsibilities**:
- Review code for OTP patterns
- Check for anti-patterns (blocking init/1, large messages, etc.)
- Validate test coverage
- Approve for completion phase

**Input**: Implementation + tests
**Output**: Review report + approval/rejection
**Communication**: Gates completion phase

**Supervision Strategy**: `permanent`

**Quality Gate**: BLOCKING (must approve before completion)

### 8. Operations Layer (Workflow)

#### 8.1 erlang-github-ops
**Role**: Git and PR operations
**Specialization**: Release workflow (MERGE ONLY - NEVER REBASE)
**Responsibilities**:
- Create branches
- Commit changes (with receipt URLs)
- Create pull requests
- Merge (NEVER rebase, NEVER --no-verify)
- Run CI/CD workflows

**Input**: Completed work orders
**Output**: Merged PRs, release artifacts
**Communication**: Coordinates with code-reviewer

**Supervision Strategy**: `permanent`

**Critical Rules**:
- NEVER REBASE EVER (merge only)
- NEVER use --no-verify (quality gates must pass)
- ALWAYS include receipt chain URLs in commits

#### 8.2 verifier
**Role**: Test suite execution coordinator
**Specialization**: Validation orchestration
**Responsibilities**:
- Coordinate all test agents
- Run comprehensive validation
- Collect results
- Generate validation report

**Input**: Code changes
**Output**: Comprehensive validation report
**Communication**: Coordinates all test agents

**Supervision Strategy**: `permanent`

### 9. Quality System Layer (TPS)

#### 9.1 agent-16-jidoka (Built-In Quality)
**Role**: Automated quality with auto-stop
**Specialization**: Quality automation
**Responsibilities**:
- Auto-detect quality issues
- Stop-the-line automatically
- Trigger root cause analysis
- Resume after fix

**Input**: Quality gate results
**Output**: Jidoka events
**Communication**: Broadcasts quality alerts

**Supervision Strategy**: `permanent`

#### 9.2 agent-17-poka-yoke (Error-Proofing)
**Role**: Mistake prevention
**Specialization**: Design for correctness
**Responsibilities**:
- Validate schemas (jesse)
- Check message bounds
- Validate behavior types
- Prevent common mistakes

**Input**: Design artifacts
**Output**: Validation results
**Communication**: Reports preventable issues

**Supervision Strategy**: `permanent`

#### 9.3 agent-18-andon (Stop-the-Line)
**Role**: Quality alert system
**Specialization**: Problem escalation
**Responsibilities**:
- Monitor quality signals
- Trigger stop-the-line
- Escalate to humans
- Track resolution

**Input**: Quality gate failures
**Output**: Andon alerts + escalations
**Communication**: Broadcasts to all agents + dashboard

**Supervision Strategy**: `permanent` (critical monitoring)

#### 9.4 agent-19-tcps (TPS Quality System)
**Role**: Overall quality coordination
**Specialization**: Lean manufacturing for software
**Responsibilities**:
- Coordinate Andon, Poka-Yoke, Jidoka, Kaizen
- Manage work order lifecycle
- Track metrics
- Generate quality reports

**Input**: All quality signals
**Output**: TPS dashboard metrics
**Communication**: Integrates all quality agents

**Supervision Strategy**: `permanent`

### 10. Release Layer (Deployment)

#### 10.1 agent-20-release
**Role**: Release management
**Specialization**: Deployment coordination
**Responsibilities**:
- Version tagging
- Release notes generation
- Artifact packaging
- Deployment coordination

**Input**: Merged PRs
**Output**: Release artifacts
**Communication**: Coordinates with erlang-github-ops

**Supervision Strategy**: `transient` (only during releases)

---

## Work Decomposition Strategy

### Hierarchy: Epic → Story → Task → Subtask

#### Epic (Strategic Level)
**Definition**: Large feature or system implementation (5+ work orders)
**Duration**: 2-4 weeks
**Owner**: sparc-orchestrator
**Example**: "Implement MCP Subscription Protocol"

**Structure**:
```erlang
-type epic() :: #{
    id := epic_id(),
    title := binary(),
    description := binary(),
    acceptance_criteria := [binary()],
    stories := [story_id()],
    priority := 1..10,
    bucket := security | reliability | compliance | cost | features | technical_debt,
    created_at := calendar:datetime(),
    estimated_effort_hours := float(),
    owner := sparc-orchestrator
}.
```

**Decomposition Rules**:
1. Trigger EPIC 9 workflow if:
   - Affects 5+ files
   - Spans 3+ systems
   - Multiple implementation approaches
   - Duration > 1 week
2. Break into stories using SPARC phases
3. Expected speedup: 2.8x - 4.4x via parallelization

**Example Epic Decomposition**:
```
EPIC: Implement MCP Subscription Protocol
├── STORY-1: Specification Phase (plan-designer + erlang-researcher)
├── STORY-2: Pseudocode Design (plan-designer)
├── STORY-3: Architecture Design (erlang-architect)
├── STORY-4: Core Implementation (erlang-otp-developer)
├── STORY-5: Transport Integration (erlang-transport-builder)
├── STORY-6: Testing Suite (erlang-test-engineer)
├── STORY-7: Performance Validation (erlang-performance)
└── STORY-8: Documentation & PR (code-reviewer + erlang-github-ops)
```

#### Story (Tactical Level)
**Definition**: Cohesive unit of work completing a SPARC phase or major component
**Duration**: 2-5 days
**Owner**: phase-specific agent (e.g., erlang-architect for architecture stories)
**Example**: "Design subscription supervision tree"

**Structure**:
```erlang
-type story() :: #{
    id := story_id(),
    epic_id := epic_id(),
    title := binary(),
    description := binary(),
    acceptance_criteria := [binary()],
    tasks := [task_id()],
    dependencies := [story_id()],
    sparc_phase := specification | pseudocode | architecture | refinement | completion,
    priority := 1..10,
    assigned_agent := agent_role(),
    status := queued | in_progress | review | blocked | completed,
    created_at := calendar:datetime(),
    started_at := calendar:datetime() | undefined,
    completed_at := calendar:datetime() | undefined
}.
```

**Decomposition Rules**:
1. Each story maps to ONE SPARC phase
2. Stories executed sequentially (Spec → Pseudocode → Architecture → Refinement → Completion)
3. Within phase, stories can be parallelized if no dependencies
4. Story MUST have clear acceptance criteria

**Example Story Decomposition**:
```
STORY-3: Architecture Design
├── TASK-3.1: Design supervision tree (erlang-architect)
├── TASK-3.2: Define gen_server behaviors (erlang-architect)
├── TASK-3.3: Specify callback APIs (erlang-architect)
├── TASK-3.4: Create child specs (erlang-architect)
└── TASK-3.5: Document architecture decisions (erlang-architect)
```

#### Task (Operational Level)
**Definition**: Single unit of work producing a concrete artifact
**Duration**: 2-8 hours
**Owner**: Individual agent
**Example**: "Implement erlmcp_subscription:init/1 callback"

**Structure**:
```erlang
-type task() :: #{
    id := task_id(),
    story_id := story_id(),
    title := binary(),
    description := binary(),
    acceptance_criteria := [binary()],
    subtasks := [subtask_id()],
    dependencies := [task_id()],
    assigned_agent := agent_role(),
    status := queued | in_progress | blocked | completed | failed,
    priority := 1..10,
    estimated_hours := float(),
    actual_hours := float() | undefined,
    created_at := calendar:datetime(),
    started_at := calendar:datetime() | undefined,
    completed_at := calendar:datetime() | undefined,
    artifacts := [artifact_path()],
    quality_gates := [quality_gate()]
}.
```

**Decomposition Rules**:
1. Task produces ONE artifact (module, test, doc)
2. Task assigned to ONE agent
3. Task duration ≤ 8 hours (if longer, decompose into subtasks)
4. Task MUST have quality gate validation

**Example Task Decomposition**:
```
TASK-4.2: Implement subscription gen_server
├── SUBTASK-4.2.1: Write type specs (-spec annotations)
├── SUBTASK-4.2.2: Implement init/1 (non-blocking)
├── SUBTASK-4.2.3: Implement handle_call/3 for subscribe
├── SUBTASK-4.2.4: Implement handle_call/3 for unsubscribe
├── SUBTASK-4.2.5: Implement handle_info/2 for notifications
└── SUBTASK-4.2.6: Implement terminate/2 for cleanup
```

#### Subtask (Micro Level)
**Definition**: Individual function or component implementation
**Duration**: 30 minutes - 2 hours
**Owner**: Same agent as parent task
**Example**: "Implement handle_call({subscribe, Resource}, From, State)"

**Structure**:
```erlang
-type subtask() :: #{
    id := subtask_id(),
    task_id := task_id(),
    description := binary(),
    acceptance_criteria := binary(),
    estimated_minutes := integer(),
    actual_minutes := integer() | undefined,
    status := pending | done,
    code_changes := #{file := binary(), function := atom(), arity := integer()}
}.
```

**Decomposition Rules**:
1. Subtask = 1 function implementation or similar unit
2. Subtask ≤ 2 hours
3. Subtask tracked in TodoWrite (activeForm + content)
4. Subtask marked done immediately after completion

### Work Order Protocol

Work orders are the primary unit of coordination, based on TCPS work order system.

```erlang
-type work_order() :: #{
    id := work_order_id(),           % Format: "WO-timestamp-random"
    type := epic | story | task,     % Hierarchy level
    parent_id := work_order_id() | undefined,
    title := binary(),
    description := binary(),
    pull_signal := pull_signal(),    % Original demand source
    bucket := bucket(),              % Security, reliability, features, etc.
    priority := 1..10,               % 10 = critical
    status := work_order_status(),
    assigned_agents := [agent_role()],
    dependencies := [work_order_id()],
    blocking := [work_order_id()],   % Work orders blocked by this one
    sparc_phase := sparc_phase() | undefined,
    created_at := calendar:datetime(),
    sla_deadline := calendar:datetime(),
    started_at := calendar:datetime() | undefined,
    completed_at := calendar:datetime() | undefined,
    current_stage := stage(),
    stages_completed := [stage()],
    artifacts := [artifact()],
    quality_gates := [quality_gate_result()],
    receipts := [receipt_id()],
    metadata := map()
}.

-type work_order_status() ::
    queued |           % Created, waiting in priority queue
    in_progress |      % Active work, consuming WIP slot
    review |           % Code review phase
    testing |          % Testing phase
    blocked |          % Waiting for dependencies
    completed |        % Work finished successfully
    failed |           % Work failed quality gates
    cancelled.         % Work cancelled

-type stage() ::
    requirements | design | implementation | testing | integration | deployment | published.

-type bucket() ::
    security |         % SLA: 24 hours
    reliability |      % SLA: 7 days
    compliance |       % SLA: 7 days
    cost |            % SLA: 30 days
    features |        % SLA: 30 days
    technical_debt.   % SLA: best effort

-type pull_signal() :: #{
    type := github_issue | cve | marketplace_install | marketplace_refund | internal_request,
    source := binary(),
    labels := [binary()],
    metadata := map()
}.
```

### Decomposition Algorithm

```erlang
%% Top-down decomposition: Epic → Stories → Tasks
decompose_epic(Epic) ->
    %% Phase 1: SPARC phase decomposition
    SpecStory = create_story(Epic, specification, plan_designer),
    PseudoStory = create_story(Epic, pseudocode, plan_designer),
    ArchStory = create_story(Epic, architecture, erlang_architect),
    RefineStory = create_story(Epic, refinement, erlang_otp_developer),
    CompleteStory = create_story(Epic, completion, code_reviewer),

    %% Phase 2: Add dependencies (sequential SPARC phases)
    add_dependency(PseudoStory, SpecStory),
    add_dependency(ArchStory, PseudoStory),
    add_dependency(RefineStory, ArchStory),
    add_dependency(CompleteStory, RefineStory),

    %% Phase 3: Decompose refinement story into parallel tasks
    RefineStory_Tasks = [
        create_task(RefineStory, <<"Implement core logic">>, erlang_otp_developer),
        create_task(RefineStory, <<"Build transport layer">>, erlang_transport_builder),
        create_task(RefineStory, <<"Write tests">>, erlang_test_engineer),
        create_task(RefineStory, <<"Benchmark performance">>, erlang_performance)
    ],

    Stories = [SpecStory, PseudoStory, ArchStory, RefineStory, CompleteStory],
    {ok, #{epic => Epic, stories => Stories, tasks => RefineStory_Tasks}}.

%% Bottom-up aggregation: Subtasks → Tasks → Stories → Epic
aggregate_completion(Subtasks) ->
    case all_subtasks_complete(Subtasks) of
        true ->
            Task = get_parent_task(Subtasks),
            complete_task(Task),
            check_story_completion(Task);
        false ->
            {incomplete, pending_subtasks(Subtasks)}
    end.

check_story_completion(Task) ->
    Story = get_parent_story(Task),
    case all_tasks_complete(Story) of
        true ->
            complete_story(Story),
            check_epic_completion(Story);
        false ->
            {incomplete, pending_tasks(Story)}
    end.

check_epic_completion(Story) ->
    Epic = get_parent_epic(Story),
    case all_stories_complete(Epic) of
        true ->
            complete_epic(Epic),
            {completed, Epic};
        false ->
            {incomplete, pending_stories(Epic)}
    end.
```

---

## Dependency Management

### Dependency Types

#### 1. Sequential Dependencies (SPARC Phases)
**Definition**: Work order B cannot start until work order A completes
**Example**: Architecture phase cannot start until Specification phase completes

**Implementation**:
```erlang
-spec add_dependency(DependentWorkOrder :: work_order_id(),
                     BlockingWorkOrder :: work_order_id()) ->
    ok | {error, circular_dependency | not_found}.

%% Before starting work order, check all dependencies resolved
-spec can_start_work_order(WorkOrderId :: work_order_id()) ->
    {ok, ready} | {blocked, [work_order_id()]}.
can_start_work_order(WorkOrderId) ->
    Dependencies = get_dependencies(WorkOrderId),
    BlockedBy = lists:filter(fun(DepId) ->
        not is_completed(DepId)
    end, Dependencies),
    case BlockedBy of
        [] -> {ok, ready};
        _ -> {blocked, BlockedBy}
    end.
```

#### 2. Parallel Dependencies (Fan-Out/Fan-In)
**Definition**: Multiple work orders can execute in parallel, but all must complete before next phase
**Example**: Implementation tasks (core, transport, tests) run in parallel during Refinement phase

**Implementation**:
```erlang
%% Fan-out: Create parallel work orders
fan_out(Story, TaskSpecs) ->
    Tasks = [create_task(Story, Spec) || Spec <- TaskSpecs],
    %% All tasks depend on story start
    [add_dependency(Task, Story) || Task <- Tasks],
    %% Mark story as "waiting for fan-in"
    update_story_status(Story, fan_out_in_progress),
    {ok, Tasks}.

%% Fan-in: Wait for all parallel tasks to complete
fan_in(Story, Tasks) ->
    case all_tasks_complete(Tasks) of
        true ->
            update_story_status(Story, completed),
            {ok, completed};
        false ->
            {waiting, incomplete_tasks(Tasks)}
    end.
```

#### 3. Data Dependencies
**Definition**: Work order B requires artifacts produced by work order A
**Example**: Test task depends on implementation task producing the module

**Implementation**:
```erlang
-spec add_data_dependency(Consumer :: work_order_id(),
                         Producer :: work_order_id(),
                         RequiredArtifact :: artifact_path()) ->
    ok | {error, term()}.

%% Check artifact availability before starting
-spec can_start_with_artifact(WorkOrderId :: work_order_id(),
                              RequiredArtifact :: artifact_path()) ->
    {ok, ready} | {blocked, artifact_not_available}.
can_start_with_artifact(WorkOrderId, RequiredArtifact) ->
    case artifact_exists(RequiredArtifact) of
        true -> {ok, ready};
        false -> {blocked, artifact_not_available}
    end.
```

#### 4. Resource Dependencies (WIP Limits)
**Definition**: Work order can only start if WIP capacity available
**Example**: Cannot start new implementation task if already 5 tasks in progress

**Implementation**:
```erlang
-spec check_wip_limit(Bucket :: bucket()) ->
    {ok, capacity_available} | {error, wip_limit_exceeded}.
check_wip_limit(Bucket) ->
    Limit = get_wip_limit(Bucket),
    Active = count_active_work_orders(Bucket),
    if
        Active < Limit -> {ok, capacity_available};
        true -> {error, wip_limit_exceeded}
    end.

%% WIP limits by bucket
-define(WIP_LIMITS, #{
    security => 5,
    reliability => 5,
    compliance => 5,
    cost => 5,
    features => 10,  % Higher capacity for features
    technical_debt => 5
}).
```

### Dependency Graph Management

#### Graph Structure
```erlang
-type dependency_graph() :: digraph:graph().

%% Build dependency graph for epic
build_dependency_graph(Epic) ->
    G = digraph:new([acyclic]),
    Stories = get_stories(Epic),

    %% Add vertices (work orders)
    [digraph:add_vertex(G, Story) || Story <- Stories],

    %% Add edges (dependencies)
    lists:foreach(fun(Story) ->
        Dependencies = get_dependencies(Story),
        lists:foreach(fun(Dep) ->
            digraph:add_edge(G, Dep, Story)
        end, Dependencies)
    end, Stories),

    {ok, G}.

%% Detect circular dependencies
-spec detect_circular_dependency(Graph :: dependency_graph(),
                                 NewDep :: {work_order_id(), work_order_id()}) ->
    ok | {error, circular_dependency}.
detect_circular_dependency(Graph, {From, To}) ->
    %% Check if adding edge creates cycle
    case digraph:get_path(Graph, To, From) of
        false -> ok;  % No path, safe to add
        Path -> {error, {circular_dependency, Path}}
    end.

%% Topological sort for execution order
-spec topological_sort(Graph :: dependency_graph()) ->
    {ok, [work_order_id()]} | {error, has_cycle}.
topological_sort(Graph) ->
    case digraph_utils:topsort(Graph) of
        false -> {error, has_cycle};
        Order -> {ok, Order}
    end.

%% Critical path analysis (longest path)
-spec critical_path(Graph :: dependency_graph()) ->
    {ok, [work_order_id()], EstimatedHours :: float()}.
critical_path(Graph) ->
    Paths = digraph_utils:reaching([root_vertex(Graph)], Graph),
    PathsWithDurations = lists:map(fun(Path) ->
        Duration = lists:sum([get_estimated_hours(WO) || WO <- Path]),
        {Path, Duration}
    end, Paths),
    {CriticalPath, Duration} = lists:max(PathsWithDurations),
    {ok, CriticalPath, Duration}.
```

#### Dependency Resolution

```erlang
%% Automatic unblocking when dependency completes
-spec resolve_dependency(CompletedWorkOrder :: work_order_id()) -> ok.
resolve_dependency(CompletedWorkOrder) ->
    BlockedWorkOrders = get_blocked_by(CompletedWorkOrder),
    lists:foreach(fun(BlockedWO) ->
        RemainingDeps = get_unresolved_dependencies(BlockedWO),
        case RemainingDeps of
            [] ->
                %% All dependencies resolved, unblock
                update_status(BlockedWO, queued),
                notify_ready(BlockedWO);
            _ ->
                %% Still has dependencies
                ok
        end
    end, BlockedWorkOrders).

%% Cascade failure handling
-spec handle_dependency_failure(FailedWorkOrder :: work_order_id()) ->
    {ok, AffectedWorkOrders :: [work_order_id()]}.
handle_dependency_failure(FailedWorkOrder) ->
    %% Get all work orders that depend on failed one
    DependentWorkOrders = get_dependent_work_orders(FailedWorkOrder),

    %% Option 1: Block all dependents
    lists:foreach(fun(WO) ->
        update_status(WO, blocked),
        add_blocker_reason(WO, {dependency_failed, FailedWorkOrder})
    end, DependentWorkOrders),

    %% Option 2: Trigger Andon alert for manual intervention
    trigger_andon_alert(dependency_chain_failure, #{
        failed_work_order => FailedWorkOrder,
        affected_work_orders => DependentWorkOrders
    }),

    {ok, DependentWorkOrders}.
```

### Dependency Visualization

```erlang
%% Generate Mermaid diagram for dependencies
generate_dependency_diagram(Epic) ->
    Graph = build_dependency_graph(Epic),
    Vertices = digraph:vertices(Graph),
    Edges = digraph:edges(Graph),

    Mermaid = [
        <<"graph TD\n">>,
        [format_vertex(V, Graph) || V <- Vertices],
        [format_edge(E, Graph) || E <- Edges]
    ],

    iolist_to_binary(Mermaid).

format_vertex(VertexId, Graph) ->
    WO = digraph:vertex(Graph, VertexId),
    Status = get_status(WO),
    Color = status_color(Status),
    io_lib:format("    ~s[~s]:::~s\n", [VertexId, get_title(WO), Color]).

format_edge(EdgeId, Graph) ->
    {_, From, To, _} = digraph:edge(Graph, EdgeId),
    io_lib:format("    ~s --> ~s\n", [From, To]).

status_color(queued) -> "blue";
status_color(in_progress) -> "yellow";
status_color(completed) -> "green";
status_color(blocked) -> "red";
status_color(failed) -> "darkred".
```

---

## Communication Patterns

### 1. Message Passing (Asynchronous)

#### Work Order Events
```erlang
%% Event bus for work order state changes
-type work_order_event() ::
    {work_order_created, work_order_id(), work_order()} |
    {work_order_started, work_order_id(), agent_role()} |
    {work_order_progressed, work_order_id(), stage()} |
    {work_order_completed, work_order_id(), sku_id()} |
    {work_order_failed, work_order_id(), reason()} |
    {work_order_blocked, work_order_id(), [work_order_id()]} |
    {work_order_unblocked, work_order_id()}.

%% Publish event to all subscribers
-spec publish_event(Event :: work_order_event()) -> ok.
publish_event(Event) ->
    Subscribers = get_subscribers(work_order_events),
    lists:foreach(fun(Subscriber) ->
        Subscriber ! Event
    end, Subscribers),
    ok.

%% Subscribe to work order events
-spec subscribe_events(Pid :: pid()) -> ok.
subscribe_events(Pid) ->
    gen_server:cast(event_bus, {subscribe, Pid, work_order_events}).
```

#### Agent-to-Agent Communication
```erlang
%% Request-response pattern (synchronous)
-spec request_research(Query :: binary(), Timeout :: integer()) ->
    {ok, research_summary()} | {error, timeout}.
request_research(Query, Timeout) ->
    %% plan-designer → erlang-researcher
    gen_server:call({agent, erlang_researcher}, {research, Query}, Timeout).

%% Cast pattern (asynchronous fire-and-forget)
-spec notify_completion(WorkOrderId :: work_order_id()) -> ok.
notify_completion(WorkOrderId) ->
    %% erlang-otp-developer → sparc-orchestrator
    gen_server:cast({agent, sparc_orchestrator}, {completed, WorkOrderId}).

%% Broadcast pattern (one-to-many)
-spec broadcast_quality_alert(Alert :: andon_alert()) -> ok.
broadcast_quality_alert(Alert) ->
    %% agent-18-andon → all agents
    Agents = get_all_agents(),
    lists:foreach(fun(Agent) ->
        gen_server:cast(Agent, {quality_alert, Alert})
    end, Agents).
```

### 2. State Sharing (ETS Tables)

#### Shared State Tables
```erlang
%% Work order registry (public, read-concurrent)
create_work_order_table() ->
    ets:new(work_orders, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

%% Agent registry (public, read-concurrent)
create_agent_table() ->
    ets:new(agents, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {keypos, #agent.id}
    ]).

%% Quality metrics (public, ordered_set for time-series)
create_metrics_table() ->
    ets:new(metrics, [
        ordered_set,
        public,
        named_table,
        {read_concurrency, true}
    ]).

%% Read patterns (fast, concurrent)
-spec get_work_order(WorkOrderId :: work_order_id()) ->
    {ok, work_order()} | {error, not_found}.
get_work_order(WorkOrderId) ->
    case ets:lookup(work_orders, WorkOrderId) of
        [{WorkOrderId, WorkOrder}] -> {ok, WorkOrder};
        [] -> {error, not_found}
    end.

%% Write patterns (coordinated via gen_server)
-spec update_work_order(WorkOrder :: work_order()) -> ok.
update_work_order(WorkOrder) ->
    gen_server:call(work_order_manager, {update, WorkOrder}).
```

#### GPROC Registry for Agent Discovery
```erlang
%% Register agent with gproc
-spec register_agent(Role :: agent_role(), Pid :: pid()) -> ok.
register_agent(Role, Pid) ->
    gproc:reg({n, l, {agent, Role}}, Pid).

%% Lookup agent by role
-spec find_agent(Role :: agent_role()) ->
    {ok, pid()} | {error, not_found}.
find_agent(Role) ->
    case gproc:lookup_pid({n, l, {agent, Role}}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%% Publish-subscribe via gproc
-spec subscribe_topic(Topic :: atom()) -> ok.
subscribe_topic(Topic) ->
    gproc:reg({p, l, {topic, Topic}}).

-spec publish_topic(Topic :: atom(), Message :: term()) -> ok.
publish_topic(Topic, Message) ->
    gproc:send({p, l, {topic, Topic}}, Message).
```

### 3. Coordination Protocols

#### Kanban Pull Protocol
```erlang
%% Agent pulls work from prioritized queue
-spec pull_work(Bucket :: bucket(), AgentRole :: agent_role()) ->
    {ok, work_order()} | {empty} | {error, wip_limit}.
pull_work(Bucket, AgentRole) ->
    %% Check WIP limit
    case check_wip_limit(Bucket) of
        {ok, capacity_available} ->
            %% Dequeue highest priority work order
            case dequeue_next(Bucket) of
                {ok, WorkOrder} ->
                    %% Assign to agent and start
                    assign_and_start(WorkOrder, AgentRole),
                    {ok, WorkOrder};
                empty ->
                    {empty}
            end;
        {error, wip_limit_exceeded} ->
            {error, wip_limit}
    end.

%% Two-phase commit for agent assignment
assign_and_start(WorkOrder, AgentRole) ->
    %% Phase 1: Reserve WIP slot
    ok = reserve_wip_slot(get_bucket(WorkOrder)),

    %% Phase 2: Assign and start (or rollback)
    try
        ok = assign_agent(WorkOrder, AgentRole),
        ok = update_status(WorkOrder, in_progress),
        ok = start_work(WorkOrder, AgentRole),
        {ok, started}
    catch
        Error:Reason ->
            %% Rollback: Release WIP slot
            release_wip_slot(get_bucket(WorkOrder)),
            {error, {failed_to_start, Error, Reason}}
    end.
```

#### Handoff Protocol (Agent-to-Agent)
```erlang
%% Handoff work order between SPARC phases
-spec handoff_work_order(WorkOrder :: work_order(),
                        FromAgent :: agent_role(),
                        ToAgent :: agent_role()) ->
    {ok, accepted} | {error, rejected, reason()}.
handoff_work_order(WorkOrder, FromAgent, ToAgent) ->
    %% Step 1: FromAgent marks work order ready for handoff
    ok = update_status(WorkOrder, ready_for_handoff),
    ok = generate_handoff_receipt(WorkOrder, FromAgent),

    %% Step 2: ToAgent validates preconditions
    case validate_handoff(WorkOrder, ToAgent) of
        {ok, valid} ->
            %% Step 3: ToAgent accepts and starts
            ok = accept_handoff(WorkOrder, ToAgent),
            ok = update_status(WorkOrder, in_progress),
            {ok, accepted};
        {error, Reason} ->
            %% Rollback
            ok = update_status(WorkOrder, handoff_rejected),
            {error, rejected, Reason}
    end.

validate_handoff(WorkOrder, ToAgent) ->
    Checks = [
        fun() -> check_artifacts_present(WorkOrder) end,
        fun() -> check_quality_gates_passed(WorkOrder) end,
        fun() -> check_agent_capacity(ToAgent) end,
        fun() -> check_dependencies_resolved(WorkOrder) end
    ],
    run_checks(Checks).
```

#### Consensus Protocol (Design Decisions)
```erlang
%% Request consensus from agent group on design decision
-spec request_consensus(Decision :: design_decision(),
                       Voters :: [agent_role()],
                       Timeout :: integer()) ->
    {consensus, approved | rejected} | {split, Votes :: map()}.
request_consensus(Decision, Voters, Timeout) ->
    %% Step 1: Broadcast decision to voters
    RequestId = make_ref(),
    lists:foreach(fun(Voter) ->
        gen_server:cast({agent, Voter}, {vote_request, RequestId, Decision, self()})
    end, Voters),

    %% Step 2: Collect votes
    Votes = collect_votes(RequestId, Voters, Timeout),

    %% Step 3: Tally votes
    {Approve, Reject, Abstain} = tally_votes(Votes),

    %% Step 4: Determine consensus (2/3 majority)
    Total = Approve + Reject + Abstain,
    if
        Approve >= (Total * 2) div 3 -> {consensus, approved};
        Reject >= (Total * 2) div 3 -> {consensus, rejected};
        true -> {split, #{approve => Approve, reject => Reject, abstain => Abstain}}
    end.

collect_votes(RequestId, Voters, Timeout) ->
    collect_votes_loop(RequestId, Voters, [], Timeout).

collect_votes_loop(_RequestId, [], Votes, _Timeout) ->
    Votes;
collect_votes_loop(RequestId, Voters, Votes, Timeout) ->
    receive
        {vote, RequestId, Voter, Vote} ->
            collect_votes_loop(RequestId, lists:delete(Voter, Voters),
                             [{Voter, Vote} | Votes], Timeout)
    after Timeout ->
        %% Timeout: Count missing votes as abstain
        [{V, abstain} || V <- Voters] ++ Votes
    end.
```

### 4. Error Propagation

```erlang
%% Error event propagation
-type error_event() :: #{
    type := compilation_error | test_failure | quality_gate_failure,
    work_order_id := work_order_id(),
    agent := agent_role(),
    error_details := term(),
    timestamp := calendar:datetime(),
    severity := critical | high | medium | low
}.

%% Propagate error up the chain
-spec propagate_error(ErrorEvent :: error_event()) -> ok.
propagate_error(ErrorEvent) ->
    %% Step 1: Log error
    log_error(ErrorEvent),

    %% Step 2: Trigger Andon based on severity
    case maps:get(severity, ErrorEvent) of
        critical ->
            trigger_andon_alert(stop_the_line, ErrorEvent),
            halt_all_work();
        high ->
            trigger_andon_alert(warning, ErrorEvent),
            halt_dependent_work(ErrorEvent);
        _ ->
            log_warning(ErrorEvent)
    end,

    %% Step 3: Notify orchestrator
    notify_orchestrator(error_occurred, ErrorEvent),

    %% Step 4: Notify dependent agents
    notify_dependent_agents(ErrorEvent),

    ok.
```

---

## Consensus Mechanisms

### Design Decision Voting

#### Voting Triggers
1. **Architecture Decisions**: Behavior selection, supervision strategy, module boundaries
2. **API Design**: Public interface changes, breaking changes
3. **Performance Tradeoffs**: Memory vs. speed, latency vs. throughput
4. **Security Decisions**: Auth mechanisms, encryption algorithms
5. **Conflict Resolution**: Multiple valid implementation approaches

#### Voting Protocol

```erlang
-type vote() :: approve | reject | abstain.
-type design_decision() :: #{
    id := decision_id(),
    title := binary(),
    description := binary(),
    options := [option()],
    rationale := binary(),
    impact := high | medium | low,
    proposed_by := agent_role(),
    voters := [agent_role()],
    deadline := calendar:datetime()
}.

-type option() :: #{
    id := option_id(),
    description := binary(),
    pros := [binary()],
    cons := [binary()],
    estimated_effort := float()
}.

%% Initiate design decision vote
-spec initiate_vote(Decision :: design_decision()) ->
    {ok, decision_id()} | {error, term()}.
initiate_vote(Decision) ->
    %% Step 1: Validate decision
    ok = validate_decision(Decision),

    %% Step 2: Store decision
    DecisionId = generate_decision_id(),
    ok = store_decision(DecisionId, Decision),

    %% Step 3: Notify voters
    Voters = maps:get(voters, Decision),
    lists:foreach(fun(Voter) ->
        notify_vote_request(Voter, DecisionId, Decision)
    end, Voters),

    %% Step 4: Set deadline timer
    Deadline = maps:get(deadline, Decision),
    schedule_vote_close(DecisionId, Deadline),

    {ok, DecisionId}.

%% Cast vote
-spec cast_vote(DecisionId :: decision_id(),
               Voter :: agent_role(),
               OptionId :: option_id(),
               Comment :: binary()) ->
    ok | {error, already_voted | deadline_passed}.
cast_vote(DecisionId, Voter, OptionId, Comment) ->
    case can_vote(DecisionId, Voter) of
        {ok, allowed} ->
            Vote = #{
                voter => Voter,
                option_id => OptionId,
                comment => Comment,
                timestamp => calendar:universal_time()
            },
            ok = record_vote(DecisionId, Vote),
            check_quorum(DecisionId);
        {error, Reason} ->
            {error, Reason}
    end.

%% Tally votes and decide
-spec tally_votes(DecisionId :: decision_id()) ->
    {consensus, option_id()} | {no_consensus, vote_distribution()}.
tally_votes(DecisionId) ->
    Votes = get_votes(DecisionId),
    Decision = get_decision(DecisionId),

    %% Group votes by option
    VoteDistribution = lists:foldl(fun(Vote, Acc) ->
        OptionId = maps:get(option_id, Vote),
        maps:update_with(OptionId, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Votes),

    %% Determine winner (2/3 majority or highest count)
    TotalVotes = length(Votes),
    case find_majority(VoteDistribution, TotalVotes) of
        {majority, OptionId} ->
            %% 2/3+ votes for one option
            ok = record_decision_outcome(DecisionId, OptionId, consensus),
            {consensus, OptionId};
        {plurality, OptionId} ->
            %% Highest count but no majority - escalate
            ok = record_decision_outcome(DecisionId, OptionId, plurality),
            escalate_decision(DecisionId, VoteDistribution);
        tie ->
            %% Tie - escalate to human
            escalate_to_human(DecisionId, VoteDistribution)
    end.

find_majority(VoteDistribution, TotalVotes) ->
    Threshold = (TotalVotes * 2) div 3,
    case lists:filter(fun({_, Count}) -> Count >= Threshold end,
                     maps:to_list(VoteDistribution)) of
        [{OptionId, _}] -> {majority, OptionId};
        [] ->
            %% No majority, find plurality
            [{WinningOption, _} | _] = lists:reverse(lists:keysort(2,
                                                     maps:to_list(VoteDistribution))),
            case count_ties(VoteDistribution) of
                1 -> {plurality, WinningOption};
                _ -> tie
            end
    end.
```

#### Voting Agent Composition

```erlang
%% Voting groups by decision type
-define(ARCHITECTURE_VOTERS, [
    erlang_architect,
    erlang_otp_developer,
    erlang_transport_builder,
    code_reviewer
]).

-define(API_DESIGN_VOTERS, [
    erlang_architect,
    erlang_otp_developer,
    plan_designer,
    code_reviewer
]).

-define(PERFORMANCE_VOTERS, [
    erlang_performance,
    erlang_otp_developer,
    erlang_architect
]).

-define(SECURITY_VOTERS, [
    code_reviewer,
    erlang_architect,
    erlang_otp_developer
]).

%% Select voters based on decision impact
select_voters(Decision) ->
    Impact = maps:get(impact, Decision),
    Type = maps:get(type, Decision),

    BaseVoters = case Type of
        architecture -> ?ARCHITECTURE_VOTERS;
        api_design -> ?API_DESIGN_VOTERS;
        performance -> ?PERFORMANCE_VOTERS;
        security -> ?SECURITY_VOTERS
    end,

    %% For high-impact decisions, add more reviewers
    case Impact of
        high -> BaseVoters ++ [code_reviewer, plan_designer];
        _ -> BaseVoters
    end.
```

### Conflict Resolution

#### Conflict Types
1. **Resource Conflicts**: Two agents need same WIP slot
2. **Design Conflicts**: Incompatible architectural decisions
3. **Priority Conflicts**: Disagreement on work order priority
4. **Quality Conflicts**: Different interpretations of quality gates

#### Resolution Protocol

```erlang
-type conflict() :: #{
    id := conflict_id(),
    type := resource | design | priority | quality,
    parties := [agent_role()],
    description := binary(),
    proposed_resolutions := [resolution()],
    status := open | voting | escalated | resolved,
    created_at := calendar:datetime()
}.

-type resolution() :: #{
    id := resolution_id(),
    description := binary(),
    proposed_by := agent_role(),
    votes := [vote()],
    outcome := term()
}.

%% Detect and raise conflict
-spec raise_conflict(Conflict :: conflict()) ->
    {ok, conflict_id()} | {error, term()}.
raise_conflict(Conflict) ->
    %% Step 1: Log conflict
    ConflictId = generate_conflict_id(),
    ok = store_conflict(ConflictId, Conflict),

    %% Step 2: Notify parties
    Parties = maps:get(parties, Conflict),
    lists:foreach(fun(Party) ->
        notify_conflict(Party, ConflictId, Conflict)
    end, Parties),

    %% Step 3: Request resolutions
    ok = request_resolutions(ConflictId, Parties),

    {ok, ConflictId}.

%% Resolution strategies (in order of preference)
resolve_conflict(ConflictId) ->
    Conflict = get_conflict(ConflictId),

    %% Strategy 1: Automated resolution rules
    case apply_resolution_rules(Conflict) of
        {ok, Resolution} ->
            apply_resolution(ConflictId, Resolution);
        no_rule_applies ->
            %% Strategy 2: Agent voting
            case initiate_conflict_vote(ConflictId) of
                {consensus, Resolution} ->
                    apply_resolution(ConflictId, Resolution);
                no_consensus ->
                    %% Strategy 3: Escalate to human
                    escalate_conflict_to_human(ConflictId)
            end
    end.

%% Automated resolution rules
apply_resolution_rules(Conflict) ->
    Type = maps:get(type, Conflict),
    case Type of
        resource ->
            %% Rule: Higher priority work order wins
            resolve_by_priority(Conflict);
        priority ->
            %% Rule: Security bucket always wins
            resolve_by_bucket(Conflict);
        design ->
            %% Rule: Follow existing patterns (consistency)
            resolve_by_consistency(Conflict);
        quality ->
            %% Rule: Stricter gate wins
            resolve_by_strictness(Conflict)
    end.

resolve_by_priority(Conflict) ->
    Parties = maps:get(parties, Conflict),
    WorkOrders = [get_current_work_order(Party) || Party <- Parties],
    Priorities = [{WO, get_priority(WO)} || WO <- WorkOrders],
    {WinningWO, _} = lists:max(Priorities),
    WinningAgent = get_assigned_agent(WinningWO),
    {ok, #{winner => WinningAgent, reason => highest_priority}}.
```

---

## Progress Tracking

### Real-Time Dashboard

#### Dashboard Architecture
```erlang
-module(agent_coordination_dashboard).
-behaviour(gen_server).

-record(dashboard_state, {
    work_orders = #{} :: #{work_order_id() => work_order()},
    agents = #{} :: #{agent_role() => agent_status()},
    metrics = #{} :: #{metric_name() => metric_value()},
    events = [] :: [event()],
    subscribers = [] :: [pid()]
}).

-type agent_status() :: #{
    role := agent_role(),
    pid := pid(),
    status := idle | busy | blocked | error,
    current_work_order := work_order_id() | undefined,
    completed_count := integer(),
    error_count := integer(),
    last_heartbeat := calendar:datetime()
}.

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_dashboard_state() ->
    gen_server:call(?MODULE, get_state).

subscribe_updates(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

update_work_order(WorkOrder) ->
    gen_server:cast(?MODULE, {update_work_order, WorkOrder}).

update_agent_status(Agent, Status) ->
    gen_server:cast(?MODULE, {update_agent, Agent, Status}).

%% gen_server callbacks
handle_cast({update_work_order, WorkOrder}, State) ->
    WoId = maps:get(id, WorkOrder),
    NewWorkOrders = maps:put(WoId, WorkOrder, State#dashboard_state.work_orders),
    NewState = State#dashboard_state{work_orders = NewWorkOrders},

    %% Broadcast update to subscribers
    broadcast_update({work_order_updated, WorkOrder}, NewState),

    {noreply, NewState};

handle_cast({update_agent, Agent, Status}, State) ->
    NewAgents = maps:put(Agent, Status, State#dashboard_state.agents),
    NewState = State#dashboard_state{agents = NewAgents},

    %% Broadcast update to subscribers
    broadcast_update({agent_updated, Agent, Status}, NewState),

    {noreply, NewState}.

broadcast_update(Event, State) ->
    Subscribers = State#dashboard_state.subscribers,
    lists:foreach(fun(Sub) ->
        Sub ! {dashboard_update, Event}
    end, Subscribers).
```

#### Dashboard Metrics

```erlang
%% Compute real-time metrics
compute_dashboard_metrics(State) ->
    WorkOrders = maps:values(State#dashboard_state.work_orders),
    Agents = maps:values(State#dashboard_state.agents),

    #{
        %% Work order metrics
        total_work_orders => length(WorkOrders),
        queued => count_by_status(WorkOrders, queued),
        in_progress => count_by_status(WorkOrders, in_progress),
        completed => count_by_status(WorkOrders, completed),
        blocked => count_by_status(WorkOrders, blocked),
        failed => count_by_status(WorkOrders, failed),

        %% Agent metrics
        total_agents => length(Agents),
        idle_agents => count_agent_status(Agents, idle),
        busy_agents => count_agent_status(Agents, busy),
        blocked_agents => count_agent_status(Agents, blocked),
        error_agents => count_agent_status(Agents, error),

        %% Performance metrics
        average_lead_time => calculate_average_lead_time(WorkOrders),
        throughput_per_hour => calculate_throughput(WorkOrders),
        sla_compliance_rate => calculate_sla_compliance(WorkOrders),

        %% Quality metrics
        test_pass_rate => calculate_test_pass_rate(WorkOrders),
        quality_gate_pass_rate => calculate_quality_gate_pass_rate(WorkOrders),

        %% WIP metrics by bucket
        wip_by_bucket => #{
            security => count_wip(WorkOrders, security),
            reliability => count_wip(WorkOrders, reliability),
            features => count_wip(WorkOrders, features),
            cost => count_wip(WorkOrders, cost),
            compliance => count_wip(WorkOrders, compliance),
            technical_debt => count_wip(WorkOrders, technical_debt)
        },

        %% Critical path
        critical_path => calculate_critical_path(State),

        %% Blockers
        blocking_work_orders => find_blocking_work_orders(WorkOrders)
    }.
```

### Receipt Chain

#### Receipt Structure
```erlang
-type receipt() :: #{
    id := receipt_id(),
    work_order_id := work_order_id(),
    agent := agent_role(),
    operation := operation_type(),
    timestamp := calendar:datetime(),
    artifacts := [artifact_path()],
    quality_gates := [quality_gate_result()],
    previous_receipt_hash := binary(),  % SHA-256 of previous receipt
    receipt_hash := binary(),           % SHA-256 of this receipt
    signature := binary(),              % Agent signature
    metadata := map()
}.

-type operation_type() ::
    created | started | progressed | completed | failed | cancelled |
    quality_gate_passed | quality_gate_failed |
    handoff | blocked | unblocked.

%% Generate receipt
-spec generate_receipt(WorkOrder :: work_order(),
                      Operation :: operation_type(),
                      Agent :: agent_role()) ->
    {ok, receipt()}.
generate_receipt(WorkOrder, Operation, Agent) ->
    %% Get previous receipt hash for chaining
    PreviousHash = get_latest_receipt_hash(get_id(WorkOrder)),

    Receipt = #{
        id => generate_receipt_id(),
        work_order_id => get_id(WorkOrder),
        agent => Agent,
        operation => Operation,
        timestamp => calendar:universal_time(),
        artifacts => get_artifacts(WorkOrder),
        quality_gates => get_quality_gates(WorkOrder),
        previous_receipt_hash => PreviousHash,
        metadata => #{
            erlang_version => erlang:system_info(otp_release),
            hostname => net_adm:localhost()
        }
    },

    %% Compute hash of this receipt
    ReceiptHash = compute_receipt_hash(Receipt),

    %% Add hash to receipt
    FinalReceipt = Receipt#{receipt_hash => ReceiptHash},

    %% Store receipt
    ok = store_receipt(FinalReceipt),

    %% Add to work order receipt chain
    ok = add_receipt_to_work_order(get_id(WorkOrder), get_id(FinalReceipt)),

    {ok, FinalReceipt}.

compute_receipt_hash(Receipt) ->
    %% Remove hash fields before hashing
    ReceiptWithoutHash = maps:without([receipt_hash, signature], Receipt),

    %% Serialize to binary
    Binary = term_to_binary(ReceiptWithoutHash),

    %% Compute SHA-256
    crypto:hash(sha256, Binary).

%% Verify receipt chain integrity
-spec verify_receipt_chain(WorkOrderId :: work_order_id()) ->
    {ok, valid} | {error, {broken_chain, receipt_id()}}.
verify_receipt_chain(WorkOrderId) ->
    Receipts = get_receipts_for_work_order(WorkOrderId),
    verify_chain_recursive(Receipts).

verify_chain_recursive([]) ->
    {ok, valid};
verify_chain_recursive([Receipt | Rest]) ->
    %% Verify hash matches
    StoredHash = maps:get(receipt_hash, Receipt),
    ComputedHash = compute_receipt_hash(Receipt),

    case StoredHash =:= ComputedHash of
        true ->
            %% Verify previous hash chain
            case Rest of
                [] ->
                    {ok, valid};
                [NextReceipt | _] ->
                    NextPrevHash = maps:get(previous_receipt_hash, NextReceipt),
                    case NextPrevHash =:= StoredHash of
                        true -> verify_chain_recursive(Rest);
                        false -> {error, {broken_chain, maps:get(id, NextReceipt)}}
                    end
            end;
        false ->
            {error, {invalid_hash, maps:get(id, Receipt)}}
    end.
```

### TodoWrite Integration

#### Synchronization Protocol
```erlang
%% Sync work order state with TodoWrite
-spec sync_to_todowrite(WorkOrder :: work_order()) -> ok.
sync_to_todowrite(WorkOrder) ->
    %% Convert work order to todo items
    Todos = work_order_to_todos(WorkOrder),

    %% Update TodoWrite
    TodoWrite(#{todos => Todos}),

    ok.

work_order_to_todos(WorkOrder) ->
    Tasks = get_tasks(WorkOrder),
    lists:map(fun(Task) ->
        #{
            content => get_title(Task),
            activeForm => get_active_form(Task),
            status => map_status(get_status(Task))
        }
    end, Tasks).

map_status(queued) -> "pending";
map_status(in_progress) -> "in_progress";
map_status(completed) -> "completed";
map_status(blocked) -> "pending";  % Show as pending but note blocker
map_status(_) -> "pending".

%% Reverse sync: TodoWrite updates work order
-spec sync_from_todowrite(Todos :: [todo()]) -> ok.
sync_from_todowrite(Todos) ->
    lists:foreach(fun(Todo) ->
        case find_task_by_content(Todo) of
            {ok, TaskId} ->
                NewStatus = map_todo_status(maps:get(status, Todo)),
                update_task_status(TaskId, NewStatus);
            {error, not_found} ->
                %% New task added manually, create work order
                create_adhoc_work_order(Todo)
        end
    end, Todos),
    ok.
```

### Reporting

#### Progress Reports
```erlang
%% Generate progress report for epic
-spec generate_progress_report(Epic :: epic()) -> progress_report().
generate_progress_report(Epic) ->
    Stories = get_stories(Epic),
    Tasks = lists:flatmap(fun get_tasks/1, Stories),

    #{
        epic_id => get_id(Epic),
        epic_title => get_title(Epic),

        %% Completion metrics
        total_stories => length(Stories),
        completed_stories => count_completed(Stories),
        completion_percentage => calculate_completion_percentage(Stories),

        %% Time metrics
        estimated_hours => sum_estimated_hours(Tasks),
        actual_hours => sum_actual_hours(Tasks),
        remaining_hours => calculate_remaining_hours(Tasks),

        %% Quality metrics
        tests_passed => count_tests_passed(Tasks),
        tests_failed => count_tests_failed(Tasks),
        coverage => calculate_overall_coverage(Tasks),

        %% Status breakdown
        status_breakdown => #{
            queued => count_by_status(Tasks, queued),
            in_progress => count_by_status(Tasks, in_progress),
            completed => count_by_status(Tasks, completed),
            blocked => count_by_status(Tasks, blocked),
            failed => count_by_status(Tasks, failed)
        },

        %% Blockers
        blocking_items => find_blocking_items(Tasks),

        %% Critical path
        critical_path => calculate_critical_path_report(Epic),

        %% Velocity
        velocity => calculate_velocity(Tasks),

        %% Forecasts
        estimated_completion => forecast_completion_date(Epic),
        at_risk_sla => find_at_risk_sla(Tasks)
    }.

%% Velocity calculation (story points per day)
calculate_velocity(Tasks) ->
    CompletedTasks = filter_by_status(Tasks, completed),

    %% Group by completion date
    ByDate = group_by_date(CompletedTasks),

    %% Calculate daily velocity
    DailyVelocities = maps:map(fun(_Date, DayTasks) ->
        sum_story_points(DayTasks)
    end, ByDate),

    %% Average velocity
    TotalPoints = lists:sum(maps:values(DailyVelocities)),
    TotalDays = maps:size(DailyVelocities),

    case TotalDays of
        0 -> 0.0;
        _ -> TotalPoints / TotalDays
    end.

%% Forecast completion date based on velocity
forecast_completion_date(Epic) ->
    Tasks = get_all_tasks(Epic),
    RemainingTasks = filter_by_status(Tasks, [queued, in_progress, blocked]),
    RemainingPoints = sum_story_points(RemainingTasks),

    Velocity = calculate_velocity(Tasks),

    case Velocity of
        0.0 -> undefined;  % Cannot forecast
        _ ->
            DaysRemaining = RemainingPoints / Velocity,
            Now = calendar:universal_time(),
            add_days(Now, round(DaysRemaining))
    end.
```

---

## Escalation and Conflict Resolution

### Andon System Integration

#### Andon Alert Levels
```erlang
-type andon_level() ::
    info |        % Informational, no action required
    warning |     % Warning, attention recommended
    stop_line |   % Stop all work, immediate attention required
    critical.     % Critical failure, escalate to humans

-type andon_alert() :: #{
    id := alert_id(),
    level := andon_level(),
    category := category(),
    work_order_id := work_order_id() | undefined,
    agent := agent_role() | undefined,
    message := binary(),
    details := map(),
    triggered_at := calendar:datetime(),
    acknowledged_at := calendar:datetime() | undefined,
    resolved_at := calendar:datetime() | undefined,
    resolution := binary() | undefined
}.

-type category() ::
    compilation_error | test_failure | quality_gate_failure |
    sla_breach | dependency_failure | resource_exhaustion |
    agent_failure | deadlock_detected | consensus_failed.
```

#### Triggering Andon Alerts
```erlang
%% Trigger andon alert
-spec trigger_andon(Level :: andon_level(),
                   Category :: category(),
                   Context :: map()) ->
    {ok, alert_id()}.
trigger_andon(Level, Category, Context) ->
    Alert = #{
        id => generate_alert_id(),
        level => Level,
        category => Category,
        work_order_id => maps:get(work_order_id, Context, undefined),
        agent => maps:get(agent, Context, undefined),
        message => format_alert_message(Category, Context),
        details => Context,
        triggered_at => calendar:universal_time()
    },

    %% Store alert
    ok = store_alert(Alert),

    %% Take action based on level
    case Level of
        stop_line ->
            halt_all_work(),
            notify_humans(Alert),
            broadcast_stop_signal();
        critical ->
            notify_humans(Alert),
            escalate_immediately(Alert);
        warning ->
            notify_dashboard(Alert),
            auto_attempt_recovery(Alert);
        info ->
            log_info(Alert)
    end,

    {ok, maps:get(id, Alert)}.

%% Halt all work (stop-the-line)
halt_all_work() ->
    %% Pause all agents
    Agents = get_all_active_agents(),
    lists:foreach(fun(Agent) ->
        gen_server:cast(Agent, pause_work)
    end, Agents),

    %% Mark all in-progress work orders as paused
    InProgressWOs = get_in_progress_work_orders(),
    lists:foreach(fun(WO) ->
        update_status(WO, paused)
    end, InProgressWOs),

    %% Set system state to stopped
    set_system_state(stopped),

    ok.

%% Resume after Andon resolution
-spec resolve_andon(AlertId :: alert_id(), Resolution :: binary()) -> ok.
resolve_andon(AlertId, Resolution) ->
    %% Update alert
    ok = update_alert(AlertId, #{
        resolved_at => calendar:universal_time(),
        resolution => Resolution
    }),

    %% Check if can resume
    case can_resume_work() of
        {ok, safe_to_resume} ->
            resume_all_work();
        {blocked, RemainingAlerts} ->
            {error, {still_blocked, RemainingAlerts}}
    end.

can_resume_work() ->
    %% Check no stop_line or critical alerts active
    ActiveAlerts = get_active_alerts(),
    StopLineAlerts = [A || A <- ActiveAlerts,
                          maps:get(level, A) =:= stop_line],
    CriticalAlerts = [A || A <- ActiveAlerts,
                          maps:get(level, A) =:= critical],

    case StopLineAlerts ++ CriticalAlerts of
        [] -> {ok, safe_to_resume};
        Remaining -> {blocked, Remaining}
    end.

resume_all_work() ->
    %% Resume all agents
    Agents = get_all_paused_agents(),
    lists:foreach(fun(Agent) ->
        gen_server:cast(Agent, resume_work)
    end, Agents),

    %% Resume all paused work orders
    PausedWOs = get_paused_work_orders(),
    lists:foreach(fun(WO) ->
        update_status(WO, in_progress)
    end, PausedWOs),

    %% Set system state to running
    set_system_state(running),

    ok.
```

#### Root Cause Analysis (5 Whys)

```erlang
%% Automated 5 Whys analysis
-spec analyze_root_cause(Alert :: andon_alert()) ->
    {ok, root_cause_analysis()}.
analyze_root_cause(Alert) ->
    Category = maps:get(category, Alert),
    Details = maps:get(details, Alert),

    %% Ask 5 Whys
    Why1 = ask_why(Category, Details, 1),
    Why2 = ask_why(Why1, Details, 2),
    Why3 = ask_why(Why2, Details, 3),
    Why4 = ask_why(Why3, Details, 4),
    Why5 = ask_why(Why4, Details, 5),

    %% Identify root cause
    RootCause = Why5,

    %% Suggest corrective actions
    Actions = suggest_corrective_actions(RootCause),

    Analysis = #{
        alert_id => maps:get(id, Alert),
        five_whys => [Why1, Why2, Why3, Why4, Why5],
        root_cause => RootCause,
        corrective_actions => Actions,
        analyzed_at => calendar:universal_time()
    },

    %% Store analysis
    ok = store_root_cause_analysis(Analysis),

    {ok, Analysis}.

ask_why(compilation_error, Details, _Level) ->
    <<"Code contains syntax error or type mismatch">>;
ask_why(<<"Code contains syntax error">>, Details, 2) ->
    <<"Agent wrote code without validating syntax">>;
ask_why(<<"Agent wrote code without validating">>, Details, 3) ->
    <<"No pre-write validation gate in place">>;
ask_why(<<"No pre-write validation gate">>, Details, 4) ->
    <<"Poka-yoke agent not integrated into write path">>;
ask_why(<<"Poka-yoke agent not integrated">>, Details, 5) ->
    <<"System design lacked error-proofing from start (root cause)">>.

suggest_corrective_actions(RootCause) ->
    %% Map root causes to actions
    case RootCause of
        <<"System design lacked error-proofing">> ->
            [
                <<"Integrate poka-yoke agent into all write operations">>,
                <<"Add syntax validation before file writes">>,
                <<"Implement jidoka quality gates after writes">>,
                <<"Train agents on error-proofing principles">>
            ];
        _ ->
            [<<"Manual investigation required">>]
    end.
```

### Human Escalation

#### Escalation Triggers
1. **Stop-the-Line Andon**: Immediate human notification
2. **Consensus Failed**: No 2/3 majority after voting deadline
3. **Deadlock Detected**: Circular dependencies or blocked resources
4. **Critical SLA Breach**: Security work order past 24h deadline
5. **Repeated Failures**: Same quality gate fails 3+ times

#### Escalation Protocol

```erlang
-type escalation() :: #{
    id := escalation_id(),
    priority := urgent | high | medium,
    reason := reason(),
    context := map(),
    escalated_at := calendar:datetime(),
    escalated_to := [contact()],
    acknowledged_at := calendar:datetime() | undefined,
    resolved_at := calendar:datetime() | undefined,
    resolution := binary() | undefined
}.

-type reason() ::
    andon_stop_line | consensus_failed | deadlock_detected |
    critical_sla_breach | repeated_failures | agent_failure.

-type contact() :: #{
    type := slack | email | sms | pagerduty,
    destination := binary()
}.

%% Escalate to humans
-spec escalate_to_human(Reason :: reason(), Context :: map()) ->
    {ok, escalation_id()}.
escalate_to_human(Reason, Context) ->
    %% Determine priority
    Priority = determine_priority(Reason),

    %% Determine contacts based on priority
    Contacts = get_escalation_contacts(Priority),

    Escalation = #{
        id => generate_escalation_id(),
        priority => Priority,
        reason => Reason,
        context => Context,
        escalated_at => calendar:universal_time(),
        escalated_to => Contacts
    },

    %% Store escalation
    ok = store_escalation(Escalation),

    %% Send notifications
    lists:foreach(fun(Contact) ->
        send_notification(Contact, Escalation)
    end, Contacts),

    %% Log escalation
    log_escalation(Escalation),

    {ok, maps:get(id, Escalation)}.

determine_priority(andon_stop_line) -> urgent;
determine_priority(critical_sla_breach) -> urgent;
determine_priority(deadlock_detected) -> high;
determine_priority(consensus_failed) -> high;
determine_priority(repeated_failures) -> medium;
determine_priority(_) -> medium.

get_escalation_contacts(urgent) ->
    [
        #{type => slack, destination => <<"#incidents">>},
        #{type => pagerduty, destination => <<"on-call-engineer">>},
        #{type => sms, destination => <<"team-lead-phone">>}
    ];
get_escalation_contacts(high) ->
    [
        #{type => slack, destination => <<"#incidents">>},
        #{type => email, destination => <<"team@example.com">>}
    ];
get_escalation_contacts(medium) ->
    [
        #{type => slack, destination => <<"#alerts">>}
    ].

send_notification(#{type := slack, destination := Channel}, Escalation) ->
    Message = format_slack_message(Escalation),
    slack_client:send_message(Channel, Message);

send_notification(#{type := email, destination := Email}, Escalation) ->
    Subject = format_email_subject(Escalation),
    Body = format_email_body(Escalation),
    email_client:send_email(Email, Subject, Body);

send_notification(#{type := sms, destination := Phone}, Escalation) ->
    Message = format_sms_message(Escalation),
    sms_client:send_sms(Phone, Message);

send_notification(#{type := pagerduty, destination := Service}, Escalation) ->
    Incident = format_pagerduty_incident(Escalation),
    pagerduty_client:trigger_incident(Service, Incident).
```

### Deadlock Detection and Resolution

```erlang
%% Detect deadlocks in dependency graph
-spec detect_deadlock() ->
    {ok, no_deadlock} | {deadlock, [work_order_id()]}.
detect_deadlock() ->
    %% Build dependency graph
    Graph = build_global_dependency_graph(),

    %% Check for cycles
    case digraph_utils:cyclic_strong_components(Graph) of
        [] -> {ok, no_deadlock};
        Cycles -> {deadlock, Cycles}
    end.

%% Resolve deadlock by breaking cycle
-spec resolve_deadlock(Cycle :: [work_order_id()]) ->
    {ok, resolved} | {error, manual_intervention_required}.
resolve_deadlock(Cycle) ->
    %% Strategy 1: Find lowest priority work order in cycle and cancel it
    Priorities = [{WO, get_priority(WO)} || WO <- Cycle],
    {LowestPriorityWO, _} = lists:min(Priorities),

    %% Cancel lowest priority work order
    ok = cancel_work_order(LowestPriorityWO, <<"Cancelled to resolve deadlock">>),

    %% Verify deadlock resolved
    case detect_deadlock() of
        {ok, no_deadlock} -> {ok, resolved};
        {deadlock, _} -> {error, manual_intervention_required}
    end.
```

---

## Coordination Tooling

### 1. Agent Supervisor

```erlang
-module(agent_coordinator_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% Child specs for all permanent agents
    ChildSpecs = [
        %% Orchestration layer
        agent_spec(sparc_orchestrator, permanent),

        %% Development layer
        agent_spec(erlang_otp_developer, permanent),
        agent_spec(erlang_transport_builder, permanent),
        agent_spec(build_engineer, permanent),

        %% Testing layer
        agent_spec(erlang_test_engineer, permanent),
        agent_spec(agent_06_test_eunit, transient),
        agent_spec(agent_07_test_ct, transient),
        agent_spec(agent_10_test_proper, transient),

        %% Quality layer
        agent_spec(agent_01_compile_gate, permanent),
        agent_spec(agent_11_coverage, permanent),
        agent_spec(agent_12_dialyzer, permanent),
        agent_spec(agent_13_xref, permanent),
        agent_spec(agent_14_format, permanent),

        %% Performance layer
        agent_spec(erlang_performance, permanent),
        agent_spec(agent_15_benchmark, transient),

        %% Review layer
        agent_spec(code_reviewer, permanent),

        %% Operations layer
        agent_spec(erlang_github_ops, permanent),
        agent_spec(verifier, permanent),

        %% Quality system layer
        agent_spec(agent_16_jidoka, permanent),
        agent_spec(agent_17_poka_yoke, permanent),
        agent_spec(agent_18_andon, permanent),
        agent_spec(agent_19_tcps, permanent),

        %% Dashboard
        agent_spec(agent_coordination_dashboard, permanent)
    ],

    {ok, {SupFlags, ChildSpecs}}.

agent_spec(AgentModule, Restart) ->
    #{
        id => AgentModule,
        start => {AgentModule, start_link, []},
        restart => Restart,
        shutdown => 5000,
        type => worker,
        modules => [AgentModule]
    }.
```

### 2. Work Order Manager

```erlang
-module(work_order_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_work_order/1,
    start_work_order/1,
    complete_work_order/2,
    get_work_order/1,
    get_queue/1,
    dequeue_next/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/3, handle_info/2]).

-record(state, {
    work_orders = #{} :: #{work_order_id() => work_order()},
    queues = #{} :: #{bucket() => [work_order_id()]},
    wip_counts = #{} :: #{bucket() => integer()},
    dependencies = digraph:graph()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Create ETS table for fast lookups
    ets:new(work_orders, [set, public, named_table, {read_concurrency, true}]),

    %% Create dependency graph
    DepsGraph = digraph:new([acyclic]),

    State = #state{
        queues = init_queues(),
        wip_counts = init_wip_counts(),
        dependencies = DepsGraph
    },

    {ok, State}.

handle_call({create_work_order, WorkOrder}, _From, State) ->
    %% Generate ID
    WoId = generate_work_order_id(),
    WorkOrderWithId = WorkOrder#{id => WoId},

    %% Store in ETS
    ets:insert(work_orders, {WoId, WorkOrderWithId}),

    %% Add to appropriate queue
    Bucket = maps:get(bucket, WorkOrderWithId),
    NewQueues = add_to_queue(Bucket, WoId, WorkOrderWithId, State#state.queues),

    %% Generate creation receipt
    {ok, _Receipt} = generate_receipt(WorkOrderWithId, created, system),

    %% Publish event
    publish_event({work_order_created, WoId, WorkOrderWithId}),

    {reply, {ok, WoId}, State#state{queues = NewQueues}};

handle_call({start_work_order, WoId}, _From, State) ->
    case ets:lookup(work_orders, WoId) of
        [{WoId, WorkOrder}] ->
            Bucket = maps:get(bucket, WorkOrder),

            %% Check WIP limit
            case check_wip_limit(Bucket, State) of
                {ok, capacity_available} ->
                    %% Check dependencies
                    case check_dependencies_resolved(WoId, State) of
                        {ok, resolved} ->
                            %% Start work order
                            UpdatedWO = WorkOrder#{
                                status => in_progress,
                                started_at => calendar:universal_time()
                            },
                            ets:insert(work_orders, {WoId, UpdatedWO}),

                            %% Increment WIP
                            NewWipCounts = increment_wip(Bucket, State#state.wip_counts),

                            %% Generate receipt
                            {ok, _Receipt} = generate_receipt(UpdatedWO, started, system),

                            %% Publish event
                            publish_event({work_order_started, WoId, system}),

                            {reply, ok, State#state{wip_counts = NewWipCounts}};
                        {blocked, BlockedBy} ->
                            {reply, {error, {blocked_by_dependencies, BlockedBy}}, State}
                    end;
                {error, wip_limit_exceeded} ->
                    {reply, {error, wip_limit_exceeded}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end.

%% ... additional handlers ...
```

### 3. Agent Communication Bus

```erlang
-module(agent_comm_bus).
-behaviour(gen_server).

%% Publish-subscribe message bus for agent coordination

-export([
    start_link/0,
    subscribe/2,
    unsubscribe/2,
    publish/2,
    request/3,
    reply/3
]).

-record(state, {
    subscriptions = #{} :: #{topic() => [pid()]},
    pending_requests = #{} :: #{request_id() => {pid(), reference()}}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Subscribe to topic
subscribe(Topic, Pid) ->
    gen_server:call(?MODULE, {subscribe, Topic, Pid}).

%% Unsubscribe from topic
unsubscribe(Topic, Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Topic, Pid}).

%% Publish message to topic (broadcast)
publish(Topic, Message) ->
    gen_server:cast(?MODULE, {publish, Topic, Message}).

%% Request-response (with timeout)
request(Topic, Message, Timeout) ->
    RequestId = make_ref(),
    gen_server:call(?MODULE, {request, Topic, Message, RequestId, self()}),

    receive
        {response, RequestId, Response} ->
            {ok, Response}
    after Timeout ->
        {error, timeout}
    end.

%% Reply to request
reply(RequestId, Response, ToPid) ->
    ToPid ! {response, RequestId, Response}.

%% gen_server callbacks
handle_call({subscribe, Topic, Pid}, _From, State) ->
    Subs = State#state.subscriptions,
    TopicSubs = maps:get(Topic, Subs, []),
    NewSubs = maps:put(Topic, [Pid | TopicSubs], Subs),

    %% Monitor subscriber
    erlang:monitor(process, Pid),

    {reply, ok, State#state{subscriptions = NewSubs}};

handle_call({request, Topic, Message, RequestId, FromPid}, _From, State) ->
    %% Store pending request
    Pending = State#state.pending_requests,
    MonitorRef = erlang:monitor(process, FromPid),
    NewPending = maps:put(RequestId, {FromPid, MonitorRef}, Pending),

    %% Publish request to topic subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    lists:foreach(fun(Sub) ->
        Sub ! {request, RequestId, Topic, Message, FromPid}
    end, Subscribers),

    {reply, ok, State#state{pending_requests = NewPending}}.

handle_cast({publish, Topic, Message}, State) ->
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    lists:foreach(fun(Sub) ->
        Sub ! {message, Topic, Message}
    end, Subscribers),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Remove dead subscriber from all topics
    NewSubs = maps:map(fun(_Topic, Subs) ->
        lists:delete(Pid, Subs)
    end, State#state.subscriptions),
    {noreply, State#state{subscriptions = NewSubs}}.
```

### 4. CLI Integration

```bash
#!/usr/bin/env bash
# Agent coordination CLI

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$1" in
    create)
        # Create new work order
        erl -noshell -eval "
            {ok, WoId} = work_order_manager:create_work_order(#{
                type => story,
                title => <<\"$2\">>,
                bucket => features,
                priority => 5
            }),
            io:format(\"Created work order: ~s~n\", [WoId]),
            halt(0).
        "
        ;;

    start)
        # Start work order
        erl -noshell -eval "
            ok = work_order_manager:start_work_order(<<\"$2\">>),
            io:format(\"Started work order: ~s~n\", [<<\"$2\">>]),
            halt(0).
        "
        ;;

    status)
        # Get work order status
        erl -noshell -eval "
            {ok, Status} = work_order_manager:get_work_order_status(<<\"$2\">>),
            io:format(\"~p~n\", [Status]),
            halt(0).
        "
        ;;

    dashboard)
        # Launch web dashboard
        erl -noshell -eval "
            {ok, _} = agent_coordination_dashboard:start_link(),
            io:format(\"Dashboard running at http://localhost:8080~n\"),
            receive _ -> ok end.
        "
        ;;

    *)
        echo "Usage: $0 {create|start|status|dashboard} [args]"
        exit 1
        ;;
esac
```

---

## Implementation Protocols

### Agent Spawning Protocol

```javascript
// GOLDEN RULE: Spawn all agents in ONE message

// Epic-level coordination (full SPARC workflow)
Task("Research Patterns", "Analyze codebase for subscription patterns", "erlang-researcher")
Task("Design Specification", "Define subscription protocol requirements", "plan-designer")
Task("Create Architecture", "Design supervision tree and behaviors", "erlang-architect")
Task("Implement Core", "Build subscription gen_server", "erlang-otp-developer")
Task("Build Transports", "Integrate subscription with transports", "erlang-transport-builder")
Task("Write Tests", "Comprehensive EUnit + CT tests", "erlang-test-engineer")
Task("Benchmark Performance", "Validate subscription throughput", "erlang-performance")
Task("Review Code", "OTP compliance and quality review", "code-reviewer")
Task("Create PR", "Generate PR with receipt chain", "erlang-github-ops")

// Parallel quality gates
Task("Compile Gate", "Validate compilation", "agent-01-compile-gate")
Task("Run EUnit", "Execute unit tests", "agent-06-test-eunit")
Task("Run CT", "Execute integration tests", "agent-07-test-ct")
Task("Check Coverage", "Validate 80% coverage", "agent-11-coverage")
Task("Run Dialyzer", "Type checking", "agent-12-dialyzer")
Task("Run Xref", "Cross-reference analysis", "agent-13-xref")
Task("Format Code", "Apply code formatting", "agent-14-format")
Task("Run Benchmarks", "Performance validation", "agent-15-benchmark")

// Quality system
Task("Jidoka Monitor", "Built-in quality checks", "agent-16-jidoka")
Task("Poka-Yoke Validate", "Error prevention checks", "agent-17-poka-yoke")
Task("Andon Monitor", "Quality alert monitoring", "agent-18-andon")
Task("TCPS Coordination", "TPS quality system coordination", "agent-19-tcps")
```

### Work Order Lifecycle Protocol

```erlang
%% Complete work order lifecycle with all hooks

%% 1. Create work order
{ok, WoId} = work_order_manager:create_work_order(#{
    type => task,
    title => <<"Implement subscription gen_server">>,
    description => <<"Build erlmcp_subscription gen_server with OTP patterns">>,
    bucket => features,
    priority => 6,
    assigned_agents => [erlang_otp_developer],
    estimated_hours => 6.0,
    acceptance_criteria => [
        <<"init/1 non-blocking">>,
        <<"handle_call for subscribe/unsubscribe">>,
        <<"handle_info for notifications">>,
        <<"proper supervision">>,
        <<"80%+ test coverage">>
    ]
}),

%% 2. Add dependencies
ok = work_order_manager:add_dependency(WoId, ArchitectureWoId),

%% 3. Wait for dependencies to resolve
case work_order_manager:can_start(WoId) of
    {ok, ready} ->
        %% 4. Start work order
        ok = work_order_manager:start_work_order(WoId),

        %% 5. Progress through stages
        ok = work_order_manager:progress_work_order(WoId, design),
        ok = work_order_manager:progress_work_order(WoId, implementation),
        ok = work_order_manager:progress_work_order(WoId, testing),

        %% 6. Run quality gates
        ok = run_quality_gates(WoId),

        %% 7. Complete work order
        SkuId = <<"sku-subscription-gen-server">>,
        ok = work_order_manager:complete_work_order(WoId, SkuId),

        %% 8. Verify receipt chain
        {ok, valid} = verify_receipt_chain(WoId);
    {blocked, Deps} ->
        io:format("Blocked by: ~p~n", [Deps])
end.
```

---

## Quality Gates and Governance

### Quality Gate Definitions

```erlang
-type quality_gate() :: #{
    name := gate_name(),
    type := blocking | warning | info,
    checker_agent := agent_role(),
    check_function := fun((work_order()) -> gate_result()),
    auto_fix := boolean(),
    retry_count := integer(),
    timeout_ms := integer()
}.

-type gate_result() ::
    {pass, Details :: map()} |
    {fail, Reason :: binary(), Details :: map()} |
    {warning, Reason :: binary(), Details :: map()}.

%% Define all quality gates
quality_gates() ->
    [
        %% Compilation gate (BLOCKING)
        #{
            name => compilation,
            type => blocking,
            checker_agent => agent_01_compile_gate,
            check_function => fun check_compilation/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 30000
        },

        %% Test gate (BLOCKING)
        #{
            name => tests,
            type => blocking,
            checker_agent => verifier,
            check_function => fun check_tests/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 180000
        },

        %% Coverage gate (WARNING at <80%, BLOCKING at <70%)
        #{
            name => coverage,
            type => warning,
            checker_agent => agent_11_coverage,
            check_function => fun check_coverage/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 30000
        },

        %% Dialyzer gate (WARNING)
        #{
            name => dialyzer,
            type => warning,
            checker_agent => agent_12_dialyzer,
            check_function => fun check_dialyzer/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 90000
        },

        %% Xref gate (BLOCKING on undefined calls)
        #{
            name => xref,
            type => blocking,
            checker_agent => agent_13_xref,
            check_function => fun check_xref/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 30000
        },

        %% Format gate (AUTO-FIX)
        #{
            name => format,
            type => info,
            checker_agent => agent_14_format,
            check_function => fun check_format/1,
            auto_fix => true,
            retry_count => 1,
            timeout_ms => 10000
        },

        %% Benchmark gate (WARNING at >10% regression)
        #{
            name => benchmark,
            type => warning,
            checker_agent => agent_15_benchmark,
            check_function => fun check_benchmarks/1,
            auto_fix => false,
            retry_count => 0,
            timeout_ms => 300000
        }
    ].

%% Run all quality gates for work order
-spec run_quality_gates(WorkOrderId :: work_order_id()) ->
    {ok, all_passed} | {failed, [gate_result()]}.
run_quality_gates(WorkOrderId) ->
    Gates = quality_gates(),
    Results = run_gates_sequential(WorkOrderId, Gates),

    %% Check for blocking failures
    BlockingFailures = [R || R <- Results,
                            is_blocking_failure(R)],

    case BlockingFailures of
        [] -> {ok, all_passed};
        Failures -> {failed, Failures}
    end.

run_gates_sequential(WorkOrderId, Gates) ->
    lists:foldl(fun(Gate, Acc) ->
        Result = run_single_gate(WorkOrderId, Gate),
        [Result | Acc]
    end, [], Gates).

run_single_gate(WorkOrderId, Gate) ->
    CheckFun = maps:get(check_function, Gate),
    Timeout = maps:get(timeout_ms, Gate),

    try
        Result = CheckFun(WorkOrderId),

        %% Auto-fix if configured
        case {Result, maps:get(auto_fix, Gate)} of
            {{fail, _, _}, true} ->
                attempt_auto_fix(WorkOrderId, Gate);
            _ ->
                Result
        end
    catch
        Error:Reason:Stacktrace ->
            {fail, <<"Gate execution error">>, #{
                error => Error,
                reason => Reason,
                stacktrace => Stacktrace
            }}
    end.
```

### Governance Rules

```erlang
%% Governance rules enforced by coordination system

-define(GOVERNANCE_RULES, [
    %% Rule 1: No work proceeds until compilation succeeds
    {compilation_gate, blocking, always},

    %% Rule 2: Tests must pass before completion
    {test_gate, blocking, before_completion},

    %% Rule 3: Coverage must be ≥80% before completion
    {coverage_gate, warning, before_completion},

    %% Rule 4: Code must be reviewed before merge
    {code_review, blocking, before_merge},

    %% Rule 5: Never rebase (merge only)
    {git_rebase, forbidden, always},

    %% Rule 6: Never skip hooks (no --no-verify)
    {git_no_verify, forbidden, always},

    %% Rule 7: All work orders must have receipts
    {receipt_generation, required, on_completion},

    %% Rule 8: SLA breaches trigger Andon
    {sla_monitoring, required, always},

    %% Rule 9: Quality gates cannot be bypassed
    {quality_gate_bypass, forbidden, always},

    %% Rule 10: Circular dependencies are rejected
    {circular_dependency, forbidden, on_add_dependency}
]).

%% Enforce governance rule
-spec enforce_rule(Rule :: atom(), Context :: map()) ->
    {ok, allowed} | {error, {violation, Reason :: binary()}}.
enforce_rule(git_rebase, #{operation := rebase}) ->
    {error, {violation, <<"Git rebase is forbidden - use merge only">>}};

enforce_rule(git_no_verify, #{flags := Flags}) ->
    case lists:member("--no-verify", Flags) of
        true -> {error, {violation, <<"--no-verify flag is forbidden">>}};
        false -> {ok, allowed}
    end;

enforce_rule(quality_gate_bypass, #{bypass := true}) ->
    {error, {violation, <<"Quality gates cannot be bypassed">>}};

enforce_rule(circular_dependency, #{dependency_graph := Graph, new_dep := {From, To}}) ->
    case detect_circular_dependency(Graph, {From, To}) of
        ok -> {ok, allowed};
        {error, {circular_dependency, Cycle}} ->
            {error, {violation, io_lib:format("Circular dependency detected: ~p", [Cycle])}}
    end;

enforce_rule(_Rule, _Context) ->
    {ok, allowed}.
```

---

## Performance and Scaling

### Performance Targets

```erlang
-define(PERFORMANCE_TARGETS, #{
    %% Work order throughput
    work_orders_per_hour => 20,  % Completed work orders per hour

    %% Agent response time
    agent_spawn_time_ms => 100,  % Time to spawn new agent
    agent_response_time_ms => 1000,  % Time for agent to respond

    %% Coordination overhead
    coordination_overhead_percent => 10,  % Max 10% overhead

    %% Dependency resolution
    dependency_check_time_ms => 50,  % Time to check dependencies

    %% Quality gate execution
    compilation_time_s => 30,  % Max compilation time
    test_execution_time_s => 180,  % Max test execution time

    %% Message passing
    message_latency_ms => 10,  % Max message passing latency

    %% Dashboard updates
    dashboard_update_latency_ms => 100  % Max dashboard update latency
}).
```

### Scaling Strategies

#### Horizontal Scaling (Multiple Nodes)
```erlang
%% Distribute work orders across Erlang nodes
-spec distribute_work_order(WorkOrder :: work_order()) ->
    {ok, node()}.
distribute_work_order(WorkOrder) ->
    %% Select node based on bucket or load
    Bucket = maps:get(bucket, WorkOrder),
    Node = select_node_for_bucket(Bucket),

    %% Spawn remote agent
    spawn(Node, fun() ->
        execute_work_order(WorkOrder)
    end),

    {ok, Node}.

select_node_for_bucket(Bucket) ->
    Nodes = [node() | nodes()],
    LoadByNode = [{N, get_node_load(N)} || N <- Nodes],
    {LeastLoadedNode, _} = lists:min(LoadByNode),
    LeastLoadedNode.
```

#### Vertical Scaling (Process Pools)
```erlang
%% Agent process pool for parallel execution
-module(agent_pool).

start_pool(AgentModule, PoolSize) ->
    poolboy:start_link([
        {name, {local, AgentModule}},
        {worker_module, AgentModule},
        {size, PoolSize},
        {max_overflow, 10}
    ]).

execute_in_pool(AgentModule, WorkOrder) ->
    poolboy:transaction(AgentModule, fun(Worker) ->
        gen_server:call(Worker, {execute, WorkOrder})
    end).
```

#### Caching Strategy
```erlang
%% Cache frequently accessed work order data
-module(work_order_cache).

-define(CACHE_TTL, 300000).  % 5 minutes

get_work_order(WoId) ->
    case ets:lookup(work_order_cache, WoId) of
        [{WoId, WorkOrder, Timestamp}] ->
            case is_cache_valid(Timestamp) of
                true -> {ok, WorkOrder};
                false ->
                    %% Cache expired, fetch fresh
                    fetch_and_cache(WoId)
            end;
        [] ->
            fetch_and_cache(WoId)
    end.

fetch_and_cache(WoId) ->
    {ok, WorkOrder} = work_order_manager:get_work_order(WoId),
    ets:insert(work_order_cache, {WoId, WorkOrder, erlang:system_time(millisecond)}),
    {ok, WorkOrder}.

is_cache_valid(Timestamp) ->
    Now = erlang:system_time(millisecond),
    (Now - Timestamp) < ?CACHE_TTL.
```

---

## Conclusion

This agent coordination workflow design provides a comprehensive framework for implementing the MCP protocol in erlmcp using claude-flow principles, SPARC methodology, and TPS quality concepts.

### Key Features

1. **20 Specialized Agents**: Each with clear roles and responsibilities
2. **4-Level Work Decomposition**: Epic → Story → Task → Subtask
3. **Sophisticated Dependency Management**: Sequential, parallel, data, and resource dependencies
4. **Erlang-Native Communication**: Message passing, ETS, gproc registry
5. **Democratic Consensus**: 2/3 majority voting on design decisions
6. **Real-Time Progress Tracking**: Dashboard, receipts, metrics
7. **Automatic Escalation**: Andon system with human escalation
8. **Quality Built-In**: Jidoka, Poka-Yoke, automated gates
9. **High Performance**: Targets 20 work orders/hour with <10% overhead
10. **Horizontal Scalability**: Distributed Erlang support

### Next Steps

1. Implement core coordination modules (agent_coordinator_sup, work_order_manager)
2. Build communication bus (agent_comm_bus)
3. Create quality gate framework
4. Implement dashboard and reporting
5. Build CLI tooling
6. Write comprehensive tests
7. Deploy and monitor performance

---

**Document Status**: Ready for Implementation
**Approval Required**: Architecture Review Board
**Implementation Priority**: Phase 1 (Foundation)
