# Governance System for erlmcp

**Version**: 3.0.0
**Last Updated**: 2026-02-01
**Status**: Production

---

## Table of Contents

- [Quick Start](#quick-start)
- [Core Concepts](#core-concepts)
- [Work Order Specification](#work-order-specification)
- [Agent Reference](#agent-reference)
- [Submitting Work Orders](#submitting-work-orders)
- [Kanban Limits & Single-Tasking](#kanban-limits--single-tasking)
- [Dependency Resolution](#dependency-resolution)
- [File-Level Locking](#file-level-locking)
- [Multi-Agent Orchestration](#multi-agent-orchestration)
- [Receipts & Audit Trail](#receipts--audit-trail)
- [Status Tracking](#status-tracking)
- [Error Recovery](#error-recovery)
- [Best Practices](#best-practices)
- [Common Workflows](#common-workflows)
- [Troubleshooting](#troubleshooting)
- [FAQ](#faq)

---

## Quick Start

The erlmcp governance system enables **autonomous multi-agent development** with deterministic quality gates, audit trails, and Armstrong-style reliability principles.

### What is the Governance System?

The governance system coordinates work between specialized Erlang/OTP development agents using a **work order protocol**. Each agent has specific capabilities, constraints, and a single-tasking model to maximize quality.

### Submit Your First Work Order

```bash
# Create a simple work order (manual method)
cat > .erlmcp/work-orders/queue.json <<EOF
{
  "id": "wo-001",
  "task": "Add EUnit tests for erlmcp_client module",
  "agent": "erlang-test-engineer",
  "priority": "high",
  "dependencies": [],
  "constraints": {
    "time_budget": 1800,
    "cost_budget": 0.05,
    "files": ["apps/erlmcp_core/test/erlmcp_client_tests.erl"],
    "branch": "claude/add-client-tests-wo001"
  }
}
EOF

# Agent will pick up automatically when available
```

### Check Status

```bash
# View active work orders
cat .erlmcp/work-orders/wip.json

# View completed work orders
cat .erlmcp/work-orders/completed.json

# View recent receipts
ls -lt .erlmcp/receipts/ | head -5
```

### Simple Example

```erlang
%% Request a simple task via conversational interface
%% "Please add validation for MCP request IDs in erlmcp_json_rpc"

%% System creates work order internally:
#{
    id => <<"wo-validation-001">>,
    task => <<"Add request ID validation to erlmcp_json_rpc">>,
    agent => 'erlang-otp-developer',
    priority => normal,
    dependencies => [],
    constraints => #{
        time_budget => 600,
        files => [<<"apps/erlmcp_core/src/erlmcp_json_rpc.erl">>,
                  <<"apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl">>]
    }
}

%% Agent completes task:
%% 1. Acquires file locks
%% 2. Implements validation logic
%% 3. Writes tests (Chicago TDD)
%% 4. Runs quality gates
%% 5. Generates receipt
%% 6. Reports completion
```

---

## Core Concepts

### Work Orders

A **work order** is a formal specification of a development task with:
- **Unique ID**: Immutable work order identifier
- **Task Description**: Clear, actionable objective
- **Agent Assignment**: Which specialized agent executes
- **Priority**: Execution ordering (high/normal/low)
- **Dependencies**: Other work orders that must complete first
- **Constraints**: Time, cost, files, branch constraints
- **Status**: Current lifecycle state (queued → wip → done/failed)

### Agents

**Specialized autonomous developers** with specific capabilities:

| Agent | Domain | Capabilities |
|-------|--------|--------------|
| **erlang-otp-developer** | OTP implementation | gen_server, supervisor, OTP behaviors |
| **erlang-architect** | System design | Supervision trees, architecture decisions |
| **erlang-test-engineer** | Testing | EUnit, CT, PropEr, Chicago TDD |
| **erlang-researcher** | Exploration | Codebase analysis, pattern discovery |
| **erlang-performance** | Optimization | Benchmarking, profiling, hot path analysis |
| **erlang-github-ops** | Git/CI/CD | Branches, PRs, releases, workflows |
| **code-reviewer** | Quality | Code review, OTP compliance, security |
| **erlang-transport-builder** | Transports | MCP transport implementations |
| **release-scout** | Dependencies | Version monitoring, security advisories |
| **plan-designer** | Planning | Implementation planning, roadmap design |
| **verifier** | Validation | Quality gates, test validation, coverage |

### Kanban Limits

**Single-tasking constraint**: ∀agent. |WIP(agent)| ≤ 1

- Each agent works on exactly ONE task at a time
- Prevents context switching
- Maximizes quality and completion speed
- New tasks queue when agent busy

### Branches and State Management

Each work order executes on a unique branch:

```bash
# Branch naming convention
claude/<task-description>-<session-id>

# Example
claude/add-validation-logic-wo001
```

Branches are:
- **Auto-created**: System creates branch on work order start
- **Isolated**: No interference between work orders
- **Rebased**: Automatically rebased onto main when complete
- **Cleaned**: Deleted after successful merge

### Receipts and Audit Trails

Every work order generates a **deterministic receipt**:

```json
{
  "work_order_id": "wo-001",
  "agent": "erlang-otp-developer",
  "task": "Add validation logic",
  "start_time": "2026-02-01T10:00:00Z",
  "end_time": "2026-02-01T10:15:23Z",
  "status": "completed",
  "quality_gates": {
    "compile": "pass",
    "eunit": "pass",
    "dialyzer": "pass",
    "coverage": "85%"
  },
  "artifacts": [
    "apps/erlmcp_core/src/erlmcp_json_rpc.erl",
    "apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl"
  ],
  "duration_seconds": 923,
  "cost_usd": 0.038
}
```

Receipts provide:
- **Proof of completion**
- **Quality verification**
- **Cost accounting**
- **Audit trail**

---

## Work Order Specification

### Erlang Type Definition

```erlang
-type work_order_id() :: binary().
-type agent_role() :: erlang-otp-developer | erlang-architect |
                      erlang-test-engineer | erlang-researcher |
                      erlang-performance | erlang-github-ops |
                      code-reviewer | erlang-transport-builder |
                      release-scout | plan-designer | verifier.
-type priority() :: high | normal | low.
-type status() :: queued | wip | done | failed.

-type work_order() :: #{
    id := work_order_id(),                    % Unique identifier
    task := binary(),                         % "Implement X" | "Test Y" | "Review Z"
    agent := agent_role(),                    % Target agent from table above
    priority := priority(),                   % Execution priority
    dependencies := [work_order_id()],        % Must complete before this task
    constraints := #{
        time_budget => pos_integer(),         % Seconds
        cost_budget => float(),               % USD
        files => [file_path()],               % File-level locks
        branch => binary()                    % Auto-managed branch name
    },
    status := status(),
    result => #{                              % Written on completion
        exit_code => integer(),
        gates => gate_results(),
        artifacts => [file_path()],
        duration => pos_integer()             % Seconds
    }
}.

-type gate_results() :: #{
    compile := pass | fail,
    eunit := pass | fail,
    ct := pass | fail,
    dialyzer := pass | fail,
    coverage := {pass, float()} | {fail, float()}
}.
```

### Required Fields

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| **id** | binary() | Unique work order ID | `"wo-001"` |
| **task** | binary() | Clear task description | `"Add request ID validation"` |
| **agent** | agent_role() | Target agent | `erlang-otp-developer` |
| **priority** | priority() | Execution priority | `high`, `normal`, `low` |
| **dependencies** | [work_order_id()] | Prerequisite work orders | `["wo-000"]` or `[]` |
| **constraints** | map() | Execution constraints | See below |
| **status** | status() | Current state | `queued`, `wip`, `done`, `failed` |

### Constraints

```erlang
#{
    time_budget => 1800,           % Maximum 30 minutes
    cost_budget => 0.10,           % Maximum $0.10 cloud compute
    files => [                     % Files this work order modifies
        <<"apps/erlmcp_core/src/erlmcp_client.erl">>,
        <<"apps/erlmcp_core/test/erlmcp_client_tests.erl">>
    ],
    branch => <<"claude/add-validation-wo001">>  % Auto-generated
}
```

### Status Transitions

```
queued → wip → done
              ↘ failed
```

- **queued**: Waiting for agent availability or dependencies
- **wip**: Agent actively working on task
- **done**: Successfully completed, all quality gates passed
- **failed**: Quality gates failed or error occurred

### Result Structure

Populated when work order transitions to `done` or `failed`:

```erlang
#{
    exit_code => 0,                           % 0 = success, >0 = failure
    gates => #{
        compile => pass,
        eunit => pass,
        ct => pass,
        dialyzer => pass,
        coverage => {pass, 0.87}
    },
    artifacts => [                            % Files created/modified
        <<"apps/erlmcp_core/src/erlmcp_json_rpc.erl">>,
        <<"apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl">>
    ],
    duration => 923                           % Seconds elapsed
}
```

---

## Agent Reference

### Agent Capabilities Matrix

| Agent | Read | Write | Compile | Test | Review | Network | OTP Expert |
|-------|------|-------|---------|------|--------|---------|------------|
| erlang-otp-developer | ✅ | ✅ | ✅ | ✅ | ⚠️ | ❌ | ✅ |
| erlang-architect | ✅ | ✅ | ✅ | ⚠️ | ✅ | ❌ | ✅ |
| erlang-test-engineer | ✅ | ✅ | ✅ | ✅ | ⚠️ | ❌ | ✅ |
| erlang-researcher | ✅ | ❌ | ❌ | ❌ | ⚠️ | ✅ | ✅ |
| erlang-performance | ✅ | ✅ | ✅ | ✅ | ⚠️ | ❌ | ✅ |
| erlang-github-ops | ✅ | ⚠️ | ⚠️ | ⚠️ | ❌ | ✅ | ⚠️ |
| code-reviewer | ✅ | ❌ | ✅ | ❌ | ✅ | ❌ | ✅ |
| erlang-transport-builder | ✅ | ✅ | ✅ | ✅ | ⚠️ | ✅ | ✅ |
| release-scout | ✅ | ❌ | ❌ | ❌ | ⚠️ | ✅ | ⚠️ |
| plan-designer | ✅ | ✅ | ❌ | ❌ | ✅ | ❌ | ✅ |
| verifier | ✅ | ❌ | ✅ | ✅ | ✅ | ❌ | ✅ |

**Legend**: ✅ Full capability | ⚠️ Limited/advisory | ❌ Not permitted

### Detailed Agent Descriptions

#### erlang-otp-developer

**Domain**: OTP implementation, gen_server, supervisor, behaviors

**Capabilities**:
- Implement gen_server, gen_statem, gen_event modules
- Write supervision trees
- Create OTP behaviors
- Follow Chicago TDD (no mocks)
- Write EUnit and Common Test suites

**Constraints**:
- Must follow OTP design patterns
- init/1 must not block (use handle_continue)
- All spawns must be supervised
- Coverage ≥80% required

**Example Tasks**:
- "Implement gen_server cache with ETS backend"
- "Add supervision tree for transport pool"
- "Create rate limiter using gen_statem"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### erlang-architect

**Domain**: System design, supervision hierarchies, architecture decisions

**Capabilities**:
- Design supervision trees (3-tier architecture)
- Make architectural decisions (with rationale)
- Create high-level design documents
- Review system-wide patterns
- Plan refactoring strategies

**Constraints**:
- Must document design decisions
- Must follow Armstrong principles
- Must consider failure modes
- Must design for observability

**Example Tasks**:
- "Design supervision tree for distributed session manager"
- "Architect fault-tolerant transport layer"
- "Plan migration from ETS to Mnesia for sessions"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### erlang-test-engineer

**Domain**: EUnit, Common Test, PropEr, Chicago TDD

**Capabilities**:
- Write comprehensive test suites
- EUnit (unit tests)
- Common Test (integration tests)
- PropEr (property-based tests)
- Test real processes (no mocks)
- Coverage analysis

**Constraints**:
- Chicago TDD mandatory (no mocks/fakes/stubs)
- Tests must use real gen_servers
- Coverage target ≥85%
- Black-box testing (test behavior, not implementation)

**Example Tasks**:
- "Write EUnit tests for erlmcp_json_rpc validation"
- "Add Common Test suite for HTTP transport"
- "Create PropEr tests for message serialization"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### erlang-researcher

**Domain**: Codebase exploration, pattern analysis

**Capabilities**:
- Read and analyze codebase
- Find patterns and anti-patterns
- Identify dependencies
- Locate files and modules
- Analyze call graphs
- Document existing architecture

**Constraints**:
- Read-only (no code modifications)
- Network access for documentation
- Limited to analysis and reporting

**Example Tasks**:
- "Analyze supervision tree structure in erlmcp_core"
- "Find all gen_server modules using gproc registry"
- "Document message flow in MCP protocol implementation"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### erlang-performance

**Domain**: Benchmarking, profiling, optimization

**Capabilities**:
- Write benchmark suites
- Profile code (fprof, eprof, recon)
- Analyze hot paths
- Optimize performance-critical code
- Measure throughput and latency
- Generate performance reports

**Constraints**:
- Must preserve correctness
- Must maintain test coverage
- Must benchmark before/after
- Regression threshold <10%

**Example Tasks**:
- "Profile erlmcp_registry message passing performance"
- "Optimize JSON encoding using native json module"
- "Benchmark HTTP transport under 10K concurrent connections"

**Cloud-Ready**: ⚠️ Benchmarks prefer local hardware (consistent results)

---

#### erlang-github-ops

**Domain**: Git, GitHub, CI/CD, releases

**Capabilities**:
- Create branches and PRs
- Manage GitHub workflows
- Tag releases
- Update CHANGELOG
- Trigger CI/CD pipelines
- Resolve merge conflicts (simple cases)

**Constraints**:
- Never force push to main
- Never skip git hooks
- Always run quality gates before PR
- Follow semantic versioning

**Example Tasks**:
- "Create PR for work order wo-001 completion"
- "Tag release v2.2.0 with CHANGELOG updates"
- "Update CI workflow to test OTP 29"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### code-reviewer

**Domain**: Code quality, OTP compliance, security

**Capabilities**:
- Review code for OTP compliance
- Check Armstrong principles adherence
- Identify security issues
- Verify test coverage
- Check documentation
- Approve/reject PRs

**Constraints**:
- Read-only (cannot modify code)
- Must provide constructive feedback
- Must cite specific violations
- Must check all quality gates

**Example Tasks**:
- "Review PR #42 for OTP compliance"
- "Check security of authentication module"
- "Verify test coverage meets 85% threshold"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### erlang-transport-builder

**Domain**: MCP transport implementations

**Capabilities**:
- Implement transport behaviors
- Write STDIO, HTTP, TCP, WebSocket, SSE transports
- Handle connection lifecycle
- Implement framing protocols
- Write transport-specific tests

**Constraints**:
- Must implement transport behavior contract
- Must handle all MCP message types
- Must support connection pooling
- Coverage ≥85%

**Example Tasks**:
- "Implement WebSocket transport with text frame encoding"
- "Add connection pooling to HTTP transport"
- "Create SSE transport with Last-Event-ID support"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### release-scout

**Domain**: Dependency monitoring, security advisories

**Capabilities**:
- Monitor dependency versions
- Check for security advisories (CVEs)
- Recommend upgrades
- Assess breaking changes
- Generate upgrade reports

**Constraints**:
- Read-only (reports only, no upgrades)
- Must check hex.pm and erlang-solutions.com
- Must assess compatibility

**Example Tasks**:
- "Check for security updates in dependencies"
- "Assess impact of upgrading Cowboy 2.10 → 2.11"
- "Monitor for OTP 29 RC releases"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### plan-designer

**Domain**: Implementation planning, roadmap design

**Capabilities**:
- Analyze requirements
- Design implementation approach
- Create step-by-step plans
- Identify agent delegations
- Make architectural decisions
- Define testing strategy

**Constraints**:
- Must follow Research → Plan → Execute workflow
- Must document rationale
- Must identify risks
- Must estimate effort

**Example Tasks**:
- "Design implementation plan for distributed session manager"
- "Plan migration from jsx to native json module"
- "Create roadmap for OTP 29 upgrade"

**Cloud-Ready**: ✅ Full cloud execution support

---

#### verifier

**Domain**: Quality gates, test validation, coverage

**Capabilities**:
- Run quality gate suite
- Validate test results
- Check coverage thresholds
- Verify compilation
- Run dialyzer and xref
- Generate quality reports

**Constraints**:
- Read-only (cannot modify code)
- Must run all gates (compile, test, coverage, dialyzer, xref)
- Must report failures with details

**Example Tasks**:
- "Verify all quality gates pass before PR merge"
- "Check coverage is ≥85% for new modules"
- "Validate no regressions in benchmark suite"

**Cloud-Ready**: ✅ Full cloud execution support

---

## Submitting Work Orders

### Method 1: Direct JSON (Manual)

Create work order JSON in `.erlmcp/work-orders/queue.json`:

```json
{
  "id": "wo-002",
  "task": "Add circuit breaker to HTTP client",
  "agent": "erlang-otp-developer",
  "priority": "high",
  "dependencies": [],
  "constraints": {
    "time_budget": 3600,
    "cost_budget": 0.15,
    "files": [
      "apps/erlmcp_transports/src/erlmcp_transport_http.erl",
      "apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl"
    ],
    "branch": "claude/add-circuit-breaker-wo002"
  },
  "status": "queued"
}
```

Agent picks up automatically when:
1. Agent is available (WIP limit = 0)
2. All dependencies complete
3. No file locks conflict

### Method 2: Conversational (Automatic)

Simply describe the task:

```
"Please add validation for MCP message sizes in erlmcp_message_size module,
ensuring messages >10MB are rejected with error code 1042"
```

System automatically:
1. Creates work order with appropriate agent
2. Sets priority based on task type
3. Identifies file dependencies
4. Queues for execution

### Method 3: Multi-Agent Orchestration

For complex features requiring multiple agents:

```json
{
  "work_orders": [
    {
      "id": "wo-010",
      "task": "Design distributed session architecture",
      "agent": "erlang-architect",
      "priority": "high",
      "dependencies": []
    },
    {
      "id": "wo-011",
      "task": "Implement distributed session manager",
      "agent": "erlang-otp-developer",
      "priority": "high",
      "dependencies": ["wo-010"]
    },
    {
      "id": "wo-012",
      "task": "Write tests for distributed sessions",
      "agent": "erlang-test-engineer",
      "priority": "high",
      "dependencies": ["wo-011"]
    },
    {
      "id": "wo-013",
      "task": "Review session manager implementation",
      "agent": "code-reviewer",
      "priority": "normal",
      "dependencies": ["wo-011", "wo-012"]
    },
    {
      "id": "wo-014",
      "task": "Create PR with quality gates",
      "agent": "erlang-github-ops",
      "priority": "normal",
      "dependencies": ["wo-013"]
    }
  ]
}
```

**Execution Order**: wo-010 → wo-011 → {wo-012, wo-013 parallel} → wo-014

### Specifying Dependencies

Dependencies ensure correct execution order:

```erlang
%% Work order B depends on work order A completing first
#{
    id => <<"wo-b">>,
    dependencies => [<<"wo-a">>],
    ...
}

%% Work order D depends on both B and C
#{
    id => <<"wo-d">>,
    dependencies => [<<"wo-b">>, <<"wo-c">>],
    ...
}
```

**Dependency Resolution Algorithm**:
1. Build dependency graph
2. Topological sort
3. Execute independent work orders in parallel
4. Wait for dependencies before starting dependent tasks

---

## Kanban Limits & Single-Tasking

### Why Single-Tasking?

**Kanban Principle**: ∀agent. |WIP(agent)| ≤ 1

**Benefits**:
- **Higher Quality**: Full focus on one task eliminates context switching
- **Faster Completion**: Single task completes faster than multiple partial tasks
- **Clearer State**: Always know what agent is working on
- **Simpler Debugging**: Fewer concurrent state changes

### How It Works

```erlang
%% Agent picks up work order from queue
acquire_work_order(Agent) ->
    case get_agent_wip_count(Agent) of
        0 ->
            %% Agent available
            {ok, WorkOrder} = dequeue_next(Agent),
            set_agent_wip(Agent, WorkOrder),
            {ok, WorkOrder};
        1 ->
            %% Agent busy
            {error, agent_busy}
    end.
```

### Consequence: Work Orders Queue

When agent is busy:
- New work orders queue in priority order
- Agent picks up next work order after completing current task
- Higher priority work orders move to front of queue

### Priority Queue Ordering

```
Priority: HIGH > NORMAL > LOW

Queue State:
┌─────────────────────────────────┐
│ HIGH:   wo-005 → wo-007         │
│ NORMAL: wo-003 → wo-009 → wo-012│
│ LOW:    wo-001 → wo-004         │
└─────────────────────────────────┘

Agent picks: wo-005 (highest priority)
```

### Checking Agent Availability

```bash
# View agent WIP status
cat .erlmcp/work-orders/wip.json | jq '.[] | {agent, task}'

# Example output:
# {
#   "agent": "erlang-otp-developer",
#   "task": "Implement circuit breaker"
# }
# {
#   "agent": "erlang-test-engineer",
#   "task": "Write PropEr tests"
# }
```

---

## Dependency Resolution

### Declaring Dependencies

```erlang
%% Example: Feature depends on library implementation
WorkOrders = [
    #{
        id => <<"wo-lib">>,
        task => <<"Implement authentication library">>,
        agent => 'erlang-otp-developer',
        dependencies => [],
        priority => high
    },
    #{
        id => <<"wo-feature">>,
        task => <<"Add OAuth login using auth library">>,
        agent => 'erlang-otp-developer',
        dependencies => [<<"wo-lib">>],  % Depends on wo-lib
        priority => high
    },
    #{
        id => <<"wo-test">>,
        task => <<"Test OAuth login">>,
        agent => 'erlang-test-engineer',
        dependencies => [<<"wo-feature">>],  % Depends on wo-feature
        priority => high
    }
].

%% Execution order: wo-lib → wo-feature → wo-test
```

### Topological Sort Algorithm

```erlang
%% Build dependency graph
Graph = #{
    <<"wo-a">> => [],
    <<"wo-b">> => [<<"wo-a">>],
    <<"wo-c">> => [<<"wo-a">>],
    <<"wo-d">> => [<<"wo-b">>, <<"wo-c">>]
}.

%% Topological sort yields execution order:
%% Level 0: wo-a
%% Level 1: wo-b, wo-c (parallel)
%% Level 2: wo-d

%% Result: wo-a → {wo-b || wo-c} → wo-d
```

### Parallel vs Sequential Execution

**Sequential** (dependencies form chain):
```
wo-1 → wo-2 → wo-3 → wo-4
Total time: T1 + T2 + T3 + T4
```

**Parallel** (independent work orders):
```
wo-1 ──┐
wo-2 ──┼→ (all parallel)
wo-3 ──┘
Total time: max(T1, T2, T3)
```

**Mixed** (some dependencies):
```
        ┌→ wo-2 ──┐
wo-1 ───┤         ├→ wo-4
        └→ wo-3 ──┘

Level 0: wo-1
Level 1: wo-2, wo-3 (parallel)
Level 2: wo-4
Total time: T1 + max(T2, T3) + T4
```

### Dependency Validation

System validates:
1. **No cycles**: wo-a → wo-b → wo-a (❌ invalid)
2. **Exist**: All dependency IDs must reference valid work orders
3. **Same scope**: Dependencies must be in same work order batch

---

## File-Level Locking

### Purpose

Prevent concurrent modification of same file by multiple work orders:

```
wo-001: Modifying erlmcp_client.erl
wo-002: Also wants to modify erlmcp_client.erl

Lock prevents wo-002 from starting until wo-001 completes
```

### Lock Acquisition

```erlang
%% API
acquire_lock(FilePath) -> {ok, LockToken} | {error, {locked_by, Agent}}.

%% Example
case acquire_lock(<<"apps/erlmcp_core/src/erlmcp_client.erl">>) of
    {ok, Token} ->
        %% Lock acquired, proceed with work
        modify_file(...),
        release_lock(Token);
    {error, {locked_by, OtherAgent}} ->
        %% File locked, wait or fail
        {error, file_locked}
end.
```

### Lock Lifecycle

```
1. Agent starts work order
2. Acquire locks for all files in constraints.files
3. If any lock fails → work order queues
4. Perform work
5. Release all locks on completion (success or failure)
```

### Lock Release

Locks are automatically released when:
- Work order transitions to `done`
- Work order transitions to `failed`
- Agent crashes (supervisor cleans up locks)

### Conflict Detection

```bash
# Example conflict scenario

# Work order 1 locks files
wo-001:
  files: [erlmcp_client.erl, erlmcp_client_tests.erl]
  status: wip

# Work order 2 attempts to lock overlapping file
wo-002:
  files: [erlmcp_client.erl, erlmcp_server.erl]
  status: queued (waiting for erlmcp_client.erl lock)

# Work order 3 locks non-overlapping files (can start)
wo-003:
  files: [erlmcp_registry.erl]
  status: wip (no conflict)
```

---

## Multi-Agent Orchestration

### Example: Implement Armstrong-Style FSM

**Task**: Implement finite state machine with full TDD

**Work Orders**:

```erlang
WorkOrders = [
    %% 1. Architecture design
    #{
        id => <<"wo-fsm-001">>,
        task => <<"Design FSM supervision tree and gen_statem behaviors">>,
        agent => 'erlang-architect',
        priority => high,
        dependencies => [],
        constraints => #{
            time_budget => 1200,
            cost_budget => 0.05,
            files => [<<"docs/architecture/fsm-design.md">>]
        }
    },

    %% 2. Implementation (depends on design)
    #{
        id => <<"wo-fsm-002">>,
        task => <<"Implement gen_statem FSM with supervision">>,
        agent => 'erlang-otp-developer',
        priority => high,
        dependencies => [<<"wo-fsm-001">>],
        constraints => #{
            time_budget => 3600,
            cost_budget => 0.15,
            files => [
                <<"apps/erlmcp_core/src/erlmcp_fsm.erl">>,
                <<"apps/erlmcp_core/src/erlmcp_fsm_sup.erl">>
            ]
        }
    },

    %% 3. Testing (depends on implementation)
    #{
        id => <<"wo-fsm-003">>,
        task => <<"Write EUnit and CT tests for FSM">>,
        agent => 'erlang-test-engineer',
        priority => high,
        dependencies => [<<"wo-fsm-002">>],
        constraints => #{
            time_budget => 2400,
            cost_budget => 0.10,
            files => [
                <<"apps/erlmcp_core/test/erlmcp_fsm_tests.erl">>,
                <<"apps/erlmcp_core/test/erlmcp_fsm_SUITE.erl">>
            ]
        }
    },

    %% 4. Code review (depends on implementation + tests)
    #{
        id => <<"wo-fsm-004">>,
        task => <<"Review FSM implementation for OTP compliance">>,
        agent => 'code-reviewer',
        priority => normal,
        dependencies => [<<"wo-fsm-002">>, <<"wo-fsm-003">>],
        constraints => #{
            time_budget => 600,
            cost_budget => 0.025
        }
    },

    %% 5. PR creation (depends on review)
    #{
        id => <<"wo-fsm-005">>,
        task => <<"Create PR with quality gates and documentation">>,
        agent => 'erlang-github-ops',
        priority => normal,
        dependencies => [<<"wo-fsm-004">>],
        constraints => #{
            time_budget => 300,
            cost_budget => 0.015
        }
    }
].
```

### Execution Timeline

```
t=0s:     wo-fsm-001 (architect) starts
t=1200s:  wo-fsm-001 done → wo-fsm-002 (developer) starts
t=4800s:  wo-fsm-002 done → wo-fsm-003 (test-engineer) starts
t=7200s:  wo-fsm-003 done → wo-fsm-004 (reviewer) starts
t=7800s:  wo-fsm-004 done → wo-fsm-005 (github-ops) starts
t=8100s:  wo-fsm-005 done → PR created

Total: 8100s (135 minutes) = 2.25 hours
Total cost: $0.345
```

### Parallel Execution Optimization

If tests and review are independent:

```erlang
%% Modify dependencies to allow parallelization
#{
    id => <<"wo-fsm-004">>,
    dependencies => [<<"wo-fsm-002">>],  % Only needs implementation
    ...
},

%% Timeline with parallelization:
t=0s:     wo-fsm-001 (architect) starts
t=1200s:  wo-fsm-001 done → wo-fsm-002 (developer) starts
t=4800s:  wo-fsm-002 done → wo-fsm-003 AND wo-fsm-004 start in parallel
t=7200s:  wo-fsm-003 done, wo-fsm-004 done at t=5400s
t=7500s:  wo-fsm-005 (github-ops) starts
t=7800s:  wo-fsm-005 done

Total: 7800s (130 minutes) = 2.17 hours (5 minutes saved)
```

---

## Receipts & Audit Trail

### Receipt Purpose

Receipts provide:
1. **Proof of work completion**
2. **Quality gate verification**
3. **Cost accounting**
4. **Audit trail**
5. **Debug information**

### Receipt Location

```bash
.erlmcp/receipts/<timestamp>-<work-order-id>.json

# Example
.erlmcp/receipts/1769919130-wo-fsm-002.json
```

### Receipt Format

```json
{
  "work_order_id": "wo-fsm-002",
  "agent": "erlang-otp-developer",
  "task": "Implement gen_statem FSM with supervision",
  "branch": "claude/implement-fsm-wo-fsm-002",
  "start_time": "2026-02-01T10:00:00Z",
  "end_time": "2026-02-01T11:00:23Z",
  "status": "completed",
  "quality_gates": {
    "compile": {
      "status": "pass",
      "errors": 0,
      "warnings": 0,
      "duration_ms": 2341
    },
    "eunit": {
      "status": "pass",
      "tests": 24,
      "failures": 0,
      "skipped": 0,
      "duration_ms": 4523
    },
    "ct": {
      "status": "pass",
      "suites": 2,
      "tests": 12,
      "failures": 0,
      "duration_ms": 8934
    },
    "dialyzer": {
      "status": "pass",
      "warnings": 0,
      "duration_ms": 12453
    },
    "coverage": {
      "status": "pass",
      "percentage": 87.3,
      "threshold": 80.0,
      "lines_covered": 245,
      "lines_total": 281
    }
  },
  "artifacts": [
    {
      "path": "apps/erlmcp_core/src/erlmcp_fsm.erl",
      "lines_added": 234,
      "lines_deleted": 0,
      "sha256": "a3f5b2c1..."
    },
    {
      "path": "apps/erlmcp_core/src/erlmcp_fsm_sup.erl",
      "lines_added": 47,
      "lines_deleted": 0,
      "sha256": "d8e1f9a2..."
    }
  ],
  "dependencies": ["wo-fsm-001"],
  "duration_seconds": 3623,
  "cost_usd": 0.15,
  "environment": {
    "otp_version": "28.3.1",
    "erlmcp_version": "v3.0.0",
    "build_hash": "abc123def456"
  }
}
```

### Receipt Verification

```bash
# List recent receipts
ls -lt .erlmcp/receipts/ | head -10

# View specific receipt
cat .erlmcp/receipts/1769919130-wo-fsm-002.json | jq .

# Check all receipts for failures
jq 'select(.status == "failed")' .erlmcp/receipts/*.json

# Sum total cost
jq -s 'map(.cost_usd) | add' .erlmcp/receipts/*.json
```

### Receipt Chain

Receipts form an audit chain:

```
wo-001 (receipt) → wo-002 (receipt, depends on wo-001) → wo-003 (receipt)

Verification:
1. Check wo-001 status = completed
2. Check wo-002 dependencies = ["wo-001"]
3. Check wo-002 start_time > wo-001 end_time
4. Verify quality gates all passed
```

---

## Status Tracking

### Work Order States

```bash
# View all queued work orders
cat .erlmcp/work-orders/queue.json | jq '.[] | {id, task, agent, priority}'

# View work in progress
cat .erlmcp/work-orders/wip.json | jq '.[] | {id, task, agent, start_time}'

# View completed work orders
cat .erlmcp/work-orders/completed.json | jq '.[] | {id, task, status, duration}'

# View failed work orders
cat .erlmcp/work-orders/completed.json | jq '.[] | select(.status == "failed")'
```

### Real-Time Status Updates

Work order status updates in real-time:

```json
// Initial state (queued)
{
  "id": "wo-001",
  "status": "queued",
  "created_at": "2026-02-01T10:00:00Z"
}

// Agent picks up (wip)
{
  "id": "wo-001",
  "status": "wip",
  "created_at": "2026-02-01T10:00:00Z",
  "started_at": "2026-02-01T10:05:00Z",
  "agent": "erlang-otp-developer"
}

// Completion (done)
{
  "id": "wo-001",
  "status": "done",
  "created_at": "2026-02-01T10:00:00Z",
  "started_at": "2026-02-01T10:05:00Z",
  "completed_at": "2026-02-01T10:35:23Z",
  "agent": "erlang-otp-developer",
  "result": { ... }
}
```

### Dashboard View (Conceptual)

```
╔══════════════════════════════════════════════════════════════╗
║  erlmcp Work Order Dashboard                                 ║
╠══════════════════════════════════════════════════════════════╣
║  QUEUED (5)        WIP (3)           DONE (42)               ║
║  ─────────────     ─────────────     ─────────────           ║
║  wo-010 (HIGH)     wo-007 (HIGH)     wo-001 ✓               ║
║  wo-012 (HIGH)     wo-008 (NORMAL)   wo-002 ✓               ║
║  wo-015 (NORMAL)   wo-009 (NORMAL)   wo-003 ✓               ║
║  wo-018 (NORMAL)                      ...                    ║
║  wo-020 (LOW)                                                ║
║                                                              ║
║  AGENT STATUS                                                ║
║  ─────────────                                               ║
║  erlang-otp-developer:     WIP (wo-007)                      ║
║  erlang-test-engineer:     WIP (wo-008)                      ║
║  erlang-architect:         Available                         ║
║  code-reviewer:            WIP (wo-009)                      ║
║  ...                                                         ║
╚══════════════════════════════════════════════════════════════╝
```

---

## Error Recovery

### Automatic Recovery

System automatically recovers from:

| Error Class | Recovery Strategy | Timeout |
|-------------|-------------------|---------|
| Compilation error | Report to user, block completion | ∞ (blocking) |
| Test failure | Report to user, block completion | ∞ (blocking) |
| Network timeout | Retry 3x (exponential backoff) | 30s |
| Git conflict | Attempt rebase, manual if fails | 120s |
| Agent crash | Supervisor restarts, retry work order | 5s |
| File lock timeout | Queue work order, retry later | 300s |

### Manual Intervention Required

| Error Class | Reason | User Action |
|-------------|--------|-------------|
| API design decision | Ambiguity | Choose between options |
| Breaking change | Risk assessment | Approve/reject |
| Merge conflict (semantic) | Context needed | Resolve manually |
| Budget exceeded | Cost limit | Increase budget or abort |

### Failed Work Order Handling

```json
// Failed work order example
{
  "id": "wo-042",
  "status": "failed",
  "agent": "erlang-otp-developer",
  "task": "Add validation logic",
  "error": {
    "type": "test_failure",
    "message": "3 tests failed in erlmcp_validation_tests",
    "details": {
      "failed_tests": [
        "test_validate_message_size",
        "test_validate_request_id",
        "test_validate_json_structure"
      ],
      "error_output": "..."
    }
  },
  "retry_count": 0,
  "max_retries": 3
}
```

### Retry Logic

```erlang
%% Automatic retry for transient failures
retry_work_order(WorkOrder) ->
    RetryCount = maps:get(retry_count, WorkOrder, 0),
    MaxRetries = maps:get(max_retries, WorkOrder, 3),

    case RetryCount < MaxRetries of
        true ->
            %% Retry with exponential backoff
            Delay = math:pow(2, RetryCount) * 1000,  % 1s, 2s, 4s
            timer:sleep(Delay),
            execute_work_order(WorkOrder#{retry_count => RetryCount + 1});
        false ->
            %% Max retries exceeded, manual intervention
            {error, max_retries_exceeded}
    end.
```

### Recovery Examples

**Example 1: Test Failure** (blocking)
```
wo-001: Test failure in erlmcp_client_tests
Action: Agent reports failure, waits for user to fix tests
Status: wip → blocked (awaiting fix)
```

**Example 2: Network Timeout** (automatic retry)
```
wo-002: Network timeout downloading dependency
Action: Retry with exponential backoff (1s, 2s, 4s)
Status: wip → retrying → wip (after recovery)
```

**Example 3: Git Conflict** (semi-automatic)
```
wo-003: Merge conflict in erlmcp_server.erl
Action: Attempt automatic rebase
  - Success: Continue
  - Failure: Request user intervention
Status: wip → resolving → (manual) → wip
```

---

## Best Practices

### 1. Break Large Tasks into Smaller Work Orders

**Bad**:
```erlang
#{
    id => <<"wo-huge">>,
    task => <<"Implement entire distributed session system">>,
    agent => 'erlang-otp-developer',
    ...
}
```

**Good**:
```erlang
[
    #{id => <<"wo-design">>, task => <<"Design session architecture">>, ...},
    #{id => <<"wo-backend">>, task => <<"Implement ETS backend">>, ...},
    #{id => <<"wo-mnesia">>, task => <<"Add Mnesia backend">>, ...},
    #{id => <<"wo-failover">>, task => <<"Implement failover logic">>, ...},
    #{id => <<"wo-tests">>, task => <<"Write comprehensive tests">>, ...}
]
```

**Why**: Smaller tasks → faster completion → better tracking → easier debugging

---

### 2. Declare Dependencies Explicitly

**Bad**:
```erlang
%% Implicit dependency (not declared)
#{id => <<"wo-feature">>, dependencies => [], ...}  % Assumes wo-lib exists
```

**Good**:
```erlang
%% Explicit dependency
#{id => <<"wo-feature">>, dependencies => [<<"wo-lib">>], ...}
```

**Why**: System can enforce correct execution order and detect missing dependencies

---

### 3. Set Realistic Time Budgets

**Bad**:
```erlang
#{
    constraints => #{
        time_budget => 300  % 5 minutes (too optimistic)
    }
}
```

**Good**:
```erlang
#{
    constraints => #{
        time_budget => 1800  % 30 minutes (realistic for moderate task)
    }
}
```

**Why**: Realistic budgets prevent timeout failures and allow buffer for quality gates

---

### 4. Monitor Costs for Cloud Execution

```erlang
%% Track costs per work order
TotalCost = lists:sum([
    maps:get(cost_usd, Receipt)
    || Receipt <- read_all_receipts()
]),

%% Set budget alerts
case TotalCost > 10.0 of
    true -> alert_budget_exceeded(TotalCost);
    false -> ok
end.
```

**Cost Optimization**:
- Use incremental testing (test only changed modules)
- Run benchmarks locally (hardware-dependent)
- Parallelize independent work orders

---

### 5. Use Receipts for Verification

```bash
# Verify all quality gates passed
jq 'select(.quality_gates.coverage.status == "fail")' .erlmcp/receipts/*.json

# Check for failed work orders
jq 'select(.status == "failed")' .erlmcp/receipts/*.json

# Verify no regressions
jq '.quality_gates.dialyzer.warnings' .erlmcp/receipts/*.json
```

---

### 6. Archive Old Work Orders

```bash
# Archive completed work orders older than 30 days
find .erlmcp/work-orders/completed/ -mtime +30 -exec mv {} .erlmcp/archive/ \;

# Archive receipts older than 90 days
find .erlmcp/receipts/ -mtime +90 -exec gzip {} \;
```

---

## Common Workflows

### Workflow 1: Implementing a Feature

**Scenario**: Add dark mode support to MCP server

**Work Orders**:
```erlang
[
    %% 1. Research existing patterns
    #{
        id => <<"wo-dark-001">>,
        task => <<"Research dark mode implementations in Erlang servers">>,
        agent => 'erlang-researcher'
    },

    %% 2. Design approach
    #{
        id => <<"wo-dark-002">>,
        task => <<"Design dark mode configuration and theme switching">>,
        agent => 'plan-designer',
        dependencies => [<<"wo-dark-001">>]
    },

    %% 3. Implement configuration
    #{
        id => <<"wo-dark-003">>,
        task => <<"Add dark mode config to sys.config">>,
        agent => 'erlang-otp-developer',
        dependencies => [<<"wo-dark-002">>]
    },

    %% 4. Implement theme logic
    #{
        id => <<"wo-dark-004">>,
        task => <<"Implement theme switching in dashboard server">>,
        agent => 'erlang-otp-developer',
        dependencies => [<<"wo-dark-003">>]
    },

    %% 5. Add tests
    #{
        id => <<"wo-dark-005">>,
        task => <<"Write EUnit tests for theme switching">>,
        agent => 'erlang-test-engineer',
        dependencies => [<<"wo-dark-004">>]
    },

    %% 6. Create PR
    #{
        id => <<"wo-dark-006">>,
        task => <<"Create PR with dark mode feature">>,
        agent => 'erlang-github-ops',
        dependencies => [<<"wo-dark-005">>]
    }
]
```

**Timeline**: 4-6 hours (sequential) or 3-4 hours (with parallelization)

---

### Workflow 2: Running Quality Gates

**Scenario**: Validate code before PR merge

**Work Orders**:
```erlang
[
    %% Single work order with comprehensive validation
    #{
        id => <<"wo-qa-001">>,
        task => <<"Run all quality gates (compile, test, coverage, dialyzer, xref)">>,
        agent => 'verifier',
        priority => high,
        constraints => #{
            time_budget => 600  % 10 minutes
        }
    }
]
```

**Execution**: Parallel (verifier runs all gates concurrently)

---

### Workflow 3: Autonomous Completion

**Scenario**: Fix bug and create PR without manual intervention

**Work Orders**:
```erlang
[
    %% 1. Implement fix
    #{
        id => <<"wo-fix-001">>,
        task => <<"Fix race condition in erlmcp_session_manager">>,
        agent => 'erlang-otp-developer',
        priority => high
    },

    %% 2. Add regression test
    #{
        id => <<"wo-fix-002">>,
        task => <<"Add test for race condition fix">>,
        agent => 'erlang-test-engineer',
        dependencies => [<<"wo-fix-001">>]
    },

    %% 3. Verify quality
    #{
        id => <<"wo-fix-003">>,
        task => <<"Run quality gates">>,
        agent => 'verifier',
        dependencies => [<<"wo-fix-002">>]
    },

    %% 4. Create PR
    #{
        id => <<"wo-fix-004">>,
        task => <<"Create PR with fix and tests">>,
        agent => 'erlang-github-ops',
        dependencies => [<<"wo-fix-003">>]
    }
]
```

**Result**: Autonomous execution, PR created with quality report

---

### Workflow 4: Interactive Development

**Scenario**: Develop locally, continue in cloud

**Local Development**:
```bash
# 1. Start local implementation
vim apps/erlmcp_core/src/erlmcp_feature.erl

# 2. Commit partial work
git add .
git commit -m "wip: partial feature implementation"
git push origin feature/my-feature
```

**Offload to Cloud**:
```erlang
%% Create work order to continue in cloud
#{
    id => <<"wo-cloud-001">>,
    task => <<"Complete feature implementation (continue from local work)">>,
    agent => 'erlang-otp-developer',
    constraints => #{
        branch => <<"feature/my-feature">>  % Continue on existing branch
    }
}
```

**Cloud Execution**: Agent pulls branch, continues work, runs quality gates, reports back

---

## Troubleshooting

### Agent Not Picking Up Work Order

**Symptoms**:
- Work order in `queue.json` but status remains `queued`
- No agent activity

**Diagnosis**:
```bash
# 1. Check agent WIP count
cat .erlmcp/work-orders/wip.json | jq '.[] | select(.agent == "erlang-otp-developer")'

# 2. Check dependencies
cat .erlmcp/work-orders/queue.json | jq '.[] | select(.id == "wo-001") | .dependencies'

# 3. Check file locks
cat .erlmcp/locks/*.json
```

**Solutions**:
1. **Agent busy**: Wait for current task to complete or increase agent count
2. **Dependency not met**: Ensure dependency work orders complete first
3. **File lock conflict**: Wait for conflicting work order to complete

---

### Dependency Resolution Failing

**Symptoms**:
- Work order stuck in `queued` indefinitely
- Error: `dependency_not_found`

**Diagnosis**:
```bash
# Check dependency exists
cat .erlmcp/work-orders/queue.json | jq '.[] | select(.id == "wo-dep-001")'

# Check dependency status
cat .erlmcp/work-orders/completed.json | jq '.[] | select(.id == "wo-dep-001")'
```

**Solutions**:
1. **Typo in dependency ID**: Fix work order JSON
2. **Dependency failed**: Investigate failed work order, retry
3. **Circular dependency**: Refactor dependencies to remove cycle

---

### File Lock Conflicts

**Symptoms**:
- Work order queued with message `waiting_for_file_lock`
- Multiple work orders modify same file

**Diagnosis**:
```bash
# Check current locks
cat .erlmcp/locks/*.json | jq '{file, work_order, agent}'

# Check work order file constraints
cat .erlmcp/work-orders/queue.json | jq '.[] | {id, files: .constraints.files}'
```

**Solutions**:
1. **Wait**: Lock will release when blocking work order completes
2. **Refactor**: Split work orders to avoid file overlap
3. **Sequential dependencies**: Make work orders depend on each other explicitly

---

### Budget Exceeded

**Symptoms**:
- Work order fails with `budget_exceeded`
- Cloud compute costs exceed limit

**Diagnosis**:
```bash
# Check cost for work order
cat .erlmcp/receipts/*.json | jq 'select(.work_order_id == "wo-001") | .cost_usd'

# Sum all costs
jq -s 'map(.cost_usd) | add' .erlmcp/receipts/*.json
```

**Solutions**:
1. **Increase budget**: Adjust `cost_budget` in work order constraints
2. **Optimize**: Use incremental testing, run benchmarks locally
3. **Parallelize**: Run independent work orders concurrently to reduce wall time

---

### Agent Crash/Timeout Handling

**Symptoms**:
- Work order in `wip` state but agent unresponsive
- Timeout exceeded

**Diagnosis**:
```bash
# Check work order start time
cat .erlmcp/work-orders/wip.json | jq '.[] | {id, started_at, agent}'

# Check time elapsed
current_time=$(date +%s)
start_time=$(cat .erlmcp/work-orders/wip.json | jq -r '.[] | select(.id == "wo-001") | .started_at' | date -f - +%s)
elapsed=$((current_time - start_time))
echo "Elapsed: ${elapsed}s"
```

**Solutions**:
1. **Supervisor restart**: Agent supervisor restarts crashed agent
2. **Retry**: System automatically retries work order (up to max retries)
3. **Manual intervention**: If max retries exceeded, investigate root cause

---

## FAQ

### How long do agents take?

**Answer**: Depends on task complexity:

| Task Type | Typical Duration |
|-----------|------------------|
| Simple function | 5-10 minutes |
| Module implementation | 20-40 minutes |
| Feature with tests | 1-2 hours |
| Architecture design | 30-60 minutes |
| Code review | 10-20 minutes |
| PR creation | 5 minutes |

**Quality gates add**: 2-4 minutes (compile, test, dialyzer, xref)

---

### Can I cancel a work order?

**Answer**: Yes, before agent picks it up.

```bash
# Remove from queue (before wip)
jq 'del(.[] | select(.id == "wo-cancel-me"))' .erlmcp/work-orders/queue.json > tmp.json
mv tmp.json .erlmcp/work-orders/queue.json
```

**Note**: Cannot cancel work order in `wip` state (agent already started)

---

### What if agent crashes?

**Answer**: Supervisor restarts agent automatically.

```erlang
%% Supervision tree ensures agent restart
%% Work order retries up to max_retries (default: 3)

%% If crash is persistent:
%% 1. Work order fails after max retries
%% 2. Receipt generated with failure details
%% 3. Manual investigation required
```

---

### Can multiple agents modify same file?

**Answer**: No, file locking prevents concurrent modification.

**Example**:
```
wo-001: Modifying erlmcp_client.erl (lock acquired)
wo-002: Attempts to modify erlmcp_client.erl (queued, waiting for lock)
wo-001: Completes (lock released)
wo-002: Starts (lock acquired)
```

---

### Cost per work order?

**Answer**: Varies by task and constraints:

| Task Type | Cloud Time | Estimated Cost |
|-----------|------------|----------------|
| Simple implementation | 10-20 min | $0.02-$0.05 |
| Module + tests | 30-60 min | $0.10-$0.20 |
| Architecture design | 20-40 min | $0.05-$0.10 |
| Code review | 5-15 min | $0.01-$0.03 |
| Quality gates | 2-4 min | $0.01-$0.02 |

**Note**: Costs tracked in receipts for accurate accounting

---

### How do I track progress?

**Answer**: Multiple methods:

```bash
# 1. View work order status files
cat .erlmcp/work-orders/wip.json | jq '.[] | {id, agent, task, start_time}'

# 2. View receipts for completed work
ls -lt .erlmcp/receipts/ | head -10

# 3. Check agent activity
cat .erlmcp/work-orders/wip.json | jq 'group_by(.agent) | map({agent: .[0].agent, count: length})'

# 4. Monitor quality gates
tail -f .erlmcp/build.log
tail -f .erlmcp/test.log
```

---

### What's the difference between governance hooks and work orders?

**Answer**: Two distinct systems:

| Feature | Governance Hooks (DEVELOPMENT.md) | Work Orders (GOVERNANCE_SYSTEM.md) |
|---------|-----------------------------------|-------------------------------------|
| **Purpose** | Environment enforcement | Task orchestration |
| **Trigger** | SessionStart, PreToolUse, PostToolUse | Explicit work order submission |
| **Scope** | OTP installation, policy enforcement | Multi-agent development tasks |
| **Examples** | Bootstrap OTP 28, validate network access | Implement feature, run tests, create PR |
| **Location** | `.claude/hooks/` | `.erlmcp/work-orders/` |

Both systems work together:
- **Hooks** ensure correct environment and policies
- **Work orders** orchestrate development tasks within that environment

---

### Can I see an example work order?

**Answer**: Yes, here's a complete example:

```json
{
  "id": "wo-example-001",
  "task": "Add validation for MCP request IDs in erlmcp_json_rpc",
  "agent": "erlang-otp-developer",
  "priority": "normal",
  "dependencies": [],
  "constraints": {
    "time_budget": 1800,
    "cost_budget": 0.08,
    "files": [
      "apps/erlmcp_core/src/erlmcp_json_rpc.erl",
      "apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl"
    ],
    "branch": "claude/add-request-validation-wo-example-001"
  },
  "status": "queued",
  "created_at": "2026-02-01T10:00:00Z"
}
```

**After completion**, result added:

```json
{
  ...
  "status": "done",
  "started_at": "2026-02-01T10:05:00Z",
  "completed_at": "2026-02-01T10:28:15Z",
  "result": {
    "exit_code": 0,
    "gates": {
      "compile": "pass",
      "eunit": "pass",
      "dialyzer": "pass",
      "coverage": {"status": "pass", "percentage": 89.2}
    },
    "artifacts": [
      "apps/erlmcp_core/src/erlmcp_json_rpc.erl",
      "apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl"
    ],
    "duration": 1395
  }
}
```

---

## See Also

- **CLAUDE.md** - Formal system specification, OTP patterns, quality gates
- **DEVELOPMENT.md** - Development environment, governance hooks, workflows
- **CONTRIBUTING.md** - Contribution guidelines, PR process
- **PROJECT_STATUS_REPORT_v3.0.0.md** - Project health assessment
- **docs/architecture.md** - System architecture and supervision trees

---

**Version**: 3.0.0
**Last Updated**: 2026-02-01
**Maintained By**: erlmcp project team
