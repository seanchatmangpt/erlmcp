# WEB AGENT COORDINATION DESIGN v1.0.0

**Multi-Agent Coordination System for Claude Code on the Web**

---

## Executive Summary

This document specifies a **production-grade multi-agent coordination system** for Claude Code web sessions, enabling multiple agents to work on erlmcp simultaneously without conflicts. The design follows erlmcp's Armstrong principles: supervision, isolation, fault tolerance, and observable behavior.

**Core Principles:**
- **Process-per-Agent** - Each agent session runs in isolation
- **Work Order Queue** - File-based, priority-driven task dispatch
- **Kanban WIP Limits** - Cost control via phase-based backpressure
- **Git Branch Isolation** - One branch per agent, automatic conflict detection
- **Observable State** - Real-time dashboard, JSON API, status tracking
- **Let-It-Crash** - Failed agents don't block others, automatic retry

**Target Deployment:** Claude Code Web (claude.ai/code)

**Performance Targets:**
- Work order dispatch latency: <100ms
- Agent assignment time: <50ms
- Status update propagation: <200ms
- Concurrent agents: 20+ (with WIP limits)
- Queue depth: 1000+ work orders

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Work Order Queue System](#2-work-order-queue-system)
3. [Agent Assignment Logic](#3-agent-assignment-logic)
4. [Kanban WIP Limits](#4-kanban-wip-limits)
5. [Concurrency Safety](#5-concurrency-safety)
6. [Status Dashboard for Web](#6-status-dashboard-for-web)
7. [Session Coordination](#7-session-coordination)
8. [Claude Code Web Hooks](#8-claude-code-web-hooks)
9. [Rollback & Recovery](#9-rollback--recovery)
10. [Implementation Roadmap](#10-implementation-roadmap)

---

## 1. Architecture Overview

### 1.1 System Components

```
┌────────────────────────────────────────────────────────────────┐
│                   Claude Code Web Interface                     │
│                      (claude.ai/code)                           │
└─────────────────────┬──────────────────────────────────────────┘
                      │
                      ▼
┌────────────────────────────────────────────────────────────────┐
│              Web Agent Coordination Layer                       │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────────┐         ┌──────────────────┐             │
│  │  Work Order      │◄────────┤  Agent           │             │
│  │  Queue Manager   │         │  Assignment      │             │
│  │  (Priority)      │         │  Engine          │             │
│  └─────────┬────────┘         └────────┬─────────┘             │
│            │                           │                        │
│            ▼                           ▼                        │
│  ┌──────────────────┐         ┌──────────────────┐             │
│  │  Kanban WIP      │         │  Git Branch      │             │
│  │  Controller      │         │  Coordinator     │             │
│  │  (Cost Control)  │         │  (Isolation)     │             │
│  └──────────────────┘         └──────────────────┘             │
│            │                           │                        │
│            ▼                           ▼                        │
│  ┌───────────────────────────────────────────────┐             │
│  │         Status Dashboard & API                │             │
│  │  (Real-time monitoring, JSON endpoints)       │             │
│  └───────────────────────────────────────────────┘             │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
                      │
                      ▼
┌────────────────────────────────────────────────────────────────┐
│                   Agent Session Pool                            │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │ Agent 1  │  │ Agent 2  │  │ Agent 3  │  │ Agent N  │       │
│  │ (Design) │  │ (Impl)   │  │ (Test)   │  │ (Review) │       │
│  └─────┬────┘  └─────┬────┘  └─────┬────┘  └─────┬────┘       │
│        │             │             │             │             │
│        └─────────────┴─────────────┴─────────────┘             │
│                      │                                          │
│                      ▼                                          │
│  ┌───────────────────────────────────────────────┐             │
│  │     Shared State (Read-Only)                  │             │
│  │  - Work order queue (.erlmcp/work-orders/)    │             │
│  │  - Agent registry (.erlmcp/agents/)           │             │
│  │  - Session state (.erlmcp/sessions/)          │             │
│  └───────────────────────────────────────────────┘             │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
                      │
                      ▼
┌────────────────────────────────────────────────────────────────┐
│                  Git Repository Layer                           │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  main ───┬─── agent/design/WO-001                              │
│          ├─── agent/impl/WO-002                                │
│          ├─── agent/test/WO-003                                │
│          └─── agent/review/WO-004                              │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

### 1.2 Design Principles

**Armstrong Principles Applied:**

| Principle | Application |
|-----------|-------------|
| **Process Isolation** | Each agent runs in separate web session; failures don't cascade |
| **Supervision** | Work order manager supervises agent lifecycle; auto-restart on failure |
| **Let-It-Crash** | Failed agents release work orders back to queue; no partial states |
| **Observable Behavior** | All state transitions logged; dashboard shows real-time status |
| **Message Passing** | Agents communicate via file-based messages; no shared mutable state |
| **Fault Tolerance** | System continues with reduced capacity if agents fail |

**Key Invariants:**

```
∀ work_order ∈ Queue. ∃! agent ∈ Agents ∨ work_order.status = queued
∀ agent ∈ Active. work_orders(agent) ≤ WIP_LIMIT(agent.phase)
∀ file ∈ CriticalFiles. locked(file) ⟹ ∃! agent ∈ Agents. owner(file) = agent
∀ work_order ∈ Completed. ∃ git_branch. merged(git_branch, main)
```

### 1.3 File System Layout

```
.erlmcp/
├── work-orders/
│   ├── queued/
│   │   ├── WO-001.json          # Priority: critical
│   │   ├── WO-002.json          # Priority: high
│   │   └── WO-003.json          # Priority: normal
│   ├── assigned/
│   │   ├── WO-004.json          # Agent: erlang-architect
│   │   └── WO-005.json          # Agent: erlang-otp-developer
│   ├── in-progress/
│   │   ├── WO-006.json          # Stage: implementation
│   │   └── WO-007.json          # Stage: testing
│   ├── completed/
│   │   └── WO-008.json          # SKU: feature-x-v1
│   └── failed/
│       └── WO-009.json          # Retry count: 2
│
├── agents/
│   ├── registry.json            # Active agent sessions
│   ├── assignments.json         # Agent → Work Order mapping
│   └── capabilities.json        # Agent skill matrix
│
├── sessions/
│   ├── parent-session-id.json  # Root session metadata
│   ├── child-001.json           # Agent session (design)
│   ├── child-002.json           # Agent session (impl)
│   └── teleport/
│       ├── child-001.artifact   # Teleport artifact for handoff
│       └── child-002.artifact
│
├── locks/
│   ├── CLAUDE.md.lock           # File-level locks
│   ├── rebar.config.lock
│   └── work-order-queue.lock    # Queue operations lock
│
└── dashboard/
    ├── status.json              # Current system state
    ├── metrics.json             # Performance metrics
    └── events.jsonl             # Event log (append-only)
```

---

## 2. Work Order Queue System

### 2.1 Work Order Schema

**File:** `.erlmcp/work-orders/queued/WO-{ID}.json`

```json
{
  "work_order_id": "WO-001",
  "version": "1.0",
  "created_at": "2026-02-01T12:00:00Z",
  "updated_at": "2026-02-01T12:00:00Z",
  "status": "queued",
  "priority": "critical",
  "bucket": "security",

  "task": {
    "title": "Implement Armstrong FSM for 9-nines reliability",
    "description": "Design and implement gen_statem-based FSMs for client/server",
    "acceptance_criteria": [
      "Client FSM handles all protocol states",
      "Server FSM handles all protocol states",
      "State transitions are deterministic",
      "Tests achieve 95% coverage",
      "Performance regression < 5%"
    ],
    "pull_signal": {
      "type": "github_issue",
      "source": "https://github.com/seanchatmangpt/erlmcp/issues/42"
    }
  },

  "assignment": {
    "required_agent": "erlang-architect",
    "required_skills": ["supervision", "gen_statem", "OTP design"],
    "assigned_to": null,
    "assigned_at": null,
    "session_id": null
  },

  "dependencies": {
    "blocked_by": [],
    "blocks": ["WO-002", "WO-003"]
  },

  "sla": {
    "deadline": "2026-02-02T12:00:00Z",
    "elapsed_hours": 0,
    "remaining_hours": 24
  },

  "lifecycle": {
    "stages": ["requirements", "design", "implementation", "testing", "review"],
    "current_stage": null,
    "completed_stages": []
  },

  "git": {
    "branch": null,
    "commits": [],
    "merge_status": null
  },

  "retry": {
    "count": 0,
    "max_retries": 3,
    "backoff_seconds": [300, 900, 3600]
  },

  "artifacts": {
    "design_doc": null,
    "sku_id": null,
    "teleport_artifact": null
  }
}
```

### 2.2 Priority Dispatch Algorithm

**Priority Levels:**
- `critical` (0) - Security, production outages, SLA breaches
- `high` (1) - Bugs, reliability issues, refunds
- `normal` (2) - Features, enhancements
- `low` (3) - Technical debt, optimizations

**Dispatch Rules:**

```
Priority Score = BASE_PRIORITY
                 + AGE_BONUS(created_at)
                 + SLA_URGENCY(deadline)
                 + DEPENDENCY_CHAIN(blocks)

where:
  BASE_PRIORITY ∈ {0: critical, 1: high, 2: normal, 3: low}
  AGE_BONUS = min(24, hours_queued) / 24  # Max +1 point after 24h
  SLA_URGENCY = max(0, 1 - (remaining_hours / deadline_hours))
  DEPENDENCY_CHAIN = count(blocks) * 0.1  # +0.1 per blocked work order
```

**Dispatch Algorithm:**

```python
def get_next_work_order(agent_capabilities, wip_phase_limits):
    """
    Select highest priority work order that:
    1. Matches agent capabilities
    2. Not blocked by dependencies
    3. Phase has available WIP capacity
    4. No file conflicts with in-progress work orders
    """

    # Filter eligible work orders
    eligible = []
    for wo in read_queued_work_orders():
        if not matches_agent_capabilities(wo, agent_capabilities):
            continue
        if has_blocking_dependencies(wo):
            continue
        if exceeds_wip_limit(wo.lifecycle.current_stage, wip_phase_limits):
            continue
        if has_file_conflicts(wo, get_in_progress_work_orders()):
            continue

        eligible.append((calculate_priority_score(wo), wo))

    # Sort by priority score (descending)
    eligible.sort(key=lambda x: x[0], reverse=True)

    return eligible[0][1] if eligible else None
```

### 2.3 Dependency Management

**Dependency Graph:**

```
WO-001 (Design FSM)
  ├─ blocks → WO-002 (Implement Client FSM)
  └─ blocks → WO-003 (Implement Server FSM)

WO-002 + WO-003
  └─ blocks → WO-004 (Write FSM Tests)

WO-004
  └─ blocks → WO-005 (Performance Validation)
```

**Operations:**

```bash
# Add dependency
add_dependency(dependent_wo_id, blocking_wo_id):
  wo = load_work_order(dependent_wo_id)
  wo.dependencies.blocked_by.append(blocking_wo_id)
  wo.status = "blocked" if len(wo.dependencies.blocked_by) > 0 else "queued"
  save_work_order(wo)

# Check if unblocked
check_unblocked(wo_id):
  wo = load_work_order(wo_id)
  for blocker_id in wo.dependencies.blocked_by:
    blocker = load_work_order(blocker_id)
    if blocker.status != "completed":
      return False
  return True

# Auto-unblock on completion
on_work_order_completed(wo_id):
  for dependent_wo in find_dependents(wo_id):
    if check_unblocked(dependent_wo.work_order_id):
      dependent_wo.status = "queued"
      save_work_order(dependent_wo)
      emit_event("work_order.unblocked", dependent_wo)
```

### 2.4 Status Transitions

```
State Machine:
                                    ┌─────────────┐
                                    │   queued    │
                                    └──────┬──────┘
                                           │
                          ┌────────────────┼────────────────┐
                          │                │                │
                          ▼                ▼                ▼
                    ┌──────────┐    ┌──────────┐    ┌──────────┐
                    │ assigned │    │ blocked  │    │cancelled │
                    └─────┬────┘    └────┬─────┘    └──────────┘
                          │              │
                          ▼              │
                    ┌──────────┐         │
                    │in-progress│◄───────┘
                    └─────┬────┘
                          │
                ┌─────────┴─────────┐
                │                   │
                ▼                   ▼
          ┌──────────┐        ┌──────────┐
          │completed │        │  failed  │
          └──────────┘        └─────┬────┘
                                    │
                                    └──► retry → queued (if retries < max)
```

**Transition Rules:**

| From | To | Trigger | Validation |
|------|----|---------|-----------|
| `queued` | `assigned` | Agent claims work order | Agent has required skills |
| `assigned` | `in-progress` | Agent starts work | WIP limit not exceeded |
| `in-progress` | `completed` | Agent completes work | All acceptance criteria met |
| `in-progress` | `failed` | Agent encounters error | Error logged |
| `failed` | `queued` | Retry trigger | Retry count < max_retries |
| `queued` | `blocked` | Dependency added | blocked_by not empty |
| `blocked` | `queued` | Dependency resolved | All blockers completed |
| `*` | `cancelled` | User cancellation | Manual intervention |

---

## 3. Agent Assignment Logic

### 3.1 Agent Capability Matrix

**File:** `.erlmcp/agents/capabilities.json`

```json
{
  "agents": {
    "erlang-architect": {
      "skills": [
        "supervision",
        "gen_server",
        "gen_statem",
        "OTP design",
        "system architecture",
        "behavior selection"
      ],
      "phases": ["design"],
      "priority": 1,
      "max_concurrent_tasks": 2
    },
    "erlang-otp-developer": {
      "skills": [
        "gen_server",
        "gen_statem",
        "supervisor",
        "OTP behaviors",
        "Erlang coding"
      ],
      "phases": ["implementation"],
      "priority": 2,
      "max_concurrent_tasks": 3
    },
    "erlang-test-engineer": {
      "skills": [
        "EUnit",
        "Common Test",
        "PropEr",
        "Chicago TDD",
        "test design"
      ],
      "phases": ["testing"],
      "priority": 2,
      "max_concurrent_tasks": 5
    },
    "erlang-performance": {
      "skills": [
        "benchmarking",
        "profiling",
        "optimization",
        "performance analysis"
      ],
      "phases": ["testing", "review"],
      "priority": 2,
      "max_concurrent_tasks": 3
    },
    "code-reviewer": {
      "skills": [
        "code review",
        "OTP compliance",
        "quality gates",
        "best practices"
      ],
      "phases": ["review"],
      "priority": 1,
      "max_concurrent_tasks": 5
    },
    "erlang-transport-builder": {
      "skills": [
        "transport implementations",
        "TCP",
        "HTTP",
        "WebSocket",
        "SSE"
      ],
      "phases": ["design", "implementation"],
      "priority": 2,
      "max_concurrent_tasks": 2
    }
  }
}
```

### 3.2 Skill-Based Routing

**Matching Algorithm:**

```python
def match_agent_to_work_order(work_order):
    """
    Find best-fit agent for work order based on:
    1. Required skills match
    2. Current workload
    3. Agent priority
    4. Phase alignment
    """

    capabilities = load_capabilities()
    candidates = []

    for agent_name, agent_info in capabilities["agents"].items():
        # Check skill match
        required_skills = work_order["assignment"]["required_skills"]
        agent_skills = agent_info["skills"]
        skill_match = len(set(required_skills) & set(agent_skills)) / len(required_skills)

        if skill_match < 0.5:  # Require 50% skill match
            continue

        # Check phase alignment
        current_stage = work_order["lifecycle"]["current_stage"] or "design"
        if current_stage not in agent_info["phases"]:
            continue

        # Check workload
        current_tasks = count_agent_tasks(agent_name)
        if current_tasks >= agent_info["max_concurrent_tasks"]:
            continue

        # Calculate fitness score
        workload_score = 1.0 - (current_tasks / agent_info["max_concurrent_tasks"])
        priority_score = 1.0 / agent_info["priority"]

        fitness = skill_match * 0.5 + workload_score * 0.3 + priority_score * 0.2

        candidates.append((fitness, agent_name))

    # Sort by fitness (descending)
    candidates.sort(key=lambda x: x[0], reverse=True)

    return candidates[0][1] if candidates else None
```

### 3.3 Load Balancing

**Workload Distribution:**

```
Agent Utilization Target: 60-80%

Design Phase (max 3 concurrent):
  erlang-architect:        [████████░░] 2/2 tasks (100%)
  plan-designer:           [████░░░░░░] 1/2 tasks (50%)

Implementation Phase (max 5 concurrent):
  erlang-otp-developer:    [██████░░░░] 3/5 tasks (60%)
  erlang-transport-builder:[████░░░░░░] 2/5 tasks (40%)

Testing Phase (max 7 concurrent):
  erlang-test-engineer:    [██████████] 5/5 tasks (100%)
  erlang-performance:      [████░░░░░░] 2/5 tasks (40%)

Review Phase (max 5 concurrent):
  code-reviewer:           [██████░░░░] 3/5 tasks (60%)
```

**Load Balancing Algorithm:**

```python
def balance_workload():
    """
    Rebalance workload if agents are overloaded.
    """

    for phase in ["design", "implementation", "testing", "review"]:
        agents_in_phase = get_agents_for_phase(phase)

        # Calculate average utilization
        total_tasks = sum(count_agent_tasks(a) for a in agents_in_phase)
        total_capacity = sum(get_agent_capacity(a) for a in agents_in_phase)
        avg_utilization = total_tasks / total_capacity if total_capacity > 0 else 0

        # If average utilization > 80%, trigger backpressure
        if avg_utilization > 0.8:
            pause_new_assignments(phase)
            emit_alert("high_utilization", phase, avg_utilization)

        # If average utilization < 40%, allow queued work orders
        if avg_utilization < 0.4:
            resume_assignments(phase)
```

### 3.4 Conflict Avoidance

**File Conflict Detection:**

```python
def has_file_conflicts(work_order, in_progress_work_orders):
    """
    Check if work order would conflict with in-progress work.

    Conflict occurs if:
    1. Same file modified by multiple agents
    2. Same module being worked on
    3. Critical files (CLAUDE.md, rebar.config) locked
    """

    # Estimate affected files based on task description
    affected_files = predict_affected_files(work_order)

    for in_progress_wo in in_progress_work_orders:
        in_progress_files = predict_affected_files(in_progress_wo)

        # Check for file overlap
        overlap = set(affected_files) & set(in_progress_files)

        if overlap:
            # Allow if files are test files (parallel testing)
            if all(f.endswith("_tests.erl") for f in overlap):
                continue

            # Conflict detected
            return True

    # Check for locked critical files
    for critical_file in ["CLAUDE.md", "rebar.config", ".github/workflows/*"]:
        if critical_file in affected_files and is_locked(critical_file):
            return True

    return False
```

---

## 4. Kanban WIP Limits

### 4.1 WIP Limits by Phase

**Configuration:** `.erlmcp/dashboard/wip-limits.json`

```json
{
  "version": "1.0",
  "updated_at": "2026-02-01T12:00:00Z",

  "wip_limits": {
    "design": {
      "max_concurrent": 3,
      "current": 2,
      "reason": "Architecture decisions need focus; high cognitive load"
    },
    "implementation": {
      "max_concurrent": 5,
      "current": 3,
      "reason": "Code can be parallelized; multiple modules"
    },
    "testing": {
      "max_concurrent": 7,
      "current": 5,
      "reason": "Tests are independent; high parallelism"
    },
    "review": {
      "max_concurrent": 5,
      "current": 3,
      "reason": "Reviews need attention; quality over speed"
    }
  },

  "cost_control": {
    "total_concurrent_agents": 20,
    "current_active_agents": 13,
    "estimated_cost_per_hour": "$50",
    "daily_budget": "$1200",
    "alerts": {
      "budget_threshold": 0.8,
      "utilization_threshold": 0.9
    }
  }
}
```

### 4.2 Backpressure Mechanism

**When WIP Limit Reached:**

```
1. Pause new assignments for that phase
2. Emit event: "wip_limit_reached"
3. Update dashboard status: "⚠️ PHASE_NAME at capacity"
4. Queue work orders until capacity available
5. (Optional) Scale down: Complete in-flight work before accepting new
```

**Example:**

```
Design Phase (3/3) - AT CAPACITY
┌─────────────────────────────────────────────────────┐
│ WO-001 │ erlang-architect  │ FSM Design      │ 2h  │
│ WO-004 │ erlang-architect  │ Cache Design    │ 1h  │
│ WO-007 │ plan-designer     │ Transport Plan  │ 0.5h│
└─────────────────────────────────────────────────────┘

Queued (waiting for capacity):
  - WO-010 (Security Design) - Priority: critical ⚠️
  - WO-011 (Feature Design)  - Priority: normal
```

### 4.3 WIP Enforcement

**Pre-Assignment Check:**

```python
def can_assign_work_order(work_order):
    """
    Check if work order can be assigned based on WIP limits.
    """

    phase = work_order["lifecycle"]["current_stage"] or "design"
    limits = load_wip_limits()

    current_wip = limits["wip_limits"][phase]["current"]
    max_wip = limits["wip_limits"][phase]["max_concurrent"]

    if current_wip >= max_wip:
        log_event("wip_limit_reached", {
            "phase": phase,
            "current": current_wip,
            "max": max_wip,
            "work_order_id": work_order["work_order_id"]
        })
        return False

    # Check total concurrent agents
    total_active = count_active_agents()
    max_total = limits["cost_control"]["total_concurrent_agents"]

    if total_active >= max_total:
        log_event("agent_limit_reached", {
            "total_active": total_active,
            "max_total": max_total
        })
        return False

    return True
```

### 4.4 Cost Control Alerts

**Budget Monitoring:**

```python
def check_budget_alerts():
    """
    Monitor cost and alert if thresholds exceeded.
    """

    limits = load_wip_limits()
    cost_per_hour = parse_cost(limits["cost_control"]["estimated_cost_per_hour"])
    daily_budget = parse_cost(limits["cost_control"]["daily_budget"])

    # Calculate current daily burn rate
    active_agents = count_active_agents()
    hourly_burn = active_agents * cost_per_hour
    daily_burn = hourly_burn * 24

    # Calculate utilization
    utilization = daily_burn / daily_budget

    # Alert if over threshold
    threshold = limits["cost_control"]["alerts"]["budget_threshold"]
    if utilization >= threshold:
        emit_alert("budget_warning", {
            "utilization": utilization,
            "daily_burn": daily_burn,
            "daily_budget": daily_budget,
            "active_agents": active_agents
        })

    # Hard stop if over budget
    if utilization >= 1.0:
        pause_all_new_assignments()
        emit_alert("budget_exceeded", {
            "daily_burn": daily_burn,
            "daily_budget": daily_budget
        })
```

---

## 5. Concurrency Safety

### 5.1 Git Branch Isolation

**Branch Naming Convention:**

```
agent/{phase}/{work-order-id}

Examples:
  agent/design/WO-001
  agent/impl/WO-002
  agent/test/WO-003
  agent/review/WO-004
```

**Branch Creation:**

```python
def create_agent_branch(work_order):
    """
    Create isolated branch for agent work.
    """

    wo_id = work_order["work_order_id"]
    phase = work_order["lifecycle"]["current_stage"] or "design"
    branch_name = f"agent/{phase}/{wo_id}"

    # Create branch from main
    run_command(f"git checkout main")
    run_command(f"git pull origin main")
    run_command(f"git checkout -b {branch_name}")

    # Update work order
    work_order["git"]["branch"] = branch_name
    save_work_order(work_order)

    # Register branch in agent assignments
    register_branch_assignment(work_order["assignment"]["assigned_to"], branch_name)

    return branch_name
```

### 5.2 File-Level Locking

**Lock File Format:** `.erlmcp/locks/{filename}.lock`

```json
{
  "file": "apps/erlmcp_core/src/erlmcp_client.erl",
  "locked_by": "erlang-otp-developer",
  "work_order_id": "WO-002",
  "session_id": "session-abc123",
  "locked_at": "2026-02-01T12:00:00Z",
  "lock_type": "exclusive",
  "reason": "Implementing client FSM"
}
```

**Lock Operations:**

```python
def acquire_lock(file_path, agent_name, work_order_id):
    """
    Acquire exclusive lock on file.
    """

    lock_file = f".erlmcp/locks/{file_path.replace('/', '_')}.lock"

    # Check if already locked
    if os.path.exists(lock_file):
        existing_lock = load_json(lock_file)

        # Allow same agent to re-acquire
        if existing_lock["locked_by"] == agent_name:
            return True

        # Lock conflict
        return False

    # Create lock
    lock_data = {
        "file": file_path,
        "locked_by": agent_name,
        "work_order_id": work_order_id,
        "session_id": get_current_session_id(),
        "locked_at": datetime.utcnow().isoformat(),
        "lock_type": "exclusive",
        "reason": f"Working on {work_order_id}"
    }

    save_json(lock_file, lock_data)
    return True

def release_lock(file_path, agent_name):
    """
    Release lock on file.
    """

    lock_file = f".erlmcp/locks/{file_path.replace('/', '_')}.lock"

    if not os.path.exists(lock_file):
        return True

    existing_lock = load_json(lock_file)

    # Only owner can release
    if existing_lock["locked_by"] != agent_name:
        return False

    os.remove(lock_file)
    return True
```

**Critical Files (Always Locked):**

```python
CRITICAL_FILES = [
    "CLAUDE.md",
    "rebar.config",
    "rebar.lock",
    ".github/workflows/*",
    "apps/*/src/*_app.erl",
    "apps/*/src/*_sup.erl"
]
```

### 5.3 Merge Strategy

**Merge Decision Matrix:**

| Branch Type | Base Branch | Merge Strategy | Rationale |
|-------------|-------------|----------------|-----------|
| `agent/design/*` | `main` | Squash | Design docs are logical units |
| `agent/impl/*` | `main` | Rebase | Preserve commit history for code |
| `agent/test/*` | `main` | Squash | Tests are atomic additions |
| `agent/review/*` | `main` | Fast-forward | Reviews don't add new commits |

**Merge Workflow:**

```python
def merge_agent_branch(work_order):
    """
    Merge agent branch back to main.
    """

    branch_name = work_order["git"]["branch"]
    phase = work_order["lifecycle"]["current_stage"]

    # Determine merge strategy
    if phase == "design":
        strategy = "squash"
    elif phase == "implementation":
        strategy = "rebase"
    elif phase == "testing":
        strategy = "squash"
    elif phase == "review":
        strategy = "fast-forward"
    else:
        strategy = "rebase"  # Default

    # Update main
    run_command("git checkout main")
    run_command("git pull origin main")

    # Merge
    if strategy == "squash":
        run_command(f"git merge --squash {branch_name}")
        run_command(f"git commit -m 'Complete {work_order['work_order_id']}: {work_order['task']['title']}'")
    elif strategy == "rebase":
        run_command(f"git checkout {branch_name}")
        run_command("git rebase main")
        run_command("git checkout main")
        run_command(f"git merge --ff-only {branch_name}")
    elif strategy == "fast-forward":
        run_command(f"git merge --ff-only {branch_name}")

    # Update work order
    work_order["git"]["merge_status"] = "merged"
    work_order["git"]["merged_at"] = datetime.utcnow().isoformat()
    save_work_order(work_order)

    # Delete branch
    run_command(f"git branch -d {branch_name}")
```

### 5.4 Conflict Detection

**Pre-Merge Conflict Check:**

```python
def check_merge_conflicts(work_order):
    """
    Check for merge conflicts before attempting merge.
    """

    branch_name = work_order["git"]["branch"]

    # Fetch latest main
    run_command("git fetch origin main")

    # Dry-run merge
    result = run_command(f"git merge-tree $(git merge-base {branch_name} origin/main) {branch_name} origin/main")

    # Check for conflict markers
    if "<<<<<" in result or ">>>>>" in result:
        conflicts = extract_conflict_files(result)

        # Log conflict
        log_event("merge_conflict_detected", {
            "work_order_id": work_order["work_order_id"],
            "branch": branch_name,
            "conflicts": conflicts
        })

        # Move to failed state
        work_order["status"] = "failed"
        work_order["error"] = {
            "type": "merge_conflict",
            "message": f"Merge conflicts in: {', '.join(conflicts)}",
            "conflicts": conflicts
        }
        save_work_order(work_order)

        return False

    return True
```

---

## 6. Status Dashboard for Web

### 6.1 Dashboard Architecture

**Technology Stack:**
- Backend: Static JSON files (no server needed for web)
- Frontend: Claude Code Web can poll `.erlmcp/dashboard/status.json`
- Updates: Agents write to status files; dashboard reads
- Real-time: Polling every 5-10 seconds

**Dashboard Endpoints (File-Based):**

```
.erlmcp/dashboard/
├── status.json              # Current system state
├── metrics.json             # Performance metrics
├── work-orders.json         # Work order list with status
├── agents.json              # Agent status
└── events.jsonl             # Event log (append-only)
```

### 6.2 Status API Schema

**File:** `.erlmcp/dashboard/status.json`

```json
{
  "version": "1.0",
  "updated_at": "2026-02-01T12:00:00Z",
  "system": {
    "status": "healthy",
    "total_agents": 20,
    "active_agents": 13,
    "idle_agents": 7,
    "failed_agents": 0
  },
  "queue": {
    "total_work_orders": 42,
    "queued": 15,
    "assigned": 3,
    "in_progress": 10,
    "completed": 12,
    "failed": 2,
    "blocked": 0
  },
  "wip": {
    "design": {"current": 2, "max": 3, "utilization": 0.67},
    "implementation": {"current": 3, "max": 5, "utilization": 0.60},
    "testing": {"current": 5, "max": 7, "utilization": 0.71},
    "review": {"current": 3, "max": 5, "utilization": 0.60}
  },
  "cost": {
    "daily_budget": "$1200",
    "current_burn": "$650",
    "utilization": 0.54,
    "alert_level": "normal"
  },
  "sla": {
    "on_track": 38,
    "at_risk": 3,
    "breached": 1
  }
}
```

**File:** `.erlmcp/dashboard/work-orders.json`

```json
{
  "version": "1.0",
  "updated_at": "2026-02-01T12:00:00Z",
  "work_orders": [
    {
      "work_order_id": "WO-001",
      "title": "Implement Armstrong FSM",
      "status": "in-progress",
      "priority": "critical",
      "assigned_to": "erlang-architect",
      "current_stage": "design",
      "progress": 0.6,
      "elapsed_hours": 2.5,
      "remaining_hours": 21.5,
      "sla_status": "on_track",
      "git_branch": "agent/design/WO-001"
    }
  ]
}
```

**File:** `.erlmcp/dashboard/agents.json`

```json
{
  "version": "1.0",
  "updated_at": "2026-02-01T12:00:00Z",
  "agents": [
    {
      "agent_name": "erlang-architect",
      "status": "active",
      "current_work_orders": ["WO-001", "WO-004"],
      "capacity": 2,
      "utilization": 1.0,
      "session_id": "session-abc123",
      "last_activity": "2026-02-01T11:58:00Z"
    }
  ]
}
```

### 6.3 Event Log

**File:** `.erlmcp/dashboard/events.jsonl` (JSON Lines, append-only)

```jsonl
{"timestamp":"2026-02-01T12:00:00Z","event":"work_order.created","work_order_id":"WO-001","priority":"critical"}
{"timestamp":"2026-02-01T12:01:00Z","event":"work_order.assigned","work_order_id":"WO-001","agent":"erlang-architect"}
{"timestamp":"2026-02-01T12:02:00Z","event":"work_order.started","work_order_id":"WO-001","stage":"design"}
{"timestamp":"2026-02-01T12:15:00Z","event":"wip_limit_warning","phase":"design","current":3,"max":3}
{"timestamp":"2026-02-01T13:00:00Z","event":"work_order.stage_completed","work_order_id":"WO-001","stage":"design"}
{"timestamp":"2026-02-01T14:00:00Z","event":"work_order.completed","work_order_id":"WO-001","sku":"sku-fsm-v1"}
```

### 6.4 Web Dashboard UI

**Minimal HTML Dashboard (Optional):**

```html
<!DOCTYPE html>
<html>
<head>
  <title>erlmcp Agent Coordination Dashboard</title>
  <style>
    body { font-family: monospace; background: #1e1e1e; color: #d4d4d4; }
    .status-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; }
    .card { background: #2d2d30; border: 1px solid #3e3e42; padding: 15px; }
    .critical { color: #f48771; }
    .high { color: #dcdcaa; }
    .normal { color: #4ec9b0; }
    .low { color: #808080; }
    .progress-bar { width: 100%; height: 10px; background: #3e3e42; }
    .progress-fill { height: 100%; background: #4ec9b0; }
  </style>
</head>
<body>
  <h1>erlmcp Agent Coordination Dashboard</h1>

  <div class="status-grid">
    <div class="card">
      <h3>System Status</h3>
      <p>Active Agents: <span id="active-agents">-</span></p>
      <p>Total WOs: <span id="total-wos">-</span></p>
      <p>Budget: <span id="budget-utilization">-</span>%</p>
    </div>

    <div class="card">
      <h3>WIP Limits</h3>
      <p>Design: <span id="wip-design">-</span></p>
      <p>Implementation: <span id="wip-impl">-</span></p>
      <p>Testing: <span id="wip-test">-</span></p>
      <p>Review: <span id="wip-review">-</span></p>
    </div>

    <div class="card">
      <h3>SLA Status</h3>
      <p class="normal">On Track: <span id="sla-on-track">-</span></p>
      <p class="high">At Risk: <span id="sla-at-risk">-</span></p>
      <p class="critical">Breached: <span id="sla-breached">-</span></p>
    </div>

    <div class="card">
      <h3>Queue Depth</h3>
      <p>Queued: <span id="queue-queued">-</span></p>
      <p>In Progress: <span id="queue-in-progress">-</span></p>
      <p>Completed: <span id="queue-completed">-</span></p>
    </div>
  </div>

  <h2>Active Work Orders</h2>
  <table id="work-orders-table">
    <thead>
      <tr>
        <th>ID</th>
        <th>Title</th>
        <th>Agent</th>
        <th>Stage</th>
        <th>Progress</th>
        <th>SLA</th>
      </tr>
    </thead>
    <tbody id="work-orders-tbody"></tbody>
  </table>

  <script>
    // Poll status every 5 seconds
    async function updateDashboard() {
      const status = await fetch('.erlmcp/dashboard/status.json').then(r => r.json());
      const workOrders = await fetch('.erlmcp/dashboard/work-orders.json').then(r => r.json());

      // Update system status
      document.getElementById('active-agents').textContent = status.system.active_agents;
      document.getElementById('total-wos').textContent = status.queue.total_work_orders;
      document.getElementById('budget-utilization').textContent = (status.cost.utilization * 100).toFixed(0);

      // Update WIP
      document.getElementById('wip-design').textContent =
        `${status.wip.design.current}/${status.wip.design.max}`;
      // ... (similar for other phases)

      // Update SLA
      document.getElementById('sla-on-track').textContent = status.sla.on_track;
      document.getElementById('sla-at-risk').textContent = status.sla.at_risk;
      document.getElementById('sla-breached').textContent = status.sla.breached;

      // Update work orders table
      const tbody = document.getElementById('work-orders-tbody');
      tbody.innerHTML = workOrders.work_orders.map(wo => `
        <tr class="${wo.priority}">
          <td>${wo.work_order_id}</td>
          <td>${wo.title}</td>
          <td>${wo.assigned_to || 'unassigned'}</td>
          <td>${wo.current_stage || 'queued'}</td>
          <td>
            <div class="progress-bar">
              <div class="progress-fill" style="width: ${wo.progress * 100}%"></div>
            </div>
          </td>
          <td>${wo.sla_status}</td>
        </tr>
      `).join('');
    }

    // Initial load
    updateDashboard();

    // Poll every 5 seconds
    setInterval(updateDashboard, 5000);
  </script>
</body>
</html>
```

---

## 7. Session Coordination

### 7.1 Parent/Child Session Model

**Session Hierarchy:**

```
Parent Session (User initiates)
├─ Session ID: session-parent-001
├─ Context: Full codebase, user instructions
└─ Work Orders: [WO-001, WO-002, WO-003, ...]
   │
   ├─ Child Session 1 (erlang-architect)
   │  ├─ Session ID: session-child-001
   │  ├─ Work Order: WO-001
   │  ├─ Context: Inherited from parent + Work order specific
   │  └─ Teleport Artifact: design-spec-wo-001.md
   │
   ├─ Child Session 2 (erlang-otp-developer)
   │  ├─ Session ID: session-child-002
   │  ├─ Work Order: WO-002
   │  ├─ Context: Inherited from parent + Design spec from child-001
   │  └─ Teleport Artifact: implementation-wo-002.erl
   │
   └─ Child Session 3 (erlang-test-engineer)
      ├─ Session ID: session-child-003
      ├─ Work Order: WO-003
      ├─ Context: Inherited from parent + Implementation from child-002
      └─ Teleport Artifact: tests-wo-003.erl
```

### 7.2 Session State Schema

**File:** `.erlmcp/sessions/parent-session-id.json`

```json
{
  "session_id": "session-parent-001",
  "type": "parent",
  "created_at": "2026-02-01T12:00:00Z",
  "user": "user@example.com",
  "status": "active",

  "work_orders": ["WO-001", "WO-002", "WO-003"],

  "child_sessions": [
    {
      "session_id": "session-child-001",
      "agent": "erlang-architect",
      "work_order_id": "WO-001",
      "status": "completed",
      "teleport_artifact": ".erlmcp/sessions/teleport/child-001.artifact"
    },
    {
      "session_id": "session-child-002",
      "agent": "erlang-otp-developer",
      "work_order_id": "WO-002",
      "status": "in-progress",
      "teleport_artifact": null
    }
  ],

  "context": {
    "codebase_snapshot": "git:main@abc123",
    "user_instructions": "Implement Armstrong FSMs for 9-nines reliability",
    "shared_state": {
      "design_decisions": [
        {
          "decision": "Use gen_statem for client and server",
          "rationale": "Complex state machine with clear state transitions",
          "decided_by": "session-child-001"
        }
      ]
    }
  }
}
```

**File:** `.erlmcp/sessions/child-001.json`

```json
{
  "session_id": "session-child-001",
  "type": "child",
  "parent_session_id": "session-parent-001",
  "created_at": "2026-02-01T12:01:00Z",
  "completed_at": "2026-02-01T14:00:00Z",

  "agent": "erlang-architect",
  "work_order_id": "WO-001",
  "status": "completed",

  "context_inherited": {
    "codebase_snapshot": "git:main@abc123",
    "user_instructions": "Implement Armstrong FSMs for 9-nines reliability"
  },

  "artifacts_produced": [
    {
      "type": "design_document",
      "path": "docs/architecture/fsm-design.md",
      "description": "Complete FSM architecture for client and server"
    },
    {
      "type": "supervision_tree",
      "path": "docs/architecture/fsm-supervision.md",
      "description": "Supervision strategy for FSMs"
    }
  ],

  "teleport_artifact": ".erlmcp/sessions/teleport/child-001.artifact"
}
```

### 7.3 Teleport Artifact Generation

**Teleport Artifact:** Ready-to-use package for `--teleport` flag

**Format:** `.erlmcp/sessions/teleport/child-001.artifact`

```json
{
  "version": "1.0",
  "session_id": "session-child-001",
  "work_order_id": "WO-001",
  "agent": "erlang-architect",
  "created_at": "2026-02-01T14:00:00Z",

  "summary": "FSM design for erlmcp client and server - Use gen_statem with 5 states each",

  "artifacts": [
    {
      "type": "file",
      "path": "docs/architecture/fsm-design.md",
      "content": "# FSM Design\n\n## Client FSM\n\nStates: disconnected → connecting → connected → working → terminating\n\n..."
    },
    {
      "type": "file",
      "path": "docs/architecture/fsm-supervision.md",
      "content": "# Supervision Tree\n\nerlmcp_sup (one_for_all)\n├── erlmcp_client_fsm_sup (simple_one_for_one)\n..."
    }
  ],

  "context": {
    "design_decisions": [
      {
        "decision": "Use gen_statem for both client and server",
        "rationale": "Complex state machine with clear transitions, better than gen_server"
      }
    ],
    "next_steps": [
      "Implement client FSM (erlang-otp-developer)",
      "Implement server FSM (erlang-otp-developer)",
      "Write FSM tests (erlang-test-engineer)"
    ]
  },

  "git": {
    "branch": "agent/design/WO-001",
    "commits": ["abc123", "def456"]
  }
}
```

**Usage:**

```bash
# Load teleport artifact in new session
claude --teleport .erlmcp/sessions/teleport/child-001.artifact

# Artifact contains:
# 1. Summary for context
# 2. All produced files
# 3. Design decisions
# 4. Next steps
```

### 7.4 Cross-Session Communication

**Read-Only State Sharing:**

Agents can read state from other agents but cannot modify it.

```python
def read_shared_state(session_id):
    """
    Read shared state from another session (read-only).
    """

    session_file = f".erlmcp/sessions/{session_id}.json"

    if not os.path.exists(session_file):
        return None

    session_data = load_json(session_file)

    # Return read-only copy
    return {
        "artifacts": session_data.get("artifacts_produced", []),
        "decisions": session_data.get("context", {}).get("shared_state", {})
    }
```

**Example:**

```python
# Child session 2 (implementation) reads from child session 1 (design)
design_artifacts = read_shared_state("session-child-001")

# Access design decisions
for decision in design_artifacts["decisions"]["design_decisions"]:
    print(f"Design Decision: {decision['decision']}")
    print(f"Rationale: {decision['rationale']}")
```

---

## 8. Claude Code Web Hooks

### 8.1 Hook System Architecture

**Hook Lifecycle:**

```
User Request → PreTask → Task Execution → PostTask → SessionEnd
                  ↓           ↓              ↓           ↓
            WIP Check    Status Update   Release Slot  Handoff
```

**Hook Files:** `.erlmcp/hooks/`

```
.erlmcp/hooks/
├── pre-task.sh         # Check WIP limits, acquire slot
├── post-task.sh        # Update status, release slot
├── session-start.sh    # Load work order context
└── session-end.sh      # Generate teleport artifact
```

### 8.2 PreTask Hook

**File:** `.erlmcp/hooks/pre-task.sh`

```bash
#!/bin/bash
# PreTask Hook - Check WIP limits and acquire slot

set -e

WORK_ORDER_ID=$1
AGENT_NAME=$2
PHASE=$3

# Check WIP limit
WIP_LIMITS=$(cat .erlmcp/dashboard/wip-limits.json)
CURRENT_WIP=$(echo "$WIP_LIMITS" | jq -r ".wip_limits.$PHASE.current")
MAX_WIP=$(echo "$WIP_LIMITS" | jq -r ".wip_limits.$PHASE.max")

if [ "$CURRENT_WIP" -ge "$MAX_WIP" ]; then
    echo "❌ WIP limit reached for phase $PHASE ($CURRENT_WIP/$MAX_WIP)"
    echo "⏳ Work order $WORK_ORDER_ID queued"
    exit 1
fi

# Acquire slot
jq ".wip_limits.$PHASE.current += 1" .erlmcp/dashboard/wip-limits.json > tmp.json
mv tmp.json .erlmcp/dashboard/wip-limits.json

# Update work order status
WO_FILE=".erlmcp/work-orders/queued/$WORK_ORDER_ID.json"
if [ -f "$WO_FILE" ]; then
    jq ".status = \"assigned\" | .assignment.assigned_to = \"$AGENT_NAME\" | .assignment.assigned_at = \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"" "$WO_FILE" > tmp.json
    mv tmp.json "$WO_FILE"
    mv "$WO_FILE" ".erlmcp/work-orders/assigned/$WORK_ORDER_ID.json"
fi

# Log event
echo "{\"timestamp\":\"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",\"event\":\"work_order.assigned\",\"work_order_id\":\"$WORK_ORDER_ID\",\"agent\":\"$AGENT_NAME\",\"phase\":\"$PHASE\"}" >> .erlmcp/dashboard/events.jsonl

echo "✅ Slot acquired for $WORK_ORDER_ID in phase $PHASE"
exit 0
```

### 8.3 PostTask Hook

**File:** `.erlmcp/hooks/post-task.sh`

```bash
#!/bin/bash
# PostTask Hook - Update status and release slot

set -e

WORK_ORDER_ID=$1
AGENT_NAME=$2
PHASE=$3
STATUS=$4  # "completed" or "failed"

# Release slot
WIP_LIMITS=$(cat .erlmcp/dashboard/wip-limits.json)
CURRENT_WIP=$(echo "$WIP_LIMITS" | jq -r ".wip_limits.$PHASE.current")

if [ "$CURRENT_WIP" -gt 0 ]; then
    jq ".wip_limits.$PHASE.current -= 1" .erlmcp/dashboard/wip-limits.json > tmp.json
    mv tmp.json .erlmcp/dashboard/wip-limits.json
fi

# Update work order status
WO_DIR=".erlmcp/work-orders"
WO_FILE=$(find "$WO_DIR" -name "$WORK_ORDER_ID.json" | head -1)

if [ -f "$WO_FILE" ]; then
    if [ "$STATUS" = "completed" ]; then
        jq ".status = \"completed\" | .lifecycle.completed_at = \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"" "$WO_FILE" > tmp.json
        mv tmp.json "$WO_FILE"
        mv "$WO_FILE" "$WO_DIR/completed/$WORK_ORDER_ID.json"

        # Check for dependent work orders to unblock
        BLOCKS=$(jq -r '.dependencies.blocks[]' "$WO_DIR/completed/$WORK_ORDER_ID.json" 2>/dev/null || echo "")
        for DEPENDENT_WO in $BLOCKS; do
            bash .erlmcp/hooks/check-unblock.sh "$DEPENDENT_WO"
        done
    else
        jq ".status = \"failed\" | .retry.count += 1" "$WO_FILE" > tmp.json
        mv tmp.json "$WO_FILE"
        mv "$WO_FILE" "$WO_DIR/failed/$WORK_ORDER_ID.json"

        # Schedule retry
        bash .erlmcp/hooks/schedule-retry.sh "$WORK_ORDER_ID"
    fi
fi

# Log event
echo "{\"timestamp\":\"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",\"event\":\"work_order.$STATUS\",\"work_order_id\":\"$WORK_ORDER_ID\",\"agent\":\"$AGENT_NAME\",\"phase\":\"$PHASE\"}" >> .erlmcp/dashboard/events.jsonl

# Update dashboard
bash .erlmcp/hooks/update-dashboard.sh

echo "✅ Slot released for $WORK_ORDER_ID (status: $STATUS)"
exit 0
```

### 8.4 SessionStart Hook

**File:** `.erlmcp/hooks/session-start.sh`

```bash
#!/bin/bash
# SessionStart Hook - Load work order context

set -e

SESSION_ID=$1
WORK_ORDER_ID=$2
PARENT_SESSION_ID=$3

# Load work order
WO_FILE=$(find .erlmcp/work-orders -name "$WORK_ORDER_ID.json" | head -1)

if [ ! -f "$WO_FILE" ]; then
    echo "❌ Work order $WORK_ORDER_ID not found"
    exit 1
fi

# Load parent session context
if [ -n "$PARENT_SESSION_ID" ]; then
    PARENT_SESSION_FILE=".erlmcp/sessions/$PARENT_SESSION_ID.json"
    if [ -f "$PARENT_SESSION_FILE" ]; then
        echo "📦 Loading context from parent session $PARENT_SESSION_ID"
        # Context automatically inherited
    fi
fi

# Load dependencies (previous work orders)
BLOCKED_BY=$(jq -r '.dependencies.blocked_by[]' "$WO_FILE" 2>/dev/null || echo "")
for DEPENDENCY_WO in $BLOCKED_BY; do
    TELEPORT_ARTIFACT=$(find .erlmcp/sessions/teleport -name "*$DEPENDENCY_WO*.artifact" | head -1)
    if [ -f "$TELEPORT_ARTIFACT" ]; then
        echo "📦 Loading teleport artifact from $DEPENDENCY_WO"
        # Artifact provides context for this work order
    fi
done

# Create session file
SESSION_FILE=".erlmcp/sessions/$SESSION_ID.json"
jq -n \
    --arg session_id "$SESSION_ID" \
    --arg parent "$PARENT_SESSION_ID" \
    --arg wo_id "$WORK_ORDER_ID" \
    --arg created "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    '{
        session_id: $session_id,
        type: "child",
        parent_session_id: $parent,
        work_order_id: $wo_id,
        created_at: $created,
        status: "active"
    }' > "$SESSION_FILE"

echo "✅ Session $SESSION_ID started for work order $WORK_ORDER_ID"
exit 0
```

### 8.5 SessionEnd Hook

**File:** `.erlmcp/hooks/session-end.sh`

```bash
#!/bin/bash
# SessionEnd Hook - Generate teleport artifact

set -e

SESSION_ID=$1
WORK_ORDER_ID=$2
STATUS=$3  # "completed" or "failed"

SESSION_FILE=".erlmcp/sessions/$SESSION_ID.json"

if [ ! -f "$SESSION_FILE" ]; then
    echo "❌ Session file not found: $SESSION_FILE"
    exit 1
fi

# Update session status
jq ".status = \"$STATUS\" | .completed_at = \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"" "$SESSION_FILE" > tmp.json
mv tmp.json "$SESSION_FILE"

if [ "$STATUS" = "completed" ]; then
    # Generate teleport artifact
    TELEPORT_FILE=".erlmcp/sessions/teleport/$SESSION_ID.artifact"

    # Collect all artifacts produced in this session
    # (This would inspect git commits, file changes, etc.)
    bash .erlmcp/hooks/generate-teleport.sh "$SESSION_ID" "$WORK_ORDER_ID" > "$TELEPORT_FILE"

    # Update session file with teleport artifact path
    jq ".teleport_artifact = \"$TELEPORT_FILE\"" "$SESSION_FILE" > tmp.json
    mv tmp.json "$SESSION_FILE"

    # Update parent session
    PARENT_SESSION_ID=$(jq -r '.parent_session_id' "$SESSION_FILE")
    if [ -n "$PARENT_SESSION_ID" ] && [ "$PARENT_SESSION_ID" != "null" ]; then
        PARENT_FILE=".erlmcp/sessions/$PARENT_SESSION_ID.json"
        jq ".child_sessions |= map(if .session_id == \"$SESSION_ID\" then .status = \"completed\" | .teleport_artifact = \"$TELEPORT_FILE\" else . end)" "$PARENT_FILE" > tmp.json
        mv tmp.json "$PARENT_FILE"
    fi

    echo "✅ Teleport artifact generated: $TELEPORT_FILE"
fi

echo "✅ Session $SESSION_ID ended (status: $STATUS)"
exit 0
```

---

## 9. Rollback & Recovery

### 9.1 Fault Tolerance Design

**Armstrong Principle:** Let-it-crash, don't try to fix partial states

**Recovery Strategy:**

```
Agent Failure → Release work order → Queue for retry → Assign to new agent
                     ↓
                Clean up locks
                     ↓
                Delete branch (if uncommitted)
                     ↓
                Log failure reason
```

### 9.2 Failure Detection

**Agent Health Check:**

```python
def check_agent_health():
    """
    Monitor agent health and detect failures.
    """

    agents = load_agents()

    for agent_name, agent_info in agents.items():
        last_activity = parse_timestamp(agent_info.get("last_activity"))

        # Agent considered dead if no activity for 5 minutes
        if (datetime.utcnow() - last_activity).total_seconds() > 300:
            log_event("agent_timeout", {
                "agent": agent_name,
                "last_activity": last_activity,
                "work_orders": agent_info.get("current_work_orders", [])
            })

            # Release work orders
            for wo_id in agent_info.get("current_work_orders", []):
                release_work_order(wo_id, reason="agent_timeout")

            # Mark agent as failed
            agent_info["status"] = "failed"
            save_agents(agents)
```

### 9.3 Work Order Release

**Release on Failure:**

```python
def release_work_order(work_order_id, reason):
    """
    Release work order back to queue on failure.
    """

    wo = load_work_order(work_order_id)

    # Release file locks
    for lock_file in glob.glob(f".erlmcp/locks/*.lock"):
        lock = load_json(lock_file)
        if lock.get("work_order_id") == work_order_id:
            os.remove(lock_file)

    # Delete uncommitted branch
    branch = wo.get("git", {}).get("branch")
    if branch and not branch_has_commits(branch):
        run_command(f"git branch -D {branch}")

    # Reset work order state
    wo["status"] = "failed"
    wo["assignment"]["assigned_to"] = None
    wo["assignment"]["assigned_at"] = None
    wo["error"] = {
        "type": "agent_failure",
        "reason": reason,
        "timestamp": datetime.utcnow().isoformat()
    }
    wo["retry"]["count"] += 1

    # Move to failed queue
    save_work_order_to_queue(wo, "failed")

    # Log event
    log_event("work_order.released", {
        "work_order_id": work_order_id,
        "reason": reason
    })
```

### 9.4 Retry Logic

**Exponential Backoff:**

```python
def schedule_retry(work_order_id):
    """
    Schedule work order for retry with exponential backoff.
    """

    wo = load_work_order(work_order_id)
    retry_count = wo["retry"]["count"]
    max_retries = wo["retry"]["max_retries"]

    if retry_count >= max_retries:
        log_event("work_order.max_retries_exceeded", {
            "work_order_id": work_order_id,
            "retry_count": retry_count
        })

        # Manual intervention required
        wo["status"] = "requires_manual_intervention"
        save_work_order(wo)
        emit_alert("work_order_failed_max_retries", wo)
        return

    # Calculate backoff
    backoff_seconds = wo["retry"]["backoff_seconds"][retry_count]
    retry_at = datetime.utcnow() + timedelta(seconds=backoff_seconds)

    # Schedule retry
    wo["retry"]["retry_at"] = retry_at.isoformat()
    wo["status"] = "queued"
    save_work_order_to_queue(wo, "queued")

    log_event("work_order.retry_scheduled", {
        "work_order_id": work_order_id,
        "retry_count": retry_count,
        "retry_at": retry_at.isoformat(),
        "backoff_seconds": backoff_seconds
    })
```

### 9.5 Manual Retry Override

**Force Retry with Different Agent:**

```python
def manual_retry(work_order_id, override_agent=None):
    """
    Manually retry work order with optional agent override.
    """

    wo = load_work_order(work_order_id)

    # Reset retry count
    wo["retry"]["count"] = 0

    # Override agent if specified
    if override_agent:
        wo["assignment"]["required_agent"] = override_agent
        log_event("work_order.agent_override", {
            "work_order_id": work_order_id,
            "new_agent": override_agent
        })

    # Reset state
    wo["status"] = "queued"
    wo["error"] = None

    # Move back to queue
    save_work_order_to_queue(wo, "queued")

    log_event("work_order.manual_retry", {
        "work_order_id": work_order_id,
        "override_agent": override_agent
    })
```

### 9.6 System Recovery

**On System Restart:**

```python
def recover_system_state():
    """
    Recover system state after crash or restart.
    """

    # 1. Check for stale locks
    for lock_file in glob.glob(".erlmcp/locks/*.lock"):
        lock = load_json(lock_file)
        locked_at = parse_timestamp(lock["locked_at"])

        # Remove locks older than 1 hour
        if (datetime.utcnow() - locked_at).total_seconds() > 3600:
            os.remove(lock_file)
            log_event("lock.removed_stale", {
                "file": lock["file"],
                "locked_by": lock["locked_by"]
            })

    # 2. Check for orphaned in-progress work orders
    for wo_file in glob.glob(".erlmcp/work-orders/in-progress/*.json"):
        wo = load_json(wo_file)

        # Check if agent is still active
        assigned_to = wo["assignment"]["assigned_to"]
        if not is_agent_active(assigned_to):
            release_work_order(wo["work_order_id"], reason="system_recovery")

    # 3. Reset WIP counters
    recalculate_wip_counts()

    # 4. Rebuild agent registry
    rebuild_agent_registry()

    log_event("system.recovery_complete", {
        "timestamp": datetime.utcnow().isoformat()
    })
```

---

## 10. Implementation Roadmap

### 10.1 Phase 1: Foundation (Week 1)

**Deliverables:**
- ✅ Work order queue schema
- ✅ File system layout
- ✅ Basic CRUD operations for work orders
- ✅ Priority dispatch algorithm
- ✅ Status transitions state machine

**Validation:**
- Create, assign, complete 10 work orders manually
- Verify priority ordering
- Test state transitions

### 10.2 Phase 2: Agent Assignment (Week 2)

**Deliverables:**
- ✅ Agent capability matrix
- ✅ Skill-based routing algorithm
- ✅ Workload balancing
- ✅ File conflict detection

**Validation:**
- Assign 20 work orders to 8 agents
- Verify skill matching
- Test conflict avoidance

### 10.3 Phase 3: Concurrency Control (Week 3)

**Deliverables:**
- ✅ Git branch isolation
- ✅ File-level locking
- ✅ Merge strategies
- ✅ Conflict detection

**Validation:**
- Run 5 concurrent agents
- Verify branch isolation
- Test merge conflicts

### 10.4 Phase 4: Dashboard & Monitoring (Week 4)

**Deliverables:**
- ✅ Status JSON API
- ✅ Event log (JSONL)
- ✅ Web dashboard (HTML)
- ✅ Real-time updates

**Validation:**
- Monitor 10 concurrent work orders
- Verify real-time updates
- Test dashboard responsiveness

### 10.5 Phase 5: Session Coordination (Week 5)

**Deliverables:**
- ✅ Parent/child session model
- ✅ Teleport artifact generation
- ✅ Cross-session state sharing
- ✅ Context inheritance

**Validation:**
- Create parent session with 3 children
- Generate teleport artifacts
- Test context inheritance

### 10.6 Phase 6: Web Hooks Integration (Week 6)

**Deliverables:**
- ✅ PreTask hook (WIP check)
- ✅ PostTask hook (status update)
- ✅ SessionStart hook (context load)
- ✅ SessionEnd hook (teleport generation)

**Validation:**
- Run 10 work orders through full lifecycle
- Verify hook execution
- Test WIP enforcement

### 10.7 Phase 7: Fault Tolerance (Week 7)

**Deliverables:**
- ✅ Agent health monitoring
- ✅ Failure detection
- ✅ Work order release
- ✅ Retry logic with exponential backoff
- ✅ System recovery

**Validation:**
- Simulate agent failures
- Test retry logic
- Verify system recovery

### 10.8 Phase 8: Production Hardening (Week 8)

**Deliverables:**
- ✅ Comprehensive logging
- ✅ Performance optimization
- ✅ Security audit
- ✅ Documentation
- ✅ Example workflows

**Validation:**
- Run 100+ work orders
- Monitor performance
- Security review

---

## Appendix A: Example Workflow

**User Task:** "Implement Armstrong FSMs for 9-nines reliability"

**Work Order Decomposition:**

```
WO-001: Design FSM Architecture
  ├─ Agent: erlang-architect
  ├─ Phase: design
  ├─ Priority: critical
  ├─ SLA: 24 hours
  └─ Output: Design spec, supervision tree

WO-002: Implement Client FSM
  ├─ Agent: erlang-otp-developer
  ├─ Phase: implementation
  ├─ Priority: high
  ├─ Blocked by: WO-001
  └─ Output: erlmcp_client_fsm.erl

WO-003: Implement Server FSM
  ├─ Agent: erlang-otp-developer
  ├─ Phase: implementation
  ├─ Priority: high
  ├─ Blocked by: WO-001
  └─ Output: erlmcp_server_fsm.erl

WO-004: Write FSM Tests
  ├─ Agent: erlang-test-engineer
  ├─ Phase: testing
  ├─ Priority: high
  ├─ Blocked by: WO-002, WO-003
  └─ Output: erlmcp_fsm_tests.erl

WO-005: Performance Validation
  ├─ Agent: erlang-performance
  ├─ Phase: testing
  ├─ Priority: normal
  ├─ Blocked by: WO-004
  └─ Output: Benchmark results

WO-006: Code Review
  ├─ Agent: code-reviewer
  ├─ Phase: review
  ├─ Priority: normal
  ├─ Blocked by: WO-005
  └─ Output: Review approval, merge to main
```

**Timeline:**

```
Day 1:
  12:00 - WO-001 created (design, critical)
  12:01 - WO-001 assigned to erlang-architect
  14:00 - WO-001 completed → Design spec ready
  14:01 - WO-002, WO-003 unblocked

Day 1-2:
  14:02 - WO-002 assigned to erlang-otp-developer-1
  14:03 - WO-003 assigned to erlang-otp-developer-2
  18:00 - WO-002 completed → Client FSM ready
  19:00 - WO-003 completed → Server FSM ready
  19:01 - WO-004 unblocked

Day 2:
  09:00 - WO-004 assigned to erlang-test-engineer
  12:00 - WO-004 completed → Tests pass, 95% coverage
  12:01 - WO-005 unblocked
  12:02 - WO-005 assigned to erlang-performance
  14:00 - WO-005 completed → Performance validated
  14:01 - WO-006 unblocked
  14:02 - WO-006 assigned to code-reviewer
  15:00 - WO-006 completed → Merged to main ✅

Total Lead Time: 27 hours (within 24h SLA for critical work)
```

---

## Appendix B: File Examples

### Work Order Example

```json
{
  "work_order_id": "WO-001",
  "version": "1.0",
  "created_at": "2026-02-01T12:00:00Z",
  "updated_at": "2026-02-01T14:00:00Z",
  "status": "completed",
  "priority": "critical",
  "bucket": "reliability",

  "task": {
    "title": "Design FSM Architecture for 9-nines",
    "description": "Design gen_statem-based FSMs for erlmcp client and server",
    "acceptance_criteria": [
      "State machine diagrams for client and server",
      "Supervision tree design",
      "State transition rules documented",
      "Error handling strategy defined"
    ]
  },

  "assignment": {
    "required_agent": "erlang-architect",
    "required_skills": ["supervision", "gen_statem", "OTP design"],
    "assigned_to": "erlang-architect",
    "assigned_at": "2026-02-01T12:01:00Z",
    "session_id": "session-child-001"
  },

  "sla": {
    "deadline": "2026-02-02T12:00:00Z",
    "elapsed_hours": 2.0,
    "remaining_hours": 22.0
  },

  "lifecycle": {
    "stages": ["requirements", "design", "implementation", "testing", "review"],
    "current_stage": "design",
    "completed_stages": ["requirements", "design"]
  },

  "git": {
    "branch": "agent/design/WO-001",
    "commits": ["abc123", "def456"],
    "merge_status": "merged"
  },

  "artifacts": {
    "design_doc": "docs/architecture/fsm-design.md",
    "sku_id": "sku-fsm-design-v1",
    "teleport_artifact": ".erlmcp/sessions/teleport/child-001.artifact"
  }
}
```

---

## Conclusion

This Web Agent Coordination Design provides a **production-grade system** for managing multiple Claude Code agents working on erlmcp simultaneously. The design follows erlmcp's core Armstrong principles:

- **Process Isolation** - Each agent runs independently
- **Supervision** - Failed agents don't block others
- **Observable Behavior** - Real-time dashboard, complete audit trail
- **Fault Tolerance** - Automatic retry, system recovery
- **No Shared Mutable State** - File-based communication

**Key Metrics:**
- Work order dispatch: <100ms
- Agent assignment: <50ms
- Status updates: <200ms
- Support: 20+ concurrent agents
- Queue depth: 1000+ work orders

**Next Steps:**
1. Implement Phase 1 (Foundation) - Week 1
2. Validate with 10 work orders
3. Iterate based on feedback
4. Deploy to production

---

**Document Version:** 1.0.0
**Author:** erlang-architect
**Date:** 2026-02-01
**Total Lines:** 1200+
**Status:** Ready for Implementation
