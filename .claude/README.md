# .claude/ Configuration

Claude Code configuration for erlmcp v2.1.0.

**Primary Source**: See `/CLAUDE.md` for all project rules, OTP patterns, and workflows. This is the single source of truth.

## Directory Structure

```
.claude/
├── settings.json      # Session config (permissions, hooks, environment, EPIC 9)
├── agents/            # 10 core agents (YAML frontmatter, loaded on-demand)
├── skills/            # Domain skills (loaded when invoked)
├── hooks/             # Pre/post execution hooks (auto-loaded)
└── README.md          # This file
```

## The 10 Core Agents

| Agent | Purpose | When to Use |
|-------|---------|-------------|
| **erlang-otp-developer** | OTP behaviors (gen_server, supervisor, application) | Implementing OTP modules |
| **erlang-transport-builder** | Transport layer (stdio, tcp, http, ws, sse) | Adding new transports |
| **erlang-test-engineer** | Chicago TDD tests (EUnit, CT, Proper) | Writing tests first |
| **erlang-researcher** | Codebase research and context | Exploring codebase |
| **erlang-architect** | System architecture and OTP design | Designing supervision trees |
| **erlang-performance** | Benchmarking and optimization | Measuring performance |
| **code-reviewer** | Quality gates and validation | Pre-completion checks |
| **erlang-github-ops** | Git, PR, CI/CD workflows | Managing releases |
| **sparc-orchestrator** | SPARC methodology coordination | Complex feature development |
| **plan-designer** | Implementation planning | Research → Plan → Execute |

## Domain Skills

| Skill | Purpose |
|-------|---------|
| **otp-manager** | Manage Erlang/OTP installation and verification |
| **chicago-tdd-erlang** | Chicago School TDD for Erlang |
| **sparc** | SPARC methodology workflows |

## Key Settings

### Parallel Execution (GOLDEN RULE)
```
tools_and_model:
  prefer_parallel_calls: true
  max_parallel_tools: 10
```
**Always batch operations in single message**: 10+ todos, spawn 10+ agents together.

### EPIC 9
```json
epic9:
  enabled: true
  default_agents: 10
  mandatory_phases: [fan_out, independent_construction, collision_detection, convergence, refactoring, closure]
  speedup_target: "2.8x-4.4x"
```

### Andon Signals
- 🔴 **RED**: `error[E`, `FAILED`, `panicked` → STOP immediately
- 🟡 **YELLOW**: `warning:`, `deprecated:`, `TODO` → Investigate
- 🟢 **GREEN**: `test result: ok`, `0 violations` → Continue

### SLO Targets
| Command | Target |
|---------|--------|
| rebar3_compile | <30s |
| rebar3_eunit | <60s |
| rebar3_ct | <120s |
| rebar3_dialyzer | <90s |
| make_check | <180s (parallel) |

## Hooks

### Session Lifecycle
- **SessionStart**: Bootstrap OTP 28.3.1+ + environment prompt
- **Stop**: Pre-stop validation reminder
- **SessionEnd**: Generate receipt + archive transcript

### Tool Safety
- **PreToolUse**: Policy hooks (bash, websearch, write)
- **PostToolUse**: CI trigger (async compile+tests), git audit

### Context Management
- **PreCompact**: Re-inject critical invariants

## Usage

1. **CLAUDE.md is primary**: All project rules, workflows, OTP patterns
2. **Agents are concise**: Reference CLAUDE.md for details
3. **Hooks enforce safety**: Poka-Yoke error prevention
4. **Settings configure**: Permissions, tools, SLOs, EPIC 9

## EPIC 9 Workflow

**Trigger**: Non-trivial tasks (5+ files, 3+ systems, multiple approaches)

```
1. Fan-Out: Spawn 10 independent agents (parallel)
2. Construction: Parallel implementation
3. Collision Detection: Analyze overlap/divergence
4. Convergence: Synthesize optimal solution
5. Refactoring: DRY, type-safety, performance
6. Closure: Generate receipts, audit trail
```

## Token Budget

Total .claude/ content:
- **Before**: ~4000 lines (agents + skills + hooks)
- **After**: ~850 lines (consolidated)
- **Reduction**: 79% fewer tokens

## Version

**v3.2.0** (2026-02-01): Added EPIC 9, parallel execution, Andon signals
