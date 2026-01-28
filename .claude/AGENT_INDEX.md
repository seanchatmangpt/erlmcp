# erlmcp Agent Index

**Last Updated**: 2026-01-27
**Agent Count**: 10 core agents (consolidated from 57)
**Status**: Active (v1.0.0)

## Quick Reference

This index helps you find the right agent for any erlmcp task in under 30 seconds.

### Find Agent by Task

| When You Need To... | Use This Agent | Why |
|---------------------|----------------|-----|
| Implement gen_server, supervisor, or OTP application | `erlang-otp-developer` | Unified OTP development expert |
| Add new transport (stdio/tcp/http) | `erlang-transport-builder` | Transport layer specialist |
| Write tests (EUnit, CT, Proper, TDD) | `erlang-test-engineer` | Comprehensive testing expert |
| Explore codebase or understand architecture | `erlang-researcher` | Context preservation specialist |
| Design system architecture or supervision tree | `erlang-architect` | OTP architecture expert |
| Benchmark performance or optimize code | `erlang-performance` | Performance specialist |
| Create PR, manage Git, configure CI/CD | `erlang-github-ops` | Git workflow expert |
| Coordinate SPARC methodology workflow | `sparc-orchestrator` | SPARC coordination expert |
| Create implementation plan before coding | `plan-designer` | Research → Plan → Execute expert |
| Review code quality before completion | `code-reviewer` | Quality validation expert |

### Find Agent by File Type

| File Pattern | Auto-Assigned Agent | Trigger |
|--------------|---------------------|---------|
| `*.erl` (gen_server) | `erlang-otp-developer` | OTP behavior implementation |
| `*_transport_*.erl` | `erlang-transport-builder` | Transport layer files |
| `*_tests.erl` | `erlang-test-engineer` | Test suite files |
| `docs/*.md` | `erlang-researcher` | Documentation analysis |
| `*_sup.erl` | `erlang-architect` | Supervisor design |
| Benchmarking task | `erlang-performance` | Performance measurement |
| `.github/workflows/*` | `erlang-github-ops` | CI/CD configuration |
| SPARC workflow | `sparc-orchestrator` | Methodology coordination |
| Planning phase | `plan-designer` | Implementation planning |
| Pre-completion review | `code-reviewer` | Quality validation |

## The 10 Core Agents

### 1. erlang-otp-developer
**Path**: `.claude/agents/erlang-otp-developer.md`
**Purpose**: Unified OTP development - gen_server, supervisors, applications, library integration
**Consolidates**: gen-server-builder, supervisor-architect, library-integrator, backend-dev (4 agents)
**SPARC Phase**: Architecture
**Use When**:
- Implementing new gen_server behaviors
- Designing supervision trees
- Integrating libraries (gproc, gun, ranch, poolboy)
- Building OTP applications

**Key Files**: `src/erlmcp_server.erl`, `src/erlmcp_client.erl`, `src/erlmcp_sup.erl`, `docs/otp-patterns.md`

### 2. erlang-transport-builder
**Path**: `.claude/agents/erlang-transport-builder.md`
**Purpose**: Transport layer implementation specialist (stdio, tcp, http)
**New Agent**: Specialized for erlmcp's core transport abstraction
**SPARC Phase**: Architecture
**Use When**:
- Adding new transport types
- Implementing `-behaviour(erlmcp_transport)` callbacks
- Integrating gun (HTTP) or ranch (TCP)
- Debugging transport layer issues

**Key Files**: `src/erlmcp_transport*.erl`, `docs/v0.6.0-FINAL-PLAN.md`

### 3. erlang-test-engineer
**Path**: `.claude/agents/erlang-test-engineer.md`
**Purpose**: Comprehensive testing - EUnit, Common Test, Proper, TDD
**Consolidates**: tdd-london-swarm, production-validator, test-suite-builder (3 agents)
**SPARC Phase**: Refinement
**Use When**:
- Writing unit tests (EUnit)
- Creating integration tests (Common Test)
- Implementing property-based tests (Proper)
- Following TDD workflow
- Achieving 80%+ coverage

**Key Files**: `test/*_tests.erl`, `rebar.config` (test profiles)

### 4. erlang-researcher
**Path**: `.claude/agents/erlang-researcher.md`
**Purpose**: Codebase research and context preservation
**Consolidates**: code-analyzer, ml-researcher, smart-agent (3 agents)
**SPARC Phase**: Specification
**Model**: Haiku (optimized for fast research)
**Use When**:
- Exploring erlmcp codebase structure
- Understanding existing patterns
- Answering "how does X work?" questions
- Preserving main context by delegating research

**Key Files**: `src/`, `test/`, `docs/`, `rebar.config`

### 5. erlang-architect
**Path**: `.claude/agents/erlang-architect.md`
**Purpose**: System architecture and OTP design
**Consolidates**: system-architect, sparc-architecture, swarm-coordinator (3 agents)
**SPARC Phase**: Architecture
**Use When**:
- Designing supervision trees
- Choosing OTP behaviors (gen_server, gen_statem, supervisor)
- Making architectural decisions
- Planning module decomposition

**Key Files**: `docs/architecture.md`, `docs/otp-patterns.md`, `src/*_sup.erl`

### 6. erlang-performance
**Path**: `.claude/agents/erlang-performance.md`
**Purpose**: Benchmarking and optimization
**Consolidates**: performance-benchmarker, bottleneck-analyzer, optimizer, cache-manager, load-balancer (5 agents)
**SPARC Phase**: Refinement
**Use When**:
- Benchmarking request-response latency
- Profiling memory usage
- Identifying bottlenecks
- Optimizing hot paths

**Key Files**: Performance testing code, profiling tools

### 7. erlang-github-ops
**Path**: `.claude/agents/erlang-github-ops.md`
**Purpose**: Git workflows, PR management, CI/CD
**Consolidates**: All 13 github agents + ci-cd-engineer (14 agents)
**SPARC Phase**: Completion
**Use When**:
- Creating pull requests
- Managing Git branches
- Configuring GitHub Actions
- Managing releases
- Code review workflows

**Key Files**: `.github/workflows/*`, release scripts

### 8. sparc-orchestrator
**Path**: `.claude/agents/sparc-orchestrator.md`
**Purpose**: SPARC methodology coordination
**Consolidates**: sparc-coordinator, specification, pseudocode, architecture, refinement (5 agents)
**SPARC Phase**: All (coordinator)
**Use When**:
- Running full SPARC workflow
- Coordinating multi-phase development
- Delegating to phase-specific agents

**Key Files**: All erlmcp files (coordinates entire workflow)

### 9. plan-designer
**Path**: `.claude/agents/plan-designer.md`
**Purpose**: Implementation planning (Research → Plan → Execute)
**Consolidates**: task-orchestrator, adaptive-coordinator (2 agents)
**SPARC Phase**: Specification
**Use When**:
- Creating implementation plans before coding
- Researching requirements
- Designing approach
- Following Anthropic best practices

**Key Files**: Depends on task (research phase determines scope)

### 10. code-reviewer
**Path**: `.claude/agents/code-reviewer.md`
**Purpose**: Code quality review and pre-completion validation
**Consolidates**: code-analyzer, memory-coordinator (2 agents)
**SPARC Phase**: Refinement
**Use When**:
- Reviewing code before completion
- Validating quality gates
- Enforcing OTP patterns
- Pre-merge review

**Key Files**: All modified files in PR

## Agent Coordination Patterns

### Research → Plan → Execute Workflow

**Standard Pattern** (Anthropic Best Practice):
```
1. Research Phase: erlang-researcher
   → Gathers context, explores codebase
   → Reports summary to preserve main context

2. Plan Phase: plan-designer
   → Analyzes requirements
   → Designs implementation approach
   → Creates detailed plan

3. Execute Phase: erlang-otp-developer (or other specialist)
   → Implements according to plan
   → Follows erlmcp patterns
   → Runs quality gates before completion
```

**Example**: Adding new gen_server
```
User: "Add a new cache server using gen_server"

1. erlang-researcher (auto-spawned by plan-designer)
   - Reads: src/erlmcp_server.erl, src/erlmcp_client.erl, docs/otp-patterns.md
   - Reports: "erlmcp uses standard gen_server pattern with #state{} records..."

2. plan-designer
   - Analyzes: Cache requirements, supervision strategy
   - Decides: gen_server with ETS backend, supervised by erlmcp_sup
   - Plans: Module structure, API functions, state management

3. erlang-otp-developer
   - Implements: cache_server.erl with init/1, handle_call/3, etc.
   - Tests: Creates cache_server_tests.erl (delegates to erlang-test-engineer)
   - Validates: Runs quality gates (tests, dialyzer, xref)
```

### Subagent Pipeline Pattern

**Three-Stage Pipeline**:
```
pm-spec → architect-review → implementer-tester
```

**Maps to erlmcp agents**:
```
plan-designer → erlang-architect → erlang-otp-developer + erlang-test-engineer
```

**Example**: Implementing new transport
```
1. plan-designer (pm-spec phase)
   - Spawns: erlang-researcher to understand transport behavior
   - Plans: Transport implementation approach

2. erlang-architect (architect-review phase)
   - Reviews: Architecture decisions
   - Validates: Supervision strategy, behavior callbacks
   - Approves: Module structure

3. erlang-transport-builder + erlang-test-engineer (implementer-tester phase)
   - Implements: Transport module with callbacks
   - Tests: Creates comprehensive test suite
   - Validates: Quality gates pass
```

### Auto-Delegation Rules

Agents automatically delegate based on YAML `description` field triggers:

| Trigger Pattern | Delegates To | Reason |
|-----------------|--------------|--------|
| "research codebase" | `erlang-researcher` | Context preservation |
| "design architecture" | `erlang-architect` | System design needed |
| "implement gen_server" | `erlang-otp-developer` | OTP implementation |
| "write tests" | `erlang-test-engineer` | Testing required |
| "optimize performance" | `erlang-performance` | Benchmarking needed |
| "create pull request" | `erlang-github-ops` | Git workflow |
| "review code quality" | `code-reviewer` | Quality validation |

## SPARC Phase Mapping

Each agent maps to specific SPARC phases:

| SPARC Phase | Primary Agent | Supporting Agents |
|-------------|---------------|-------------------|
| **Specification** | `plan-designer` | `erlang-researcher` |
| **Pseudocode** | `plan-designer` | `erlang-architect` |
| **Architecture** | `erlang-architect` | `erlang-otp-developer`, `erlang-transport-builder` |
| **Refinement** | `erlang-test-engineer` | `erlang-performance`, `code-reviewer` |
| **Completion** | `code-reviewer` | `erlang-github-ops` |

**Full SPARC Workflow** coordinated by `sparc-orchestrator`:
```
sparc-orchestrator
├── Specification → plan-designer + erlang-researcher
├── Pseudocode → plan-designer
├── Architecture → erlang-architect + erlang-otp-developer
├── Refinement → erlang-test-engineer + erlang-performance
└── Completion → code-reviewer + erlang-github-ops
```

## Quality Gates

**ALL agents must pass pre-completion verification before reporting "done"**:

### Universal Quality Gates (Mandatory)
```
✅ Tests: All pass (0 failures)
  - EUnit: rebar3 eunit --module=<module>
  - CT: rebar3 ct --suite=<suite>
  - Proper: rebar3 proper -c --module=<module>

✅ Quality: All checks clean
  - Compile: rebar3 compile (0 warnings)
  - Dialyzer: rebar3 dialyzer (0 type errors)
  - Xref: rebar3 xref (0 undefined functions)
  - Format: rebar3 format --verify (properly formatted)

✅ Coverage: ≥80% minimum

✅ Benchmarks: (if applicable)
  - Performance regression check
  - Metrics documented
```

### Agent-Specific Quality Gates

| Agent | Additional Requirements |
|-------|-------------------------|
| `erlang-otp-developer` | All gen_server callbacks implemented, supervision tested |
| `erlang-transport-builder` | Transport benchmarked (throughput, latency, memory) |
| `erlang-test-engineer` | Coverage ≥80%, edge cases tested |
| `erlang-performance` | Performance metrics documented, no regressions |
| `erlang-github-ops` | CI pipeline passes, release notes updated |

### Post-Task Hook (Automatic)

Configured in `settings.json`:
```json
{
  "hooks": {
    "post-task": {
      "enabled": true,
      "command": "make check",
      "description": "Run tests, dialyzer, xref before completion"
    }
  }
}
```

## Context Management

### Context Window Strategy

**Problem**: Each agent has limited context window
**Solution**: Progressive disclosure + subagent delegation

| Agent | Context Strategy | Max Files Before Delegation |
|-------|------------------|----------------------------|
| `erlang-researcher` | Dedicated research context (haiku model) | Unlimited (research specialist) |
| `erlang-otp-developer` | Inherits summary from researcher | 10-15 files |
| `erlang-architect` | Focuses on architecture docs only | 5-10 files |
| `erlang-test-engineer` | Reads implementation + existing tests | 10 files |
| `plan-designer` | Spawns researcher for large codebases | 5 files direct, unlimited via researcher |

### Summary Requirements

When reporting back to delegating agent:

**erlang-researcher**:
```
Summary Format:
- Key patterns discovered: [list]
- Relevant files: [paths with line numbers]
- Architectural decisions: [summary]
- Recommendations: [action items]
```

**plan-designer**:
```
Plan Format:
- Approach: [high-level strategy]
- Architecture decisions: [list]
- Implementation steps: [numbered]
- Testing strategy: [summary]
- Delegations: [which agents to spawn]
```

## Consolidation Rationale

### Why 10 Agents?

**Problem**: 57 agents created confusion and overlap
- 8 consensus agents (not erlmcp-specific)
- 13 GitHub agents (similar functionality)
- 5 optimization agents (overlapping capabilities)
- Multiple agents doing similar research/coordination

**Solution**: 10 focused agents with clear, non-overlapping purposes
- 6 Erlang-specific (OTP dev, transport, test, research, architect, performance)
- 4 workflow/quality (GitHub ops, SPARC, planning, review)

**Benefits**:
- ✅ **10x faster discovery** - Find agent in <30s vs 5+ minutes
- ✅ **Clear purpose** - No overlap between agents
- ✅ **Erlang/OTP specialized** - Every agent tailored for erlmcp
- ✅ **Best practices compliant** - Follows Anthropic 2026 standards
- ✅ **Maintainable** - 10 agents to update vs 57

### Consolidation Map (57 → 10)

See `.claude/agents-archive/README.md` for detailed mapping of which old agents were consolidated into each of the 10 core agents.

**47 agents archived** (preserved, not deleted):
- consensus/ (8 agents) → Too generic
- github/ (13 agents) → Consolidated into erlang-github-ops
- optimization/ (5 agents) → Consolidated into erlang-performance
- swarm/ (4 agents) → Consolidated into sparc-orchestrator
- templates/ (8 agents) → Functionality merged into core 10
- development/, specialized/, testing/, devops/, documentation/, data/, analysis/, architecture/ → Consolidated appropriately

## Getting Started

### Quick Start Guide

1. **Find your task** in the "Quick Reference" table above
2. **Spawn the agent** using Claude Code's Task tool:
   ```
   Task("Agent Name", "Task description with erlmcp context", "agent-identifier")
   ```
3. **Agent follows workflow**:
   - Research phase (reads relevant files)
   - Plan phase (designs approach)
   - Execute phase (implements with quality gates)
4. **Quality gates verify** before completion

### Example: Implementing New gen_server

```javascript
// Single message with proper agent coordination
Task("Research OTP Patterns", "Analyze erlmcp gen_server patterns in src/", "erlang-researcher")
Task("Plan Cache Server", "Design gen_server-based cache with ETS backend", "plan-designer")
Task("Implement Cache", "Create cache_server.erl following erlmcp patterns", "erlang-otp-developer")
Task("Test Cache", "Write comprehensive EUnit + CT tests", "erlang-test-engineer")
Task("Review Quality", "Validate code quality before completion", "code-reviewer")
```

Each agent runs in parallel (where possible) and coordinates via hooks.

## Related Documentation

- **[ERLANG_OTP_AGENT_GUIDE.md](ERLANG_OTP_AGENT_GUIDE.md)** - Erlang-specific patterns and examples
- **[SYSTEM_GUIDE.md](SYSTEM_GUIDE.md)** - Commands vs Agents vs Roo rules
- **[agents/README.md](agents/README.md)** - Agent technical details
- **[agents-archive/README.md](agents-archive/README.md)** - Consolidation history

## Best Practices Sources

This organization follows official Anthropic documentation:

1. [Claude Code: Best practices for agentic coding](https://www.anthropic.com/engineering/claude-code-best-practices)
2. [Create custom subagents - Claude Code Docs](https://code.claude.com/docs/en/sub-agents)
3. [Agent Skills - Claude API Docs](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/overview)
4. [Best practices for Claude Code subagents](https://www.pubnub.com/blog/best-practices-for-claude-code-sub-agents/)

## Metrics & Monitoring

Agent usage statistics available via:
```bash
.claude/scripts/generate-agent-report.sh
```

Generates HTML dashboard showing:
- Agent usage frequency
- Test pass rates
- Benchmark results
- Quality gate compliance

---

**Last Consolidated**: 2026-01-27
**Agent Architecture Version**: 1.0.0
**Consolidation**: 57 → 10 agents (82% reduction)
