# erlmcp Agents (v1.0.0)

**Last Updated**: 2026-01-27
**Agent Count**: 10 core agents (consolidated from 57)

## Overview

This directory contains 10 focused, erlmcp-specific agents following Anthropic 2026 best practices. Each agent has a clear, non-overlapping purpose and integrates with the Research → Plan → Execute workflow.

## The 10 Core Agents

### Development Agents
1. **erlang-otp-developer.md** - OTP behaviors (gen_server, supervisor, application, library integration)
2. **erlang-transport-builder.md** - Transport layer (stdio, tcp, http with gun/ranch)
3. **erlang-test-engineer.md** - Chicago School TDD testing (EUnit, CT, Proper)

### Research & Planning
4. **erlang-researcher.md** - Codebase research and context preservation (haiku model)
5. **plan-designer.md** - Implementation planning (Research → Plan → Execute)

### Architecture & Design
6. **erlang-architect.md** - System architecture and OTP design (no implementation)

### Quality & Performance
7. **erlang-performance.md** - Benchmarking and optimization
8. **code-reviewer.md** - Quality gates and pre-completion validation

### Workflows
9. **erlang-github-ops.md** - Git, PR, CI/CD workflows
10. **sparc-orchestrator.md** - SPARC methodology coordination

## Agent Structure (YAML Frontmatter)

Every agent follows Anthropic-compliant format:
```yaml
---
name: agent-identifier
description: Auto-delegation trigger (when this agent should be invoked)
tools: [optional whitelist, omit for full inheritance]
model: sonnet|opus|haiku
sparc_phase: specification|architecture|refinement|completion
erlang_otp_context: true
---
```

## Quick Selection Guide

**I need to...**
- Implement gen_server → `erlang-otp-developer`
- Add transport type → `erlang-transport-builder`
- Write tests → `erlang-test-engineer`
- Understand codebase → `erlang-researcher`
- Design architecture → `erlang-architect`
- Benchmark code → `erlang-performance`
- Create PR/release → `erlang-github-ops`
- Run SPARC workflow → `sparc-orchestrator`
- Plan implementation → `plan-designer`
- Review before merge → `code-reviewer`

## Consolidation (57 → 10)

**Why consolidate?**
- 57 agents had significant overlap and confusion
- Many agents were generic (not erlmcp-specific)
- Discovery took 5+ minutes instead of <30 seconds
- Maintenance burden of 57 agents

**What happened to old agents?**
- All 47 agents **preserved** in `.claude/agents-archive/`
- Consolidation map in `.claude/agents-archive/README.md`
- No data loss, can restore if needed

**Key improvements**:
- ✅ 10x faster discovery (<30s to find right agent)
- ✅ Clear non-overlapping purposes
- ✅ Erlang/OTP specialized (every agent)
- ✅ Best practices compliant (Anthropic 2026)
- ✅ Quality gates mandatory (all agents)

## Quality Gates (Mandatory)

ALL agents must pass pre-completion verification:
```bash
✅ Tests: rebar3 do eunit, ct, proper -c (0 failures)
✅ Quality: rebar3 compile && rebar3 dialyzer && rebar3 xref (clean)
✅ Format: rebar3 format --verify
✅ Coverage: ≥80% minimum (85%+ for core modules)
✅ Benchmarks: (if applicable) performance documented
```

See `.claude/templates/agent-verification-template.md` for full requirements.

## Auto-Delegation

Agents are spawned based on YAML `description` field:
- `*.erl` file with OTP behavior → `erlang-otp-developer`
- `*_transport_*.erl` → `erlang-transport-builder`
- `*_tests.erl` → `erlang-test-engineer`
- Research needed (>5 files) → `erlang-researcher`
- Architecture decision → `erlang-architect`

## SPARC Integration

| SPARC Phase | Primary Agent | Supporting Agents |
|-------------|---------------|-------------------|
| Specification | `plan-designer` | `erlang-researcher` |
| Pseudocode | `plan-designer` | - |
| Architecture | `erlang-architect` | `erlang-otp-developer`, `erlang-transport-builder` |
| Refinement | `erlang-test-engineer` | `erlang-performance`, `code-reviewer` |
| Completion | `code-reviewer` | `erlang-github-ops` |

## Related Documentation

- **[AGENT_INDEX.md](../AGENT_INDEX.md)** - Master agent directory with quick reference
- **[ERLANG_OTP_AGENT_GUIDE.md](../ERLANG_OTP_AGENT_GUIDE.md)** - Erlang-specific workflows and examples
- **[SYSTEM_GUIDE.md](../SYSTEM_GUIDE.md)** - Commands vs Agents vs Roo rules
- **[agents-archive/README.md](../agents-archive/README.md)** - Consolidation history

## Metrics Dashboard

Generate HTML dashboard:
```bash
.claude/scripts/generate-agent-report.sh
```

View at: `.claude-flow/dashboard/index.html`

## Version History

**v1.0.0** (2026-01-27):
- Consolidated from 57 → 10 agents
- Added YAML frontmatter to all agents
- Integrated Research → Plan → Execute workflow
- Added mandatory quality gates
- Chicago School TDD for erlang-test-engineer

---

**Agent Architecture Version**: 1.0.0
**Consolidation Date**: 2026-01-27
