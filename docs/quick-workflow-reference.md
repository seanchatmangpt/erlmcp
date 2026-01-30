# Quick Workflow Reference - erlmcp Task Orchestration

## At a Glance

This guide provides quick patterns for common task orchestration scenarios in erlmcp.

## Common Patterns

### 1. Adding New gen_server Module

```erlang
% Single message with all phases coordinated
Task("Research OTP Patterns", "Analyze existing gen_server patterns", "erlang-researcher")
Task("Plan Implementation", "Design module structure and API", "plan-designer")
Task("Implement Module", "Create new_module.erl following patterns", "erlang-otp-developer")
Task("Write Tests", "EUnit + CT tests for module", "erlang-test-engineer")
Task("Review Quality", "Validate implementation", "code-reviewer")
```

### 2. Full-Stack Development Swarm

```bash
# One command for complete development
./claude-flow-swarm "Build full-stack application" \
  --strategy development \
  --monitor \
  --review \
  --testing \
  --max-agents 8
```

### 3. Security Patch Workflow

```bash
# TCPS security workflow
/tcps-pull security-advisory CVE-2026-1234
/tcps-heijunka critical
/tcps-kanban move WO-SEC-123 to-code
/tcps-build WO-SEC-123
/tcps-jidoka WO-SEC-123 --full
/tcps-receipt WO-SEC-123
```

### 4. Performance Optimization

```bash
# Background optimization with monitoring
./claude-flow-swarm "Optimize system performance" \
  --strategy optimization \
  --background \
  --monitor \
  --ui

# Check results
/claude-flow memory query "optimization_results" --namespace swarm
```

## Key Commands Quick Reference

| Need to... | Use This Command | Example |
|------------|-----------------|---------|
| **Start complex task** | `./claude-flow-swarm` | `./claude-flow-swarm "build API" --strategy development` |
| **Follow SPARC methodology** | `./sparc` | `./sparc architect "design system"` |
| **Create work order** | `/tcps-pull` | `/tcps-pull github-issue 123` |
| **Check workflow status** | `/tcps-kanban check` | `/tcps-kanban check` |
| **Run quality checks** | `/tcps-jidoka` | `/tcps-jidoka WO-123 --full` |
| **Trigger stop-the-line** | `/tcps-andon` | `/tcps-andon trigger WO-123 "error"` |
| **Find root cause** | `/5-whys-analyze` | `/5-whys-analyze "test failure"` |
| **Monitor progress** | `/claude-flow monitor` | `/claude-flow monitor --focus swarm` |

## Agent Assignment Rules

| File Pattern | Auto-Agent | Use For |
|--------------|------------|---------|
| `*.erl` | `erlang-otp-developer` | OTP implementation |
| `*_tests.erl` | `erlang-test-engineer` | Testing |
| `docs/*.md` | `erlang-researcher` | Documentation |
| `*_sup.erl` | `erlang-architect` | Supervision design |
| Benchmark tasks | `erlang-performance` | Performance |
| `.github/*` | `erlang-github-ops` | Git workflows |

## Quality Gates (Must Pass Before "Done")

```erlang
% ALL agents must verify these:
✅ Tests: All pass (0 failures)
  - EUnit: rebar3 eunit --module=<module>
  - CT: rebar3 ct --suite=<suite>
  - Proper: rebar3 proper

✅ Quality: All checks clean
  - Compile: rebar3 compile (0 warnings)
  - Dialyzer: rebar3 dialyzer (0 type errors)
  - Xref: rebar3 xref (0 undefined)
  - Format: rebar3 format --verify

✅ Coverage: ≥80% minimum
✅ Benchmarks: No performance regression
```

## Common Workflows

### Bug Fix Process
```bash
/tcps-pull github-issue 456      # Create work order
/tcps-kanban move WO-456 to-code # Move to development
/tcps-build WO-456               # Fix the bug
/tcps-jidoka WO-456              # Test the fix
/tcps-receipt WO-456             # Document fix
```

### Feature Development Process
```bash
/tcps-pull github-issue 789      # Feature request
/tcps-heijunka features         # Schedule feature work
/tcps-kanban move WO-789 to-design # Design phase
/tcps-kanban move WO-789 to-code # Implementation
/tcps-jidoka WO-789             # Testing
/tcps-receipt WO-789             # Release
```

### Performance Tuning Process
```bash
./claude-flow-swarm "Optimize slow queries" \
  --strategy optimization \
  --monitor \
  --background

# Monitor progress
/claude-flow memory query "optimization_progress"

# Verify improvements
/poka-yoke-test bench --parallel
```

## Error Recovery

### If Tests Fail
```bash
/5-whys-analyze "test failure in module"
/tcps-andon trigger WO-123 "test-failure"
# Fix the issue
/tcps-andon resolve WO-123
/tcps-jidoka WO-123 --retest
```

### If Quality Gates Fail
```bash
# Run specific checks
/poka-yoke-validate dist/v1.0.0/team
/poka-yoke-test chaos --verbose

# Fix issues and retry
/tcps-build WO-123 --no-cache
/tcps-jidoka WO-123 --full
```

## Tips & Tricks

1. **Use `--dry-run`** to preview workflow before execution
2. **Use `--background`** for long-running tasks (>30 min)
3. **Use `--monitor`** to track progress of complex workflows
4. **Use `--parallel`** to speed up independent tasks
5. **Always run quality gates** before considering work complete

## Getting Help

```bash
# Show all commands
/claude-flow-help commands

# Get help on specific command
/claude-flow-help tcps

# View agent documentation
/claude-flow-help agents
```

## Quick Start

```bash
# Your first complex task
./claude-flow-swarm "Add authentication to application" \
  --strategy development \
  --monitor \
  --review

# Check your work
/tcps-kanban check

# Verify quality
/tcps-jidoka WO-123
```

This quick reference should help you get started with task orchestration in erlmcp. For detailed documentation, see the full guides in `/docs/`.