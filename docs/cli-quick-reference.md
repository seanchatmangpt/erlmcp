# Claude Code CLI Quick Reference Card

**Based on erlmcp Project Experience**

---

## 🚀 Daily Essentials

```bash
# Initialize project
claude init --template=erlang-otp
claude mcp init --sparc

# Spawn agents
claude agent spawn erlang-otp-developer --priority 9
claude agent spawn erlang-test-engineer --priority 8

# Create tasks
claude task create tdd "Implement session manager"
claude task assign <agent-id> --task <task-id>

# Store context
claude memory store "architecture" "OTP supervision tree"

# Run quality gates
./tools/claude-md-enforcer.sh
```

---

## 🧪 Testing Commands

```bash
# Unit tests
rebar3 eunit --verbose
rebar3 eunit --module=*_tests

# Integration tests
rebar3 ct --suite=test/integration --verbose

# Coverage
rebar3 cover
rebar3 cover --threshold 80

# Stress testing
./scripts/test-stress.sh --duration 5m
```

---

## 📊 Performance Commands

```bash
# Run benchmarks
./scripts/bench/run_all_benchmarks.sh
./scripts/bench/benchmark.sh core_ops_100k

# Performance analysis
claude perf analyze --target "erlmcp_bench_core_ops"
./scripts/profile-performance.sh

# Monitor system
claude monitor system --interval 5
```

---

## 🐛 Debug Commands

```bash
# Debug toolkit
./scripts/debug-toolkit.sh

# Memory leak detection
./scripts/debug-memory.sh

# Process monitoring
./scripts/monitor-processes.sh

# Error analysis
./scripts/analyze-logs.sh --errors
```

---

## 🔧 Build Commands

```bash
# Compile
rebar3 compile
TERM=dumb rebar3 compile

# Type checking
rebar3 dialyzer

# Cross-reference
rebar3 xref

# Format code
rebar3 format check
rebar3 format
```

---

## 🚀 Deployment Commands

```bash
# Deploy to staging
./scripts/deploy.sh staging

# Deploy to production
./scripts/deploy.sh production --require-approval

# Rollback
./scripts/rollback.sh <deployment-id>

# Validation
./scripts/validate-production.sh
```

---

## 🤖 Swarm Commands

```bash
# Start development swarm
claude swarm "Build feature" --strategy development --monitor

# Start production swarm
claude swarm "Deploy" --strategy production --background

# List swarms
claude swarm list
claude swarm status
```

---

## 📝 SPARC Commands

```bash
# TDD workflow
claude sparc tdd "Implement feature"

# Architecture design
claude sparc architect "Design system"

# Refinement
claude sparc refine "Optimize implementation"
```

---

## 🔑 GitHub Integration

```bash
# Create PR
claude pr create --title "Feature" --body "Description"

# Review code
claude pr review 123 --detailed

# Sync with board
claude pr sync 123 --project-board "Board"
```

---

## ⚠️ Quality Gates (MANDATORY)

Before every commit and deployment:
```bash
# Always run these in order
rebar3 compile                    # Must pass with 0 errors
rebar3 eunit                     # Must pass 100%
rebar3 cover --threshold 80      # Must meet coverage
./tools/claude-md-enforcer.sh     # Quality validation
```

---

## 🔄 Common Patterns

```bash
# TDD Pattern
claude spawn "Write failing test"
./implement-test.sh
./make-test-pass.sh
./refactor.sh

# Release Pattern
./validate-production.sh
claude pr create
./deploy.sh production

# Debug Pattern
./scripts/debug-toolkit.sh
./scripts/analyze-logs.sh
./scripts/fix-issues.sh
```

---

## 💡 Pro Tips

1. **Always run quality gates before committing**
2. **Use background mode for tasks > 30 minutes**
3. **Store important context in memory**
4. **Monitor swarms in real-time**
5. **Use aliases for common commands**

---

## 📚 Resources

- **Full Commands**: `./docs/cli-command-reference.md`
- **HOWTO Tutorials**: `./docs/diataxis-claude-code-features-HOWTO.md`
- **Agent Guide**: `./.claude/AGENT_INDEX.md`
- **Project Config**: `./CLAUDE.md`