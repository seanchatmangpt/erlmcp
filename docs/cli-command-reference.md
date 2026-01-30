# CLI Command Reference - erlmcp Project Patterns

This reference documents the essential CLI commands and patterns used in the erlmcp project, with concrete examples and usage scenarios.

## Essential Commands (Daily Use)

### 1. Project Initialization

```bash
# Initialize new project with Claude Code
claude init --template=erlang-otp
claude config set project.name "my_erlang_app"
claude config set project.language "erlang"

# Initialize Claude-Flow integration
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp init --sparc
```

### 2. Agent Management

```bash
# Spawn specialized agents
claude agent spawn erlang-otp-developer \
  --name "OTP Expert" \
  --priority 9 \
  --context "Implement supervision tree"

claude agent spawn erlang-test-engineer \
  --name "TDD Specialist" \
  --priority 8 \
  --context "Write EUnit tests"

# List active agents
claude agent list

# Get agent details
claude agent info <agent-id>

# Terminate agent
claude agent terminate <agent-id>
```

### 3. Task Orchestration

```bash
# Create TDD task
claude task create tdd "Implement session manager"

# Assign task to agent
claude task assign erlang-otp-developer --task tdd

# Monitor task progress
claude monitor --task tdd

# Create workflow
claude task workflow sparc-tdd.erl
```

### 4. Memory Operations

```bash
# Store project context
claude memory store "architecture_decisions" "OTP supervision tree"

# Store test patterns
claude memory store "tdd_patterns" "Chicago School, no mocks"

# Query memory
claude memory query "session"

# Export memory
claude memory export project_memory.json
```

### 5. Quality Validation

```bash
# Run mandatory quality gates
./tools/claude-md-enforcer.sh

# Check compilation
rebar3 compile

# Run tests with coverage
rebar3 eunit
rebar3 ct --suite=test/integration

# Check coverage threshold
rebar3 cover --threshold 80

# Type checking
rebar3 dialyzer
```

## SPARC Methodology Commands

### 1. SPARC Workflow

```bash
# Initialize SPARC project
claude sparc init

# Run specification phase
claude sparc spec "Design MCP server architecture"

# Run architecture phase
claude sparc architect "Design OTP supervision tree"

# Run TDD workflow
claude sparc tdd "Implement tool registry"

# Run refinement phase
claude sparc refine "Optimize gen_server implementation"

# Run completion phase
claude sparc complete "Deploy to production"
```

### 2. SPARC Modes

```bash
# List all SPARC modes
claude sparc modes

# Run specific mode
claude sparc run architect "Design event bus"

# Get mode details
claude sparc info tdd

# Custom workflow
claude sparc "Implement quality gates with SPARC" --mode refinement
```

## Swarm Coordination Commands

### 1. Swarm Management

```bash
# Start development swarm
claude swarm "Build REST API" \
  --strategy development \
  --agents backend-dev,frontend-dev,tester \
  --monitor

# Start production swarm
claude swarm "Deploy to production" \
  --strategy production \
  --agents backend-dev,devops,security \
  --background

# Start distributed swarm
claude swarm "Multi-region deployment" \
  --strategy distributed \
  --agents backend-dev,devops,infra \
  --distributed

# Swarm with UI
claude swarm "Complex feature development" \
  --ui \
  --review
```

### 2. Task Management in Swarms

```bash
# Create swarm task
claude task create "Implement authentication" \
  --swarm \
  --strategy development

# Monitor swarm progress
claude monitor --swarm

# Cancel swarm task
claude task cancel <task-id>

# Get swarm status
claude swarm status
```

## GitHub Integration Commands

### 1. PR Management

```bash
# Create enhanced PR
claude pr create \
  --title "Add tool registry" \
  --body "Implementation of tool registry with MCP protocol" \
  --assign "backend-dev" \
  --review "security"

# Review code
claude pr review 123 --detailed

# Enhance PR description
claude pr enhance 123 --add-performance-metrics

# Sync PR with project board
claude pr sync 123 --project-board "Development Board"
```

### 2. Issue Management

```bash
# Triage issues
claude issue triage --priority critical

# Create issue from CLI
claude issue create \
  --title "Memory leak in session manager" \
  --type "bug" \
  --assign "erlang-performance"

# Analyze issue patterns
claude issue analyze --closed
```

### 3. Repository Analysis

```bash
# Analyze repository structure
claude repo analyze --depth 2

# Check code quality
claude repo quality --metrics complexity,coverage

# Generate architecture report
claude repo architect --output architecture.md
```

## Performance Commands

### 1. Performance Analysis

```bash
# Analyze performance bottlenecks
claude perf analyze --target "erlmcp_bench_core_ops"

# Monitor performance metrics
claude perf monitor --interval 5

# Optimize performance
claude perf optimize --focus memory_usage
```

### 2. Benchmark Execution

```bash
# Run single benchmark
./scripts/bench/benchmark.sh core_ops_100k

# Run all benchmarks
./scripts/bench/run_all_benchmarks.sh

# Compare performance
./scripts/bench/compare.sh baseline current

# Generate performance report
./scripts/bench/generate-report.sh
```

## Hook Management Commands

### 1. Pre-Operation Hooks

```bash
# Install pre-commit hook
claude hook install pre-commit --script ./hooks/pre-commit.sh

# Install pre-edit hook
claude hook install pre-edit --script ./hooks/pre-edit.sh

# Validate before operations
claude hook validate pre-commit
```

### 2. Post-Operation Hooks

```bash
# Install post-commit hook
claude hook install post-commit --script ./hooks/post-commit.sh

# Install post-edit hook
claude hook install post-edit --script ./hooks/post-edit.sh

# Run post-operation validation
claude hook validate post-commit
```

## Debug and Troubleshooting Commands

### 1. Debug Toolkit

```bash
# Run debug toolkit
./scripts/debug-toolkit.sh

# Memory leak detection
./scripts/debug-memory.sh

# Process monitoring
./scripts/monitor-processes.sh
```

### 2. Log Analysis

```bash
# Analyze error logs
./scripts/analyze-logs.sh --errors

# Trace specific module
./scripts/trace-module.sh erlmcp_server

# Generate log report
./scripts/log-report.sh --last 24h
```

## CI/CD Commands

### 1. Deployment

```bash
# Deploy to staging
./scripts/deploy.sh staging

# Deploy to production
./scripts/deploy.sh production --require-approval

# Rollback deployment
./scripts/rollback.sh <deployment-id>
```

### 2. Testing Commands

```bash
# Run test suite
./scripts/test-all.sh

# Run integration tests
./scripts/test-integration.sh

# Run stress tests
./scripts/test-stress.sh --duration 5m
```

## Configuration Commands

### 1. Project Configuration

```bash
# Set project settings
claude config set project.name "erlmcp"
claude config set project.version "0.5.0"
claude config set project.language "erlang"

# View configuration
claude config get

# Export configuration
claude config export config.json
```

### 2. Agent Configuration

```bash
# Set agent defaults
claude agent config default-priority 8
claude agent config default-timeout 300

# View agent config
claude agent config get
```

## Monitoring and Observability Commands

### 1. System Monitoring

```bash
# Monitor system health
claude monitor system --interval 5

# Check memory usage
claude monitor memory --alert 80%

# Monitor performance
claude monitor performance --threshold 90%
```

### 2. Agent Monitoring

```bash
# Monitor active agents
claude monitor agents

# Check agent metrics
claude monitor agent <agent-id>

# Swarm monitoring
claude monitor swarm --detailed
```

## Common Patterns and Workflows

### 1. TDD Workflow

```bash
# Full TDD workflow using CLI
claude swarm "Implement user authentication" \
  --strategy development \
  --agents erlang-test-engineer,erlang-otp-developer \
  --monitor

# Red test - write failing test first
rebar3 eunit --module=auth_tests --verbose

# Green test - make it pass
./scripts/implement-auth.sh

# Refactor - improve implementation
claude agent spawn code-reviewer \
  --name "Quality Guardian" \
  --priority 7
```

### 2. Release Workflow

```bash
# Production release workflow
./scripts/validate-production.sh

claude pr create \
  --title "Release v$(cat VERSION)" \
  --body "Production release with quality gates passed" \
  --assign release-manager

claude deploy production \
  --require-approval \
  --validate-metrics
```

### 3. Emergency Debugging

```bash
# Emergency debugging workflow
./scripts/emergency-debug.sh

# Check system status
claude monitor system --critical

# Get full error report
./scripts/full-error-report.sh
```

## Command Aliases (Recommended)

Add these to your `.bashrc` or `.zshrc`:

```bash
# Claude Code aliases
alias ccp="claude config get"
alias ccs="claude config set"
alias ca="claude agent"
alias ct="claude task"
alias cm="claude memory"
alias cq="./tools/claude-md-enforcer.sh"
alias cb="./scripts/bench/run_all_benchmarks.sh"
alias cs="claude swarm"

# Common operations
alias cc="claude compile"
alias ct="claude test"
alias ci="claude integration"
alias cr="claude release"
```

## Error Handling Patterns

### 1. Command Error Handling

```bash
# Safe command execution
safe_run() {
    local cmd="$1"
    local msg="$2"

    if eval "$cmd"; then
        echo "✅ $msg"
    else
        echo "❌ $msg failed"
        exit 1
    fi
}

# Usage
safe_run "rebar3 compile" "Compilation"
safe_run "rebar3 eunit" "Tests"
```

### 2. Quality Gate Enforcement

```bash
# Mandatory quality check
if ! ./tools/claude-md-enforcer.sh; then
    echo "❌ Quality gates failed - cannot proceed"
    exit 1
fi
```

## Performance Tips

1. **Use background mode for long tasks:**
   ```bash
   claude swarm "Long-running task" --background
   ```

2. **Cache results:**
   ```bash
   claude config set cache.enabled true
   ```

3. **Parallel execution:**
   ```bash
   # Multiple commands in parallel
   cc & ct & cb
   ```

4. **Monitor resource usage:**
   ```bash
   claude monitor resources --alert 90%
   ```

This command reference provides the essential CLI patterns used successfully in the erlmcp project. Adapt these commands to your specific needs and always maintain the quality standards established by the project.