# SPARC CLI Quick Reference

## Quick Start Commands

### SPARC Workflow Orchestration
```bash
# Full SPARC workflow
./claude-flow sparc "Implement complete feature"

# Specific phase
./claude-flow sparc run spec "Define requirements"
./claude-flow sparc run pseudocode "Design algorithms"
./claude-flow sparc run architect "Design system"
./claude-flow sparc run tdd "Implement with TDD"
./claude-flow sparc run integration "Validate and integrate"
```

### Common Workflows
```bash
# Test-driven development
./claude-flow sparc tdd "Implement feature" --workflow=full

# Quality validation
./claude-flow sparc completion "Validate implementation" --quality-gates

# Background optimization
./claude-flow sparc orchestrate "Optimize" --strategy=background

# Multi-agent coordination
./claude-flow swarm "Complex task" --strategy=development
```

## Phase-Specific Commands

### 1. Specification Phase
```bash
# Generate specifications
./claude-flow sparc spec "Define API contracts" \
  --template=api \
  --include-test-specs \
  --output=docs/specification.md

# Requirements analysis
./claude-flow sparc spec "Analyze requirements" \
  --edge-cases \
  --constraints \
  --acceptance-criteria
```

### 2. Pseudocode Phase
```bash
# Generate pseudocode
./claude-flow sparc pseudocode "Design algorithms" \
  --tdd-anchors \
  --test-first \
  --complexity-analysis

# Algorithm design
./claude-flow sparc pseudocode "Optimization algorithm" \
  --performance-considerations \
  --memory-safety \
  --concurrency-patterns
```

### 3. Architecture Phase
```bash
# Design architecture
./claude-flow sparc architect "System design" \
  --diagrams=mermaid \
  --module-boundaries \
  --dependencies \
  --failure-modes

# OTP supervision design
./claude-flow sparc architect "OTP supervision" \
  --supervisor-strategies \
  --process-isolation \
  --failure-recovery
```

### 4. Refinement Phase (TDD)
```bash
# Full TDD workflow
./claude-flow sparc tdd "Implement feature" \
  --workflow=full \
  --test-coverage=90 \
  --include-benchmarks \
  --red-green-refactor

# Unit test generation
./claude-flow sparc tdd "Generate unit tests" \
  --test-type=unit \
  --property-based=true

# Integration test generation
./claude-flow sparc tdd "Generate integration tests" \
  --test-type=integration \
  --scenarios=end-to-end,failure-recovery
```

### 5. Completion Phase
```bash
# Quality validation
./claude-flow sparc completion "Validate implementation" \
  --quality-gates \
  --benchmark-regression \
  --test-coverage=90

# Documentation generation
./claude-flow sparc completion "Generate docs" \
  --api-docs \
  --architecture-docs \
  --examples=true

# PR creation
./claude-flow sparc completion "Create PR" \
  --pr-automation \
  --code-review \
  --quality-checks=true
```

## ErLMCP-Specific Patterns

### OTP Development
```bash
# OTP supervision implementation
./claude-flow sparc architect "Design OTP supervision" \
  --otp-patterns=true \
  --supervisor-strategies=one_for_all \
  --failure-isolation=true

# TDD for OTP
./claude-flow sparc tdd "Implement gen_server" \
  --otp-patterns \
  --supervisor-testing \
  --process-recovery-testing
```

### MCP Protocol Development
```bash
# MCP protocol handling
./claude-flow sparc architect "Design MCP protocol" \
  --protocol=JSON-RPC \
  --message-flow=registry-mediator \
  --error-handling=graceful

# Transport decoupling
./claude-flow sparc refinement "Transport decoupling" \
  --separation-of-concerns=true \
  --registry-mediator=true
```

## Agent Coordination

### Multi-Agent Workflows
```bash
# Specify agents for each phase
./claude-flow sparc orchestrate "Implementation" \
  --phase-agents="spec:plan-designer,pseudocode:plan-designer,architecture:erlang-architect"

# Background agent execution
./claude-flow sparc orchestrate "Optimization" \
  --strategy=background \
  --agents=performance-benchmarker,code-analyzer

# Memory coordination
./claude-flow memory store "arch_decision" "Supervision tree design" --namespace architecture
```

### Agent Types
- **plan-designer**: Requirements and algorithm design
- **erlang-architect**: OTP architecture and supervision
- **erlang-otp-developer**: OTP implementation
- **erlang-test-engineer**: Test suite development
- **erlang-performance**: Performance optimization
- **code-reviewer**: Quality validation
- **erlang-github-ops**: Release management

## Quality Assurance

### Quality Gates
```bash
# Compilation validation
./claude-flow sparc quality "Validate compile" --compile --dialyzer --xref

# Test validation
./claude-flow sparc quality "Validate tests" --test-coverage=90 --unit --integration

# Performance validation
./claude-flow sparc quality "Validate performance" --benchmarks --regression-threshold=10
```

### Metrics Collection
```bash
# Performance metrics
./claude-flow sparc metrics "Collect metrics" \
  --metrics=latency,throughput,memory \
  --output=metrics/report.json

# Quality metrics
./claude-flow sparc metrics "Quality report" \
  --metrics=coverage,complexity,security \
  --thresholds=coverage:90,complexity:10
```

## Memory Management

### Storage and Retrieval
```bash
# Store decisions
./claude-flow memory store "supervision_design" "one_for_all strategy" --namespace architecture

# Query memory
./claude-flow memory query "OTP patterns" --limit 5 --namespace architecture

# Export memory
./claude-flow memory export sparc_backup.json
```

## Common Options

### Global Options
- `--verbose`: Enable verbose output
- `--non-interactive`: Run in non-interactive mode
- `--timeout=<seconds>`: Set operation timeout
- `--output=<file>`: Specify output file
- `--config=<file>`: Use configuration file

### Phase-Specific Options
- **Spec**: `--template`, `--include-test-specs`, `--include-api-contracts`
- **Pseudocode**: `--tdd-anchors`, `--test-first`, `--complexity-analysis`
- **Architecture**: `--diagrams`, `--module-boundaries`, `--failure-modes`
- **TDD**: `--workflow`, `--test-coverage`, `--include-benchmarks`
- **Completion**: `--quality-gates`, `--benchmark-regression`, `--pr-automation`

## Error Handling

### Common Error Scenarios
```bash
# Handle compilation errors
./claude-flow sparc completion "Fix compilation" --fix --compile

# Handle test failures
./claude-flow sparc tdd "Fix tests" --fix --test-failures

# Handle performance regression
./claude-flow sparc optimize "Fix regression" --fix --regression-threshold=5
```

## Tips and Best Practices

### Performance Tips
1. Use `--strategy=background` for long-running operations
2. Batch multiple operations in single commands
3. Use `--parallel-phases=true` when possible
4. Cache intermediate results with memory storage

### Quality Tips
1. Always specify `--quality-gates` in completion phase
2. Use `--test-coverage=90` or higher
3. Include `--benchmark-regression` for performance-sensitive code
4. Use `--pr-automation` for consistent code review

### ErLMCP-Specific Tips
1. Use `--otp-patterns=true` for OTP development
2. Specify `--supervisor-strategies` based on failure requirements
3. Use `--separation-of-concerns=true` for transport decoupling
4. Include `--registry-mediator=true` for message routing

## Quick Troubleshooting

### Common Issues
```bash
# Module not found
Ensure root project is compiled: rebar3 compile

# Include file not found
Check Makefile has correct include paths

# Application not started
Start applications: application:ensure_all_started(erlmcp_core)

# Test coverage issues
Use: --test-coverage=90 with proper test suite
```

### Debug Commands
```bash
# Enable debug output
./claude-flow sparc "debug task" --verbose

# Show detailed error information
./claude-flow sparc "debug task" --debug --trace

# Validate configuration
./claude-flow sparc validate-config --config=sparc.config
```

---

*Quick reference for SPARC CLI integration with ErLMCP project*