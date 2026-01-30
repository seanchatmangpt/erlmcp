# Claude Code CLI Complete Guide

## Diátaxis Documentation Structure

This guide follows the Diátaxis documentation framework, providing four complementary approaches to understanding Claude Code CLI:

- **EXPLAIN** - Conceptual understanding and architecture
- **HOWTO** - Practical implementation patterns
- **REFERENCE** - Technical specifications and API details
- **TUTORIAL** - Step-by-step learning paths

---

## Table of Contents

- [1. EXPLAIN - Understanding Claude Code CLI](#1-explain---understanding-claude-code-cli)
  - [1.1 Architecture Overview](#11-architecture-overview)
  - [1.2 Core Concepts](#12-core-concepts)
  - [1.3 Agent Ecosystem](#13-agent-ecosystem)
  - [1.4 Coordination Mechanisms](#14-coordination-mechanisms)
  - [1.5 Quality Assurance Framework](#15-quality-assurance-framework)
- [2. HOWTO - Practical Implementation Patterns](#2-howto---practical-implementation-patterns)
  - [2.1 Basic Agent Spawning](#21-basic-agent-spawning)
  - [2.2 Complex Workflows](#22-complex-workflows)
  - [2.3 Multi-Agent Coordination](#23-multi-agent-coordination)
  - [2.4 Production Validation](#24-production-validation)
  - [2.5 Performance Optimization](#25-performance-optimization)
- [3. REFERENCE - Technical Specifications](#3-reference---technical-specifications)
  - [3.1 Agent Types](#31-agent-types)
  - [3.2 MCP Tools](#32-mcp-tools)
  - [3.3 Coordination Protocols](#33-coordination-protocols)
  - [3.4 Memory Management](#34-memory-management)
  - [3.5 Performance Metrics](#35-performance-metrics)
- [4. TUTORIAL - Learning Paths](#4-tutorial---learning-paths)
  - [4.1 Getting Started](#41-getting-started)
  - [4.2 Building Applications](#42-building-applications)
  - [4.3 Advanced Patterns](#43-advanced-patterns)
  - [4.4 Case Studies](#44-case-studies)
- [5. Appendix](#5-appendix)
  - [5.1 Troubleshooting](#51-troubleshooting)
  - [5.2 Best Practices](#52-best-practices)
  - [5.3 Additional Resources](#53-additional-resources)

---

## 1. EXPLAIN - Understanding Claude Code CLI

### 1.1 Architecture Overview

Claude Code CLI is a revolutionary command-line interface that transforms how developers interact with AI assistance through agent-based coordination. The architecture consists of several key components:

#### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                    Claude Code CLI                          │
├─────────────────────────────────────────────────────────────┤
│  Task Engine    │  File System    │  Git Integration       │
│  (Parallel)     │  Operations     │  & CI/CD              │
│                 │                 │                       │
├─────────────────────────────────────────────────────────────┤
│                Agent Coordination Layer                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │   MCP Tools │  │  Task Tool  │  │   Memory   │        │
│  │  (Coord.)   │  │ (Execution) │  │    System  │        │
│  └─────────────┘  └─────────────┘  └─────────────┘        │
├─────────────────────────────────────────────────────────────┤
│                 54 Specialized Agents                        │
│  ├── Hyper-Advanced Agents (Priority)                       │
│  ├── Core Development Agents                                │
│  ├── Swarm Coordination Agents                              │
│  ├── Performance & Optimization Agents                       │
│  └── Testing & Validation Agents                           │
└─────────────────────────────────────────────────────────────┘
```

#### Key Architectural Principles

1. **Separation of Concerns**: MCP tools handle coordination, Task tool handles execution
2. **Parallel Execution**: All operations batched in single messages for maximum efficiency
3. **Memory Integration**: Persistent memory across sessions for context retention
4. **Quality Assurance**: Zero-defect delivery with automated validation gates
5. **Modular Design**: Agents specialize in specific domains for optimal performance

### 1.2 Core Concepts

#### Agent-Based Architecture

Claude Code CLI operates through a sophisticated agent system where each agent brings specialized expertise:

- **Hyper-Advanced Agents**: Provide production-ready solutions (system-architect, performance-benchmarker, production-validator)
- **Core Agents**: Handle basic development tasks (coder, tester, reviewer)
- **Specialized Agents**: Domain-specific expertise (backend-dev, ml-developer, cicd-engineer)

#### Task Orchestration

The Task tool enables concurrent execution of multiple agents with proper coordination:

```javascript
// Single Message - Parallel Agent Execution
Task("System Architect", "Design microservices architecture with hooks", "system-architect")
Task("Backend Developer", "Implement API Gateway with TypeScript", "backend-dev")
Task("Performance Benchmarker", "Establish baselines for critical paths", "performance-benchmarker")
Task("Production Validator", "Validate deployment readiness", "production-validator")
Task("Code Reviewer", "Review integration patterns", "code-analyzer")

TodoWrite { todos: [comprehensive task list] }
```

### 1.3 Agent Ecosystem

#### Hyper-Advanced Agents (Production-Ready)

These agents provide enterprise-grade solutions with comprehensive documentation and validation:

| Agent | Specialization | Use Case | Output Quality |
|-------|---------------|----------|----------------|
| production-validator | Deployment readiness | Docker, OTEL, infrastructure | 178KB documentation |
| code-analyzer | Technical debt analysis | Deep code review, architecture | 5x comprehensive analysis |
| system-architect | System design | Integration patterns, architecture | Production-ready designs |
| performance-benchmarker | Performance optimization | Benchmarking, bottleneck ID | 35+ benchmark scenarios |
| backend-dev | Backend implementation | APIs, databases, infrastructure | Production-ready code |
| task-orchestrator | Workflow coordination | Multi-phase complex workflows | End-to-end orchestration |

#### Core Development Agents

Handle fundamental development tasks following established patterns:

- **Coder**: Implementation of features and functionality
- **Tester**: Creation of comprehensive test suites
- **Reviewer**: Code quality and security validation
- **Researcher**: Requirements analysis and patterns
- **Planner**: Research → Plan → Execute workflows

### 1.4 Coordination Mechanisms

#### Hook Integration Protocol

Every agent execution follows a strict coordination protocol:

```bash
# Before Work
npx claude-flow@alpha hooks pre-task --description "[task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"

# During Work
npx claude-flow@alpha hooks post-edit --file "[file]" --memory-key "swarm/[agent]/[step]"
npx claude-flow@alpha hooks notify --message "[what was done]"

# After Work
npx claude-flow@alpha hooks post-task --task-id "[task]"
npx claude-flow@alpha hooks session-end --export-metrics true
```

#### Memory Management

Claude Code CLI maintains persistent memory across sessions:

- **Session State**: Agent contexts and progress tracking
- **Neural Patterns**: Learning from successful executions
- **Decision History**: Accumulated best practices
- **Performance Metrics**: Historical data for optimization

### 1.5 Quality Assurance Framework

#### Zero-Defect Delivery Standards

The framework enforces manufacturing-grade quality standards:

```bash
# Mandatory Quality Gates
✅ Compilation: 0 errors (blocking)
✅ Tests: 100% pass rate (0 failures)
✅ Coverage: ≥80% code coverage
✅ Type Safety: All functions properly typed
✅ Security: No vulnerabilities detected
✅ Performance: <10% regression
```

#### Automatic Validation

Quality gates are enforced at multiple points:

1. **Pre-commit**: Automated validation before git commits
2. **Post-task**: Validation after agent task completion
3. **CI/CD**: Full pipeline validation in GitHub Actions
4. **Manual**: On-demand validation tools

---

## 2. HOWTO - Practical Implementation Patterns

### 2.1 Basic Agent Spawning

#### Simple Agent Execution

For straightforward tasks, use basic development agents:

```javascript
[Single Message - Basic Task Execution]:
  Task("Coder", "Implement user authentication module", "coder")
  Task("Tester", "Create authentication test suite", "tester")
  Task("Reviewer", "Review security implementation", "reviewer")

  // Batch todos in one call
  TodoWrite { todos: [
    {id: "1", content: "Implement auth service", status: "in_progress", priority: "high"},
    {id: "2", content: "Write unit tests", status: "pending", priority: "high"},
    {id: "3", content: "Integration tests", status: "pending", priority: "medium"},
    {id: "4", content: "Security review", status: "pending", priority: "medium"}
  ]}
```

#### Pattern: File Operations

Always batch file operations in single messages:

```javascript
// Write all files in one message
Write "src/auth/service.ts"
Write "src/auth/types.ts"
Write "tests/auth.test.ts"
Write "docs/auth.md"
```

### 2.2 Complex Workflows

#### Full-Stack Development Pattern

For comprehensive development, use specialized agents:

```javascript
[Single Message - Full-Stack Development]:
  // Hyper-advanced agents for production-ready code
  Task("Backend Developer", "Build REST API with Express, JWT auth, MongoDB", "backend-dev")
  Task("System Architect", "Design API architecture, endpoints, data flow", "system-architect")
  Task("Performance Benchmarker", "Benchmark API response times, throughput", "performance-benchmarker")
  Task("Production Validator", "Validate deployment, Docker, environment setup", "production-validator")
  Task("Code Analyzer", "Review code quality, technical debt, patterns", "code-analyzer")
  Task("Test Engineer", "Create comprehensive test suite with 90% coverage", "tester")

  // Comprehensive todos
  TodoWrite { todos: [15-20 comprehensive todos] }
```

#### Pattern: Multi-Phase Workflows

Break complex tasks into coordinated phases:

```javascript
// Phase 1: Architecture & Design
Task("System Architect", "Design microservices architecture", "system-architect")
Task("Code Analyzer", "Analyze existing patterns and constraints", "code-analyzer")

// Phase 2: Implementation
Task("Backend Developer", "Implement service layers", "backend-dev")
Task("Coder", "Build frontend components", "coder")

// Phase 3: Testing & Validation
Task("Test Engineer", "Integration and e2e tests", "tester")
Task("Production Validator", "Load testing and validation", "production-validator")
```

### 2.3 Multi-Agent Coordination

#### Swarm Coordination Pattern

For complex multi-agent coordination:

```javascript
[Single Message - Swarm Coordination]:
  // Set up coordination topology
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "backend-dev" }
  mcp__claude-flow__agent_spawn { type: "ml-developer" }
  mcp__claude-flow__agent_spawn { type: "cicd-engineer" }

  // Execute with specialized agents
  Task("Backend Developer", "Build API with hooks integration", "backend-dev")
  Task("ML Developer", "Implement ML pipeline with monitoring", "ml-developer")
  Task("CICD Engineer", "Set up GitHub Actions with quality gates", "cicd-engineer")
  Task("Performance Benchmarker", "Benchmark end-to-end pipeline", "performance-benchmarker")

  // Memory coordination
  npx claude-flow@alpha hooks pre-task --description "Swarm backend development"
```

#### Pattern: Cross-Agent Communication

Agents share context through memory keys:

```javascript
// Agent 1: Research
Task("Researcher", "Analyze requirements and patterns", "researcher")
Task hooks post-edit --memory-key "research/patterns"

// Agent 2: Implementation
Task("Coder", "Implement based on research findings", "coder")
Task hooks session-restore --session-id "research/patterns"
```

### 2.4 Production Validation

#### Production-Ready Agent Pattern

For deployment-critical code, use production validation agents:

```javascript
[Single Message - Production Validation]:
  Task("Production Validator", "Validate Docker setup, environment, security", "production-validator")
  Task("System Architect", "Review architecture for production scalability", "system-architect")
  Task("Performance Benchmarker", "Establish production performance baselines", "performance-benchmarker")
  Task("Code Analyzer", "Deep technical review and security audit", "code-analyzer")
  Task("Backend Developer", "Implement production-grade error handling", "backend-dev")

  // Comprehensive validation todos
  TodoWrite { todos: [
    {id: "prod-1", content: "Docker validation", status: "in_progress", priority: "high"},
    {id: "prod-2", content: "Security scan", status: "pending", priority: "high"},
    {id: "prod-3", content: "Performance baselines", status: "pending", priority: "high"},
    {id: "prod-4", content: "Architecture review", status: "pending", priority: "high"}
  ]}
```

#### Pattern: Quality Gate Integration

Automate quality gates in your workflow:

```bash
# Pre-commit validation
./tools/claude-md-enforcer.sh

# Post-commit validation
npx claude-flow@alpha hooks post-task-validate

# CI/CD pipeline integration
cargo-make ci  # Full validation suite
```

### 2.5 Performance Optimization

#### Performance Analysis Pattern

For performance-critical applications:

```javascript
[Single Message - Performance Optimization]:
  Task("Performance Benchmarker", "Profile application bottlenecks", "performance-benchmarker")
  Task("System Architect", "Design caching strategies", "system-architect")
  Task("Backend Developer", "Implement optimized data access", "backend-dev")
  Task("Code Analyzer", "Review performance anti-patterns", "code-analyzer")

  // Performance tracking
  npx claude-flow@alpha hooks notify --message "Starting performance analysis"
```

#### Pattern: Benchmarking Workflows

Establish comprehensive baselines:

```javascript
// Core operations benchmark
Task("Performance Benchmarker", "Registry/queue/pool performance", "performance-benchmarker")

// Network performance
Task("Performance Benchmarker", "TCP/HTTP socket throughput", "performance-benchmarker")

// Stress testing
Task("Performance Benchmarker", "Sustained load testing 24hr", "performance-benchmarker")

// Integration performance
Task("Performance Benchmarker", "End-to-end MCP workflow latency", "performance-benchmarker")
```

---

## 3. REFERENCE - Technical Specifications

### 3.1 Agent Types

#### Hyper-Advanced Agents

| Agent | Domain | Specialization | Output Scale |
|-------|--------|---------------|--------------|
| production-validator | DevOps | Docker, deployment, infrastructure | 178KB+ docs |
| code-analyzer | Code Quality | Technical debt, architecture review | 5x comprehensive |
| system-architect | Architecture | Integration, microservices, patterns | Production-ready |
| performance-benchmarker | Performance | Benchmarking, optimization | 35+ scenarios |
| backend-dev | Backend | APIs, databases, infrastructure | Production code |
| task-orchestrator | Workflow | Multi-phase coordination | End-to-end |

#### Core Development Agents

| Agent | Purpose | Common Tasks |
|-------|---------|--------------|
| coder | Implementation | Feature development, bug fixes |
| tester | Testing | Unit tests, integration, TDD |
| reviewer | Quality review | Code review, security audit |
| researcher | Analysis | Requirements, patterns, research |
| planner | Strategy | Planning, coordination |

### 3.2 MCP Tools

#### Coordination Tools

```javascript
// Swarm initialization
mcp__claude-flow__swarm_init {
  topology: "mesh",  // mesh, hierarchical, adaptive
  maxAgents: 6,
  memoryRetention: true
}

// Agent type definition
mcp__claude-flow__agent_spawn {
  type: "backend-dev",
  capabilities: ["Docker", "API", "Database"],
  memoryKey: "swarm/backend"
}

// Task orchestration
mcp__claude-flow__task_orchestrate {
  phases: ["research", "design", "implement", "test"],
  dependencies: ["research", "design"],
  parallel: ["implement", "test"]
}
```

#### Monitoring Tools

```javascript
// Swarm status
mcp__claude-flow__swarm_status { includeMetrics: true }

// Agent listing
mcp__claude-flow__agent_list { activeOnly: true }

// Performance metrics
mcp__claude-flow__agent_metrics { agentId: "system-architect" }
```

### 3.3 Coordination Protocols

#### Hook Execution Sequence

```bash
# Pre-operation hooks
1. Pre-Task: Validate command safety, assign agents
2. Pre-Edit: Prepare resources, optimize topology
3. Session Restore: Load context from memory

# Post-operation hooks
1. Post-Edit: Auto-format code, update memory
2. Post-Task: Train neural patterns, track performance
3. Session End: Export metrics, generate summary
```

#### Message Batching Rules

1. **Single Message Principle**: All related operations in one message
2. **Parallel Execution**: Maximum 10-20 concurrent operations
3. **Todo Batching**: 5-10 todos minimum per call
4. **File Operations**: Batch all reads/writes/edits
5. **Memory Operations**: Batch all store/retrieve calls

### 3.4 Memory Management

#### Memory Structure

```javascript
{
  "sessions": {
    "swarm-id": {
      "context": { currentTask, priorities, constraints },
      "agents": { active, completed, metrics },
      "memory": { patterns, decisions, history }
    }
  },
  "patterns": {
    "successful": [workflow patterns],
    "neural": [trained patterns]
  },
  "metrics": {
    "performance": { historical data },
    "quality": { pass rates, coverage },
    "efficiency": { token usage, speed }
  }
}
```

#### Memory Persistence

- **Session Restore**: Load context across agent restarts
- **Pattern Training**: Learn from successful workflows
- **Decision History**: Track architectural decisions
- **Performance Tracking**: Monitor optimization progress

### 3.5 Performance Metrics

#### Benchmark Results

```erlang
% Core operations (erlmcp benchmarks)
Registry: 553K msg/s
Queue: 971K msg/s
Pool: 149K msg/s
Session: 242K msg/s
Network I/O: 43K msg/s (4KB real packets)

% Sustained performance
372K msg/s (60M ops/30min)
100K+ concurrent connections requires clustering
```

#### Agent Performance Metrics

| Metric | Baseline | Optimization Target |
|--------|----------|-------------------|
| Token Usage | 32.3% reduction | 40%+ reduction |
| Speed Improvement | 2.8-4.4x | 5x improvement |
| Solve Rate | 84.8% SWE-Bench | 90%+ target |
| Coverage | 80% minimum | 95%+ goal |

---

## 4. TUTORIAL - Learning Paths

### 4.1 Getting Started

#### First Steps with Claude Code CLI

1. **Installation and Setup**
```bash
# Add Claude Flow MCP server
claude mcp add claude-flow npx claude-flow@alpha mcp start

# Configure environment
export CLAUDE_FLOW_MEMORY=true
export CLAUDE_FLOW_HOOKS=true
```

2. **Basic Agent Spawning**
```javascript
[Single Message]:
  Task("Coder", "Create a simple 'Hello World' application", "coder")
  Task("Tester", "Write tests for the Hello World application", "tester")

  TodoWrite { todos: [
    {id: "1", content: "Hello World implementation", status: "pending", priority: "high"},
    {id: "2", content: "Test creation", status: "pending", priority: "medium"}
  ]}
```

3. **Understanding Output Patterns**
- Agents follow strict coordination protocols
- Memory persists across sessions
- Quality gates validate all outputs

### 4.2 Building Applications

#### Tutorial: Full-Stack Web Application

```javascript
[Single Message - Full Application]:
  // Phase 1: Architecture
  Task("System Architect", "Design REST API with MongoDB", "system-architect")

  // Phase 2: Backend Implementation
  Task("Backend Developer", "Express.js API with JWT auth", "backend-dev")

  // Phase 3: Frontend
  Task("Coder", "React frontend with API integration", "coder")

  // Phase 4: Testing
  Task("Test Engineer", "E2E testing with Cypress", "tester")

  // Phase 5: Production
  Task("Production Validator", "Docker setup and deployment", "production-validator")

  // Comprehensive todo tracking
  TodoWrite { todos: [comprehensive task list] }
```

#### Pattern: API Development

```javascript
[Single Message - API Development]:
  Task("Backend Developer", "Build REST API with Express, TypeScript", "backend-dev")
  Task("System Architect", "Design API endpoints and data flow", "system-architect")
  Task("Performance Benchmarker", "API benchmarking and optimization", "performance-benchmarker")
  Task("Code Analyzer", "Code review and security audit", "code-analyzer")
```

### 4.3 Advanced Patterns

#### Tutorial: Multi-Agent Coordination

```javascript
[Single Message - Advanced Coordination]:
  // Initialize swarm topology
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 8 }

  // Spawn specialized agents
  mcp__claude-flow__agent_spawn { type: "backend-dev" }
  mcp__claude-flow__agent_spawn { type: "ml-developer" }
  mcp__claude-flow__agent_spawn { type: "cicd-engineer" }

  // Execute coordinated tasks
  Task("Backend Developer", "Build API with hooks", "backend-dev")
  Task("ML Developer", "ML pipeline integration", "ml-developer")
  Task("CICD Engineer", "GitHub Actions setup", "cicd-engineer")
  Task("Performance Benchmarker", "End-to-end benchmarking", "performance-benchmarker")

  // Memory coordination
  npx claude-flow@alpha hooks pre-task --description "Advanced coordination workflow"
```

#### Pattern: Microservices Architecture

```javascript
[Single Message - Microservices]:
  Task("System Architect", "Design microservices architecture", "system-architect")
  Task("Backend Developer", "Implement service A (user management)", "backend-dev")
  Task("Backend Developer", "Implement service B (product catalog)", "backend-dev")
  Task("Backend Developer", "Implement API Gateway", "backend-dev")
  Task("Test Engineer", "Integration and contract tests", "tester")
  Task("Production Validator", "Docker compose setup", "production-validator")
```

### 4.4 Case Studies

#### Case Study 1: erlmcp Performance Optimization

**Challenge**: erlmcp needed comprehensive performance benchmarking

**Solution**: Used performance-benchmarker agent with 35+ scenarios

```javascript
[Single Message - erlmcp Benchmarking]:
  Task("Performance Benchmarker", "Core operations benchmarks", "performance-benchmarker")
  Task("Performance Benchmarker", "Network real socket performance", "performance-benchmarker")
  Task("Performance Benchmarker", "Sustained load testing", "performance-benchmarker")
  Task("Performance Benchmarker", "Chaos failure injection", "performance-benchmarker")
  Task("Performance Benchmarker", "Integration workflow latency", "performance-benchmarker")
```

**Results**:
- Registry: 553K msg/s
- Queue: 971K msg/s
- Pool: 149K msg/s
- Session: 242K msg/s
- Sustained: 372K msg/s (60M ops/30min)

#### Case Study 2: Production-Ready Docker Deployment

**Challenge**: Validate Docker setup for production deployment

**Solution**: Used production-validator with comprehensive checks

```javascript
[Single Message - Production Validation]:
  Task("Production Validator", "Dockerfile validation", "production-validator")
  Task("Production Validator", "Environment setup", "production-validator")
  Task("Production Validator", "Security scanning", "production-validator")
  Task("Production Validator", "Health checks", "production-validator")
  Task("Production Validator", "Performance baselines", "production-validator")
```

**Output**: 178KB comprehensive documentation with 10-point health check system

#### Case Study 3: API Gateway Architecture

**Challenge**: Design scalable API gateway architecture

**Solution**: Used system-architect for production-ready design

```javascript
[Single Message - Architecture Design]:
  Task("System Architect", "API Gateway architecture design", "system-architect")
  Task("Code Analyzer", "Review integration patterns", "code-analyzer")
  Task("Backend Developer", "Gateway implementation", "backend-dev")
  Task("Performance Benchmarker", "Gateway benchmarking", "performance-benchmarker")
```

**Output**: Complete architecture with failure modes and recovery strategies

---

## 5. Appendix

### 5.1 Troubleshooting

#### Common Issues and Solutions

**Issue: Agent coordination fails**
```bash
# Solution: Reset swarm state
npx claude-flow@alpha swarm reset
npx claude-flow@alpha swarm init
```

**Issue: Memory persistence problems**
```bash
# Solution: Clear and restore
npx claude-flow@alpha memory clear
npx claude-flow@alpha hooks session-restore
```

**Issue: Quality gates failing**
```bash
# Solution: Manual validation
./tools/claude-md-enforcer.sh
cargo-make verify
```

#### Performance Optimization

**Slow task execution**:
```bash
# Check metrics
npx claude-flow@alpha agent metrics
npx claude-flow@alpha performance analyze

# Optimize topology
mcp__claude-flow__swarm_init { topology: "adaptive" }
```

**Memory issues**:
```bash
# Monitor usage
npx claude-flow@alpha memory usage
npx claude-flow@alpha neural status
```

### 5.2 Best Practices

#### Agent Usage Patterns

1. **Always batch operations** in single messages
2. **Use Hyper-Advanced agents** for production-ready code
3. **Follow coordination protocols** with hooks
4. **Maintain comprehensive todos** (5-10 minimum)
5. **Validate outputs** with quality gates

#### Code Quality Standards

```bash
# Mandatory checks before deployment
cargo-make              # Format, lint, tests
cargo-make verify       # All checks + tests
cargo-make ci           # Full CI pipeline
cargo-make prod-build   # Strict production build
```

#### Performance Optimization

1. **Establish baselines** before optimization
2. **Use proper metrics** for measurement
3. **Test thoroughly** after optimization
4. **Monitor continuously** for degradation
5. **Document changes** and their impact

### 5.3 Additional Resources

#### Documentation

- [Claude Flow Documentation](https://github.com/ruvnet/claude-flow)
- [erlmcp Architecture Guide](docs/architecture.md)
- [OTP Patterns Guide](docs/otp-patterns.md)
- [Metrology Guidelines](docs/metrology/)

#### Tools and Utilities

```bash
# Hook management
./tools/claude-md-sync.sh           # Install hooks
./tools/claude-md-enforcer.sh       # Manual validation

# Benchmarking
./scripts/bench/run_all_benchmarks.sh  # Full suite
rebar3 benchmark                    # Erlang benchmarks

# Development
make console        # Erlang shell
make check          # Full validation
make observer       # Process visualization
```

#### Community and Support

- GitHub Issues: Report bugs and request features
- Documentation: Comprehensive guides and examples
- Benchmarks: Performance metrics and optimization guides
- Case Studies: Real-world implementations and patterns

---

## Navigation Guide

### Quick Start (Beginner)
1. Read [4.1 Getting Started](#41-getting-started)
2. Try basic agent spawning
3. Review [2.1 Basic Agent Spawning](#21-basic-agent-spawning)

### Development (Intermediate)
1. Study [1.2 Core Concepts](#12-core-concepts)
2. Implement [2.2 Complex Workflows](#22-complex-workflows)
3. Review [3.1 Agent Types](#31-agent-types)

### Production (Advanced)
1. Study [1.5 Quality Assurance Framework](#15-quality-assurance-framework)
2. Implement [2.4 Production Validation](#24-production-validation)
3. Review [4.4 Case Studies](#44-case-studies)

### Troubleshooting
1. Check [5.1 Troubleshooting](#51-troubleshooting)
2. Review best practices in [5.2 Best Practices](#52-best-practices)
3. Consult additional resources in [5.3 Additional Resources](#53-additional-resources)

---

*This guide represents the collective knowledge from 20 specialized agents, providing a comprehensive resource for Claude Code CLI development.*