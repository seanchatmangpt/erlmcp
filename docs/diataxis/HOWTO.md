# HOWTO - Practical Implementation Patterns

## Introduction

This HOWTO guide provides practical, task-oriented patterns for implementing Claude Code CLI in real-world development scenarios. Each pattern follows the Single Message Principle and demonstrates proper agent coordination.

---

## 1. Basic Agent Spawning

### 1.1 Simple Task Execution

For straightforward development tasks, use basic agent patterns:

```javascript
[Single Message - Basic Task Execution]:
  Task("Coder", "Implement user authentication service", "coder")
  Task("Tester", "Create comprehensive test suite for auth", "tester")
  Task("Reviewer", "Review implementation for security issues", "reviewer")

  // Batch todos in one call (minimum 5-10)
  TodoWrite { todos: [
    {id: "1", content: "Implement auth service", status: "in_progress", priority: "high"},
    {id: "2", content: "Create unit tests", status: "pending", priority: "high"},
    {id: "3", content: "Write integration tests", status: "pending", priority: "medium"},
    {id: "4", content: "Security review", status: "pending", priority: "high"},
    {id: "5", content: "Documentation", status: "pending", priority: "medium"}
  ]}
```

**Key Principles**:
- Always batch operations in single messages
- Include comprehensive todo tracking
- Use appropriate agent types for the task

### 1.2 File Operations Pattern

All file operations must be batched in single messages:

```javascript
[Single Message - File Operations]:
  // Write all files in one message
  Write "src/auth/service.js"
  Write "src/auth/types.js"
  Write "tests/auth.test.js"
  Write "docs/auth.md"

  // Edit operations with proper coordination
  Edit "src/auth/service.js" {
    old_string: "// Implementation here",
    new_string: "// Updated implementation with hooks"
  }

  // Read operations for context
  Read "src/config/database.js"

  // Batch todos for tracking
  TodoWrite { todos: [file-related todos] }
```

### 1.3 Command Execution Pattern

Terminal operations batched for efficiency:

```javascript
[Single Message - Command Execution]:
  // Build and test operations
  Bash "npm run build"
  Bash "npm test"
  Bash "npm lint"

  // Development environment setup
  Bash "cargo make setup"
  Bash "cargo make test"

  // Git operations
  Bash "git add ."
  Bash "git commit -m 'Feature implementation'"

  // Quality validation
  Bash "./tools/claude-md-enforcer.sh"
```

---

## 2. Complex Workflows

### 2.1 Full-Stack Development Pattern

For comprehensive application development, use specialized agents:

```javascript
[Single Message - Full-Stack Development]:
  // Architecture and design
  Task("System Architect", "Design REST API with React frontend", "system-architect")

  // Backend implementation
  Task("Backend Developer", "Express.js API with MongoDB", "backend-dev")

  // Frontend implementation
  Task("Coder", "React frontend with TypeScript", "coder")

  // Testing and validation
  Task("Test Engineer", "E2E testing with Cypress", "tester")
  Task("Production Validator", "Docker setup and deployment", "production-validator")

  // Quality assurance
  Task("Code Analyzer", "Code review and optimization", "code-analyzer")
  Task("Performance Benchmarker", "Performance baseline establishment", "performance-benchmarker")

  // Comprehensive todo tracking
  TodoWrite { todos: [
    {id: "arch-1", content: "API architecture design", status: "in_progress", priority: "high"},
    {id: "backend-1", content: "Express.js implementation", status: "pending", priority: "high"},
    {id: "frontend-1", content: "React frontend", status: "pending", priority: "high"},
    {id: "test-1", content: "E2E tests", status: "pending", priority: "medium"},
    {id: "prod-1", content: "Docker deployment", status: "pending", priority: "high"},
    {id: "quality-1", content: "Code review", status: "pending", priority: "medium"},
    {id: "perf-1", content: "Performance baseline", status: "pending", priority: "medium"}
  ]}
```

**Case Study**: erlmcp's complex workflow pattern:

```javascript
[Single Message - erlmcp Complex Workflow]:
  Task("System Architect", "Design agent coordination architecture", "system-architect")
  Task("Backend Developer", "Implement registry and routing", "backend-dev")
  Task("Performance Benchmarker", "Benchmark core operations", "performance-benchmarker")
  Task("Production Validator", "Validate OTP patterns", "production-validator")

  // Generated comprehensive metrics
  // Registry: 553K msg/s
  // Queue: 971K msg/s
  // Sustained: 372K msg/s
```

### 2.2 Microservices Implementation Pattern

For distributed systems, use coordinated implementation:

```javascript
[Single Message - Microservices]:
  // Architecture phase
  Task("System Architect", "Design microservices architecture", "system-architect")

  // Service implementation (parallel)
  Task("Backend Developer", "Implement user service", "backend-dev")
  Task("Backend Developer", "Implement product service", "backend-dev")
  Task("Backend Developer", "Implement API Gateway", "backend-dev")

  // Infrastructure
  Task("Backend Developer", "Set up service discovery", "backend-dev")
  Task("Production Validator", "Docker compose configuration", "production-validator")

  // Testing
  Task("Test Engineer", "Service integration tests", "tester")
  Task("Test Engineer", "Contract tests", "tester")

  // Performance
  Task("Performance Benchmarker", "Load testing", "performance-benchmarker")

  TodoWrite { todos: [microservice-specific todos] }
```

### 2.3 API Development Pattern

For RESTful API development:

```javascript
[Single Message - API Development]:
  Task("System Architect", "Design API endpoints and data flow", "system-architect")
  Task("Backend Developer", "Express.js API with TypeScript", "backend-dev")
  Task("Backend Developer", "Database migrations and models", "backend-dev")
  Task("Backend Developer", "Authentication middleware", "backend-dev")

  // Documentation
  Task("API Docs", "Generate API documentation", "api-docs")

  // Testing
  Task("Test Engineer", "Unit and integration tests", "tester")

  // Performance
  Task("Performance Benchmarker", "API benchmarking", "performance-benchmarker")

  // Security
  Task("Security Manager", "Security validation", "security-manager")

  TodoWrite { todos: [API-specific todos] }
```

---

## 3. Multi-Agent Coordination

### 3.1 Swarm Coordination Pattern

For complex multi-agent projects:

```javascript
[Single Message - Swarm Coordination]:
  // Initialize swarm topology
  mcp__claude-flow__swarm_init {
    topology: "mesh",
    maxAgents: 8,
    memoryRetention: true
  }

  // Spawn specialized agents
  mcp__claude-flow__agent_spawn {
    type: "backend-dev",
    capabilities: ["Docker", "API"]
  }
  mcp__claude-flow__agent_spawn {
    type: "ml-developer",
    capabilities: ["ML", "Monitoring"]
  }
  mcp__claude-flow__agent_spawn {
    type: "cicd-engineer",
    capabilities: ["GitHub Actions", "Quality Gates"]
  }

  // Execute coordinated tasks
  Task("Backend Developer", "Build API with hooks integration", "backend-dev")
  Task("ML Developer", "ML pipeline with monitoring", "ml-developer")
  Task("CICD Engineer", "GitHub Actions setup", "cicd-engineer")
  Task("Performance Benchmarker", "End-to-end benchmarking", "performance-benchmarker")

  // Coordination hooks
  npx claude-flow@alpha hooks pre-task --description "Swarm coordination workflow"
```

### 3.2 Memory-Based Coordination Pattern

Agents share context through memory keys:

```javascript
[Single Message - Memory Coordination]:
  // Phase 1: Research
  Task("Researcher", "Analyze requirements and patterns", "researcher")
  npx claude-flow@alpha hooks post-edit --memory-key "research/patterns"

  // Phase 2: Design (restore research context)
  Task("System Architect", "Design based on research findings", "system-architect")
  npx claude-flow@alpha hooks session-restore --session-id "research/patterns"
  npx claude-flow@alpha hooks post-edit --memory-key "design/specs"

  // Phase 3: Implementation (restore design context)
  Task("Backend Developer", "Implement from design specs", "backend-dev")
  npx claude-flow@alpha hooks session-restore --session-id "design/specs"

  // Phase 4: Testing
  Task("Test Engineer", "Test implementation", "tester")

  // Batch todos for entire workflow
  TodoWrite { todos: [coordinated workflow todos] }
```

### 3.3 Cross-Agent Communication Pattern

Agents exchange data through coordinated memory:

```javascript
[Single Message - Cross-Agent Communication]:
  // Agent 1: Data analysis
  Task("Researcher", "Analyze system requirements", "researcher")
  npx claude-flow@alpha hooks post-edit --memory-key "requirements/analysis"

  // Agent 2: Architecture design (uses requirements)
  Task("System Architect", "Design architecture", "system-architect")
  npx claude-flow@alpha hooks session-restore --session-id "requirements/analysis"
  npx claude-flow@alpha hooks post-edit --memory-key "architecture/design"

  // Agent 3: Implementation (uses architecture)
  Task("Backend Developer", "Implement services", "backend-dev")
  npx claude-flow@alpha hooks session-restore --session-id "architecture/design"

  // Agent 4: Validation (validates all previous work)
  Task("Production Validator", "Validate implementation", "production-validator")

  // Communication todos
  TodoWrite { todos: [
    {id: "comm-1", content: "Requirements analysis", status: "in_progress", priority: "high"},
    {id: "comm-2", content: "Architecture design", status: "pending", priority: "high"},
    {id: "comm-3", content: "Implementation", status: "pending", priority: "high"},
    {id: "comm-4", content: "Validation", status: "pending", priority: "high"}
  ]}
```

---

## 4. Production Validation

### 4.1 Production-Ready Development Pattern

For deployment-critical code:

```javascript
[Single Message - Production Validation]:
  Task("Production Validator", "Validate Docker setup", "production-validator")
  Task("Production Validator", "Environment configuration", "production-validator")
  Task("Production Validator", "Security scanning", "production-validator")
  Task("Production Validator", "Health checks", "production-validator")
  Task("Production Validator", "Performance baselines", "production-validator")

  // Architecture review
  Task("System Architect", "Production scalability review", "system-architect")

  // Implementation
  Task("Backend Developer", "Production-grade error handling", "backend-dev")

  // Performance
  Task("Performance Benchmarker", "Production benchmarking", "performance-benchmarker")

  // Quality
  Task("Code Analyzer", "Deep technical review", "code-analyzer")

  // Comprehensive todos
  TodoWrite { todos: [
    {id: "prod-1", content: "Docker validation", status: "in_progress", priority: "high"},
    {id: "prod-2", content: "Security scan", status: "pending", priority: "high"},
    {id: "prod-3", content: "Health checks", status: "pending", priority: "high"},
    {id: "prod-4", content: "Performance baselines", status: "pending", priority: "high"},
    {id: "prod-5", content: "Scalability review", status: "pending", priority: "high"},
    {id: "prod-6", content: "Error handling", status: "pending", priority: "high"}
  ]}
```

**Case Study**: Production validation output:

```
✅ Docker validation: PASSED
✅ Security scan: 0 vulnerabilities
✅ Health checks: All endpoints responding
✅ Performance baselines: Established
✅ Scalability review: Architecture approved
✅ Error handling: Comprehensive coverage

Output: 178KB comprehensive documentation with 10-point health check system
```

### 4.2 Quality Gate Integration Pattern

Integrate quality gates into development workflow:

```javascript
[Single Message - Quality Gates]:
  // Development tasks
  Task("Coder", "Implement feature", "coder")
  Task("Tester", "Write tests", "tester")
  Task("Reviewer", "Review code", "reviewer")

  // Quality validation
  Task("Production Validator", "Validate quality gates", "production-validator")
  Task("Code Analyzer", "Check metrics", "code-analyzer")

  // Pre-commit validation hook
  npx claude-flow@alpha hooks pre-task --description "Quality gate validation"

  // Post-commit validation
  npx claude-flow@alpha hooks post-task --task-id "quality-validation"

  // Quality todos
  TodoWrite { todos: [
    {id: "quality-1", content: "Implementation", status: "in_progress", priority: "high"},
    {id: "quality-2", content: "Testing", status: "pending", priority: "high"},
    {id: "quality-3", content: "Review", status: "pending", priority: "high"},
    {id: "quality-4", content: "Quality validation", status: "pending", priority: "high"}
  ]}
```

### 4.3 Deployment Pipeline Pattern

For continuous deployment:

```javascript
[Single Message - Deployment Pipeline]:
  // Build and test
  Task("Backend Developer", "Build application", "backend-dev")
  Task("Test Engineer", "Run comprehensive tests", "tester")

  // Containerization
  Task("Production Validator", "Create Docker images", "production-validator")

  // Infrastructure
  Task("Backend Developer", "Setup deployment environment", "backend-dev")
  Task("CICD Engineer", "Configure deployment pipeline", "cicd-engineer")

  // Monitoring
  Task("Performance Benchmarker", "Setup monitoring", "performance-benchmarker")

  // Validation
  Task("Production Validator", "Pre-deployment validation", "production-validator")

  // Deployment todos
  TodoWrite { todos: [
    {id: "deploy-1", content: "Build application", status: "in_progress", priority: "high"},
    {id: "deploy-2", content: "Run tests", status: "pending", priority: "high"},
    {id: "deploy-3", content: "Create Docker images", status: "pending", priority: "high"},
    {id: "deploy-4", content: "Setup environment", status: "pending", priority: "high"},
    {id: "deploy-5", content: "Configure pipeline", status: "pending", priority: "high"},
    {id: "deploy-6", content: "Pre-deployment validation", status: "pending", priority: "high"}
  ]}
```

---

## 5. Performance Optimization

### 5.1 Performance Analysis Pattern

For performance-critical applications:

```javascript
[Single Message - Performance Analysis]:
  Task("Performance Benchmarker", "Profile application bottlenecks", "performance-benchmarker")
  Task("System Architect", "Design optimization strategies", "system-architect")
  Task("Backend Developer", "Implement optimizations", "backend-dev")
  Task("Code Analyzer", "Review performance anti-patterns", "code-analyzer")

  // Performance tracking
  npx claude-flow@alpha hooks notify --message "Starting performance analysis"

  // Performance todos
  TodoWrite { todos: [
    {id: "perf-1", content: "Profile bottlenecks", status: "in_progress", priority: "high"},
    {id: "perf-2", content: "Design strategies", status: "pending", priority: "high"},
    {id: "perf-3", content: "Implement optimizations", status: "pending", priority: "high"},
    {id: "perf-4", content: "Review anti-patterns", status: "pending", priority: "medium"}
  ]}
```

### 5.2 Benchmarking Workflows

Establish comprehensive performance baselines:

```javascript
[Single Message - Comprehensive Benchmarking]:
  // Core operations (erlmcp-style)
  Task("Performance Benchmarker", "Registry performance test", "performance-benchmarker")
  Task("Performance Benchmarker", "Queue performance test", "performance-benchmarker")
  Task("Performance Benchmarker", "Pool performance test", "performance-benchmarker")
  Task("Performance Benchmarker", "Session performance test", "performance-benchmarker")

  // Network performance
  Task("Performance Benchmarker", "TCP socket throughput", "performance-benchmarker")
  Task("Performance Benchmarker", "HTTP request latency", "performance-benchmarker")

  // Stress testing
  Task("Performance Benchmarker", "Sustained load testing 30min", "performance-benchmarker")
  Task("Performance Benchmarker", "Chaos testing", "performance-benchmarker")

  // Integration performance
  Task("Performance Benchmarker", "End-to-end workflow latency", "performance-benchmarker")

  // Benchmarking todos
  TodoWrite { todos: [
    {id: "bench-1", content: "Core operations", status: "in_progress", priority: "high"},
    {id: "bench-2", content: "Network performance", status: "pending", priority: "high"},
    {id: "bench-3", content: "Stress testing", status: "pending", priority: "high"},
    {id: "bench-4", content: "Integration performance", status: "pending", priority: "high"}
  ]}
```

### 5.3 Optimization Pattern

Iterative performance optimization:

```javascript
[Single Message - Optimization Cycle]:
  // Baseline establishment
  Task("Performance Benchmarker", "Establish current performance", "performance-benchmarker")

  // Analysis and design
  Task("System Architect", "Identify optimization opportunities", "system-architect")
  Task("Code Analyzer", "Review bottlenecks", "code-analyzer")

  // Implementation
  Task("Backend Developer", "Implement optimizations", "backend-dev")

  // Validation
  Task("Performance Benchmarker", "Measure improvement", "performance-benchmarker")

  // Optimization todos
  TodoWrite { todos: [
    {id: "opt-1", content: "Establish baseline", status: "in_progress", priority: "high"},
    {id: "opt-2", content: "Identify opportunities", status: "pending", priority: "high"},
    {id: "opt-3", content: "Implement optimizations", status: "pending", priority: "high"},
    {id: "opt-4", content: "Measure improvement", status: "pending", priority: "high"}
  ]}
```

---

## 6. Advanced Patterns

### 6.1 Multi-Phase Complex Workflow

For enterprise-level projects:

```javascript
[Single Message - Multi-Phase Workflow]:
  // Phase 1: Research and Design
  Task("Researcher", "Market research and requirements", "researcher")
  Task("System Architect", "System architecture design", "system-architect")
  Task("Planner", "Project planning and milestones", "planner")

  // Phase 2: Implementation (parallel)
  Task("Backend Developer", "Backend services", "backend-dev")
  Task("Backend Developer", "Database layer", "backend-dev")
  Task("Coder", "Frontend application", "coder")
  Task("Coder", "Mobile app", "coder")

  // Phase 3: Infrastructure
  Task("Backend Developer", "DevOps setup", "backend-dev")
  Task("CICD Engineer", "CI/CD pipeline", "cicd-engineer")

  // Phase 4: Testing and Quality
  Task("Test Engineer", "Comprehensive testing", "tester")
  Task("Production Validator", "Production validation", "production-validator")
  Task("Code Analyzer", "Quality review", "code-analyzer")

  // Phase 5: Deployment and Monitoring
  Task("Production Validator", "Deployment", "production-validator")
  Task("Performance Benchmarker", "Monitoring setup", "performance-benchmarker")

  // Comprehensive todo tracking
  TodoWrite { todos: [
    {id: "phase1-1", content: "Market research", status: "in_progress", priority: "high"},
    {id: "phase1-2", content: "Architecture design", status: "pending", priority: "high"},
    {id: "phase1-3", content: "Project planning", status: "pending", priority: "high"},
    {id: "phase2-1", content: "Backend services", status: "pending", priority: "high"},
    {id: "phase2-2", content: "Database layer", status: "pending", priority: "high"},
    {id: "phase2-3", content: "Frontend application", status: "pending", priority: "high"},
    {id: "phase2-4", content: "Mobile app", status: "pending", priority: "high"},
    {id: "phase3-1", content: "DevOps setup", status: "pending", priority: "high"},
    {id: "phase3-2", content: "CI/CD pipeline", status: "pending", priority: "high"},
    {id: "phase4-1", content: "Comprehensive testing", status: "pending", priority: "high"},
    {id: "phase4-2", content: "Production validation", status: "pending", priority: "high"},
    {id: "phase4-3", content: "Quality review", status: "pending", priority: "high"},
    {id: "phase5-1", content: "Deployment", status: "pending", priority: "high"},
    {id: "phase5-2", content: "Monitoring setup", status: "pending", priority: "high"}
  ]}
```

### 6.2 Crisis Management Pattern

For emergency situations and hotfixes:

```javascript
[Single Message - Crisis Management]:
  // Immediate response
  Task("Backend Developer", "Emergency patch deployment", "backend-dev")
  Task("Production Validator", "Quick security validation", "production-validator")

  // Root cause analysis
  Task("Code Analyzer", "Analyze root cause", "code-analyzer")
  Task("System Architect", "Review architecture impact", "system-architect")

  // Comprehensive fix
  Task("Backend Developer", "Implement proper fix", "backend-dev")
  Task("Test Engineer", "Emergency testing", "tester")

  // Prevention
  Task("Code Analyzer", "Add preventive measures", "code-analyzer")
  Task("CICD Engineer", "Add automated detection", "cicd-engineer")

  // Crisis todos
  TodoWrite { todos: [
    {id: "crisis-1", content: "Emergency patch", status: "in_progress", priority: "critical"},
    {id: "crisis-2", content: "Security validation", status: "pending", priority: "critical"},
    {id: "crisis-3", content: "Root cause analysis", status: "pending", priority: "high"},
    {id: "crisis-4", content: "Architecture review", status: "pending", priority: "high"},
    {id: "crisis-5", content: "Proper fix", status: "pending", priority: "high"},
    {id: "crisis-6", content: "Emergency testing", status: "pending", priority: "high"},
    {id: "crisis-7", content: "Prevention measures", status: "pending", priority: "high"}
  ]}
```

### 6.3 Migration Pattern

For system migrations and upgrades:

```javascript
[Single Message - Migration Workflow]:
  // Assessment
  Task("Researcher", "Current system analysis", "researcher")
  Task("System Architect", "Migration strategy", "system-architect")

  // Planning
  Task("Planner", "Migration plan", "planner")
  Task("Backend Developer", "Data migration scripts", "backend-dev")

  // Implementation
  Task("Backend Developer", "New system implementation", "backend-dev")
  Task("Coder", "Interface adaptation", "coder")

  // Testing
  Task("Test Engineer", "Migration testing", "tester")
  Task("Production Validator", "Migration validation", "production-validator")

  // Deployment
  Task("Backend Developer", "Migration execution", "backend-dev")
  Task("Production Validator", "Post-migration validation", "production-validator")

  // Migration todos
  TodoWrite { todos: [
    {id: "mig-1", content: "System analysis", status: "in_progress", priority: "high"},
    {id: "mig-2", content: "Migration strategy", status: "pending", priority: "high"},
    {id: "mig-3", content: "Migration plan", status: "pending", priority: "high"},
    {id: "mig-4", content: "Data scripts", status: "pending", priority: "high"},
    {id: "mig-5", content: "New system", status: "pending", priority: "high"},
    {id: "mig-6", content: "Interface adaptation", status: "pending", priority: "high"},
    {id: "mig-7", content: "Migration testing", status: "pending", priority: "high"},
    {id: "mig-8", content: "Migration execution", status: "pending", priority: "high"},
    {id: "mig-9", content: "Post-migration validation", status: "pending", priority: "high"}
  ]}
```

---

## 7. Integration Patterns

### 7.1 CI/CD Integration

GitHub Actions example for Claude Code CLI:

```yaml
name: Claude Code CI

on: [push, pull_request]

jobs:
    validate:
        runs-on: ubuntu-latest
        steps:
            - uses: actions/checkout@v2

            - name: Setup environment
              run: |
                  npm install
                  cargo make setup

            - name: Run Claude Code validation
              run: |
                  npx claude-flow@alpha hooks pre-task
                  cargo-make verify
                  npx claude-flow@alpha hooks post-task-validate

            - name: Run tests
              run: |
                  npm test
                  cargo make test

            - name: Quality gate
              run: |
                  ./tools/claude-md-enforcer.sh
                  npx claude-flow@alpha quality metrics
```

### 7.2 IDE Integration

VS Code extension example:

```json
{
    "contributes": {
        "commands": [
            {
                "command": "claude-code.spawn-agent",
                "title": "Spawn Claude Code Agent"
            },
            {
                "command": "claude-code.run-workflow",
                "title": "Run Claude Code Workflow"
            },
            {
                "command": "claude-code.validate-quality",
                "title": "Validate Quality Gates"
            }
        ],
        "keybindings": [
            {
                "command": "claude-code.spawn-agent",
                "key": "ctrl+shift+c a"
            }
        ]
    }
}
```

### 7.3 Git Integration

Pre-commit hook for quality validation:

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run quality validation
echo "Running Claude Code quality validation..."
./tools/claude-md-enforcer.sh

# Run tests if present
if [ -f "package.json" ]; then
    npm test
fi

if [ -f "Makefile" ]; then
    cargo make test
fi

echo "Quality validation completed successfully"
```

---

## Conclusion

This HOWTO guide provides practical patterns for implementing Claude Code CLI in real-world scenarios. Each pattern follows the Single Message Principle and demonstrates proper agent coordination.

Key takeaways:
1. Always batch operations in single messages
2. Use appropriate agent types for specific tasks
3. Implement comprehensive todo tracking
4. Follow coordination protocols with hooks
5. Integrate quality gates into your workflow
6. Use production validation for deployment-critical code

By following these patterns, developers can leverage the full power of Claude Code CLI to achieve zero-defect delivery with optimal efficiency.

---

*Next: Explore technical specifications in the [REFERENCE](../REFERENCE.md) section.*