# REFERENCE - Technical Specifications

## Introduction

This reference section provides authoritative, exhaustive technical information about Claude Code CLI. It covers agent specifications, MCP tools, coordination protocols, and performance metrics with precise details for advanced users and integrators.

---

## 1. Agent Types

### 1.1 Hyper-Advanced Agents

These agents provide enterprise-grade solutions with comprehensive documentation and validation.

| Agent | Domain | Specialization | Technical Specifications | Output Scale |
|-------|--------|---------------|-------------------------|--------------|
| production-validator | DevOps | Docker, deployment, infrastructure | Multi-protocol validation | 178KB+ docs |
| code-analyzer | Code Quality | Technical debt, architecture review | 5x comprehensive analysis | Extended reports |
| system-architect | Architecture | Integration, microservices, patterns | Production-ready designs | Architecture docs |
| performance-benchmarker | Performance | Benchmarking, optimization | 35+ benchmark scenarios | Performance reports |
| backend-dev | Backend | APIs, databases, infrastructure | Production-ready code | 100% coverage |
| task-orchestrator | Workflow | Multi-phase coordination | End-to-end orchestration | Workflow docs |

#### Technical Details

**production-validator Agent**:
```javascript
{
  "domain": "DevOps",
  "protocols": ["Docker", "Kubernetes", "AWS", "GCP"],
  "validation_scopes": [
    "containerization",
    "security_scanning",
    "performance_baselines",
    "health_checks",
    "deployment_readiness"
  ],
  "output_format": {
    "documentation": "comprehensive",
    "checklists": "automated",
    "metrics": "authoritative"
  },
  "quality_gates": [
    "0_vulnerabilities",
    "100%_health_checks",
    "performance_baselines",
    "comprehensive_coverage"
  ]
}
```

**performance-benchmarker Agent**:
```javascript
{
  "domain": "Performance",
  "benchmark_categories": [
    "core_operations",
    "network_real",
    "stress",
    "chaos",
    "integration"
  ],
  "measurement_units": {
    "throughput": "msg_per_s",
    "latency": "microseconds",
    "memory": "mib_per_conn",
    "rss": "mib_per_node"
  },
  "precision_levels": {
    "workload_id": "required",
    "transport": "required",
    "duration_s": "required",
    "scope": "required"
  }
}
```

### 1.2 Core Development Agents

| Agent | Purpose | Technical Scope | Common Patterns |
|-------|---------|-----------------|------------------|
| coder | Implementation | Full development lifecycle | TDD, refactoring, patterns |
| tester | Testing | Unit, integration, e2e | Chicago School TDD |
| reviewer | Quality review | Code analysis, security | Static analysis, security scan |
| researcher | Analysis | Requirements, patterns | Market research, technical analysis |
| planner | Strategy | Project planning, coordination | Milestone tracking, dependency management |

#### Technical Specifications

**coder Agent**:
```javascript
{
  "capabilities": [
    "implementation",
    "refactoring",
    "documentation",
    "error_handling",
    "optimization"
  ],
  "patterns": [
    "tdd",
    "clean_architecture",
    "domain_driven_design",
    "solid_principles"
  ],
  "output_requirements": {
    "test_coverage": "80%+",
    "documentation": "comprehensive",
    "error_handling": "robust"
  }
}
```

### 1.3 Specialized Domain Agents

| Agent | Domain | Technical Expertise | Integration Points |
|-------|--------|-------------------|-------------------|
| ml-developer | Machine Learning | ML pipelines, monitoring | OTEL, monitoring systems |
| cicd-engineer | CI/CD | GitHub Actions, quality gates | GitHub, deployment pipelines |
| mobile-dev | Mobile | iOS/Android, native apps | App Store, Play Store |
| api-docs | API Documentation | OpenAPI, Swagger | API gateways, documentation portals |
| security-manager | Security | Security validation, compliance | Security scanners, compliance frameworks |

---

## 2. MCP Tools

### 2.1 Coordination Tools

#### Swarm Initialization
```javascript
mcp__claude-flow__swarm_init {
    "topology": "mesh",  // Options: "mesh", "hierarchical", "adaptive"
    "maxAgents": 6,     // Maximum concurrent agents
    "memoryRetention": true,  // Keep memory across sessions
    "clustering": {
        "nodes": 1,     // Number of cluster nodes
        "loadBalancing": "round-robin",  // Load balancing strategy
        "failover": true  // Enable failover
    },
    "hooks": {
        "pre_task": true,    // Enable pre-task hooks
        "post_task": true,   // Enable post-task hooks
        "session_restore": true  // Enable session restore
    }
}
```

#### Agent Spawning
```javascript
mcp__claude-flow__agent_spawn {
    "type": "backend-dev",  // Agent type
    "capabilities": ["Docker", "API", "Database"],  // Specific skills
    "memoryKey": "swarm/backend",  // Memory key for coordination
    "priority": "high",  // Task priority
    "constraints": {  // Execution constraints
        "max_duration": "3600",  // Maximum duration in seconds
        "memory_limit": "1024",  // Memory limit in MB
        "cpu_limit": "50"  // CPU percentage limit
    }
}
```

#### Task Orchestration
```javascript
mcp__claude-flow__task_orchestrate {
    "phases": ["research", "design", "implement", "test"],  // Workflow phases
    "dependencies": ["research", "design"],  // Phase dependencies
    "parallel": ["implement", "test"],  // Parallel phases
    "memoryKeys": {  // Memory keys for each phase
        "research": "research/findings",
        "design": "design/specs",
        "implement": "implementation/code",
        "test": "test/results"
    },
    "timeout": "7200",  // Total timeout in seconds
    "retryPolicy": {  // Retry configuration
        "maxRetries": 3,
        "backoff": "exponential",
        "delay": 1000  // Base delay in ms
    }
}
```

### 2.2 Monitoring Tools

#### Swarm Status
```javascript
mcp__claude-flow__swarm_status {
    "includeMetrics": true,  // Include performance metrics
    "includeMemory": true,  // Include memory usage
    "includeAgents": true,  // Include agent status
    "includeTasks": true,  // Include task status
    "limit": 100  // Maximum number of entries
}
```

#### Agent Metrics
```javascript
mcp__claude-flow__agent_metrics {
    "agentId": "system-architect",  // Specific agent or all
    "metrics": [
        "performance",  // Performance metrics
        "quality",     // Quality metrics
        "efficiency",  // Efficiency metrics
        "memory",      // Memory usage
        "tasks"        // Task completion metrics
    ],
    "timeRange": "7d"  // Time range for metrics
}
```

#### Memory Management
```javascript
mcp__claude-flow__memory_usage {
    "includePatterns": true,  // Include trained patterns
    "includeSessions": true,  // Include session data
    "includeMetrics": true,  // Include metrics
    "cleanup": false,  // Don't cleanup old data
    "format": "detailed"  // Output format
}
```

---

## 3. Coordination Protocols

### 3.1 Hook Execution Protocol

#### Pre-Task Hook
```bash
npx claude-flow@alpha hooks pre-task \
    --description "Task description" \
    --agent-type "backend-dev" \
    --priority "high" \
    --memory-restore "swarm/previous" \
    --validate-quality true
```

#### Post-Edit Hook
```bash
npx claude-flow@alpha hooks post-edit \
    --file "src/app.js" \
    --memory-key "swarm/backend" \
    --train-pattern true \
    --notify "File updated successfully"
```

#### Post-Task Hook
```bash
npx claude-flow@alpha hooks post-task \
    --task-id "backend-implementation" \
    --export-metrics true \
    --cleanup-memory true \
    --generate-summary true
```

### 3.2 Session Management Protocol

#### Session Restore
```bash
npx claude-flow@alpha hooks session-restore \
    --session-id "swarm-2024-01-29" \
    --include-context true \
    --include-agents true \
    --include-memory true
```

#### Session End
```bash
npx claude-flow@alpha hooks session-end \
    --export-metrics true \
    --export-memory true \
    --generate-summary true \
    --cleanup false
```

#### Memory Persistence
```javascript
{
    "session_id": "swarm-2024-01-29",
    "timestamp": "2024-01-29T10:30:00Z",
    "context": {
        "current_task": "backend_implementation",
        "priorities": ["high", "medium"],
        "constraints": ["time_budget", "quality_gates"]
    },
    "agents": {
        "active": [
            {"id": "backend-dev", "status": "running", "start_time": "10:00:00Z"}
        ],
        "completed": [
            {"id": "system-architect", "status": "completed", "duration": "1800s"}
        ]
    },
    "memory": {
        "patterns": [],
        "decisions": [],
        "history": []
    }
}
```

### 3.3 Quality Assurance Protocol

#### Pre-Commit Validation
```bash
npx claude-flow@alpha hooks pre-commit \
    --staged-files "true" \
    --validate-quality "true" \
    --run-tests "true" \
    --check-coverage "true"
```

#### Quality Metrics
```javascript
{
    "compilation": {
        "status": "ok",
        "errors": 0,
        "warnings": 0
    },
    "tests": {
        "pass_rate": 100,
        "total": 156,
        "passed": 156,
        "failed": 0,
        "coverage": 92.5
    },
    "security": {
        "vulnerabilities": 0,
        "scanned": true
    },
    "performance": {
        "regression": false,
        "baseline": "2.69M ops/sec",
        "current": "2.71M ops/sec"
    }
}
```

---

## 4. Memory Management

### 4.1 Memory Structure

#### Full Memory Schema
```javascript
{
    "sessions": {
        "swarm-id": {
            "metadata": {
                "created": "2024-01-29T10:00:00Z",
                "last_updated": "2024-01-29T10:30:00Z",
                "duration": "1800s"
            },
            "context": {
                "current_task": "implementation",
                "priorities": ["high", "medium", "low"],
                "constraints": ["time", "quality", "budget"],
                "requirements": ["performance", "scalability", "security"]
            },
            "agents": {
                "active": [],
                "completed": [],
                "failed": [],
                "metrics": {
                    "total_tasks": 10,
                    "completed_tasks": 8,
                    "failed_tasks": 0,
                    "average_duration": "300s"
                }
            },
            "memory": {
                "patterns": [],
                "decisions": [],
                "history": [],
                "artifacts": []
            },
            "performance": {
                "throughput": 372000,
                "latency_p50": 10.5,
                "latency_p95": 45.2,
                "latency_p99": 89.7,
                "memory_usage": 256,
                "cpu_usage": 45.2
            }
        }
    },
    "global": {
        "patterns": {
            "successful": [],
            "failed": [],
            "neural": []
        },
        "metrics": {
            "historical": [],
            "averages": {
                "throughput": 350000,
                "latency_p50": 12.0,
                "coverage": 88.5
            }
        },
        "configuration": {
            "default_topology": "mesh",
            "max_agents": 6,
            "memory_retention": true
        }
    }
}
```

#### Memory Persistence Format
```javascript
{
    "version": "1.0.0",
    "format": "json",
    "compression": "gzip",
    "encryption": "aes-256",
    "data": {
        // Session data as above
    },
    "metadata": {
        "created": "2024-01-29T10:00:00Z",
        "expires": "2024-02-05T10:00:00Z",
        "size": "2.5MB",
        "checksum": "sha256:..."
    }
}
```

### 4.2 Memory Operations

#### Memory Store
```bash
npx claude-flow@alpha memory store \
    --key "swarm/backend" \
    --data '{"status": "completed", "result": "success"}' \
    --type "json" \
    --persist true
```

#### Memory Retrieve
```bash
npx claude-flow@alpha memory retrieve \
    --key "swarm/backend" \
    --format "json" \
    --decrypt true
```

#### Memory Cleanup
```bash
npx claude-flow@alpha memory cleanup \
    --older-than "30d" \
    --pattern-only false \
    --dry-run false
```

### 4.3 Pattern Training

#### Pattern Learning
```javascript
{
    "pattern_id": "microservice_implementation",
    "trigger": "system_architect + backend_dev",
    "success_rate": 0.95,
    "usage_count": 42,
    "context": {
        "domain": "microservices",
        "complexity": "medium",
        "team_size": "5-10"
    },
    "steps": [
        {
            "order": 1,
            "agent": "system_architect",
            "task": "design architecture",
            "success_rate": 0.98
        },
        {
            "order": 2,
            "agent": "backend_dev",
            "task": "implement services",
            "success_rate": 0.92
        }
    ],
    "optimizations": [
        "parallel_backend_development",
        "early_validation"
    ]
}
```

---

## 5. Performance Metrics

### 5.1 Benchmark Results

#### erlmcp Performance Baselines
```erlang
% Core operations (Jan 2026 baseline)
[
    {registry, 553000},    % msg/s
    {queue, 971000},       % msg/s
    {pool, 149000},        % msg/s
    {session, 242000},    % msg/s
    {network_io, 43000}   % msg/s (4KB real packets)
]

% Sustained performance
{throughput, 372000},    % msg/s (60M ops/30min)
{capacity, 40000},       % concurrent connections per node
{scalability, 100000}   % requires clustering
```

#### Agent Performance Metrics
```javascript
{
    "performance": {
        "baseline": {
            "solve_rate": 84.8,        % SWE-Bench
            "token_reduction": 32.3,    % token usage
            "speed_improvement": 4.4,  % x faster
            "neural_models": 27
        },
        "current": {
            "solve_rate": 87.2,
            "token_reduction": 35.1,
            "speed_improvement": 4.8,
            "neural_models": 31
        }
    }
}
```

### 5.2 Metrology Standards

#### Canonical Units
```javascript
{
    "throughput": {
        "unit": "msg_per_s",
        "description": "Messages processed per second",
        "precision": "integer"
    },
    "latency": {
        "unit": "us",
        "description": "Microseconds",
        "precision": "float",
        "percentiles": ["p50", "p95", "p99"]
    },
    "memory": {
        "connection_heap": {
            "unit": "mib_per_conn",
            "description": "Megabytes per connection"
        },
        "node_total": {
            "unit": "mib_per_node",
            "description": "Megabytes per node"
        }
    },
    "scope": {
        "required": ["workload_id", "transport", "duration_s", "precision"]
    }
}
```

#### Validation Protocol
```javascript
function validateMetrology(metrics) {
    const required = ['workload_id', 'transport', 'duration_s', 'scope'];

    for (const field of required) {
        if (!metrics[field]) {
            throw new Error(`Missing required field: ${field}`);
        }
    }

    // Validate units
    if (metrics.throughput && !metrics.throughput.includes('msg_per_s')) {
        throw new Error('Invalid throughput unit');
    }

    // Validate precision
    if (metrics.precision && metrics.precision !== 'canonical') {
        throw new Error('Invalid precision setting');
    }

    return true;
}
```

### 5.3 Performance Optimization

#### Optimization Targets
```javascript
{
    "optimization_targets": {
        "throughput": {
            "current": 350000,
            "target": 500000,
            "improvement": "42.8%"
        },
        "latency": {
            "p50_current": 12.0,
            "p50_target": 8.0,
            "p95_current": 45.2,
            "p95_target": 30.0,
            "p99_current": 89.7,
            "p99_target": 60.0
        },
        "memory": {
            "current": 256,
            "target": 200,
            "improvement": "21.9%"
        }
    }
}
```

#### Bottleneck Analysis
```javascript
{
    "bottlenecks": [
        {
            "component": "network_io",
            "current": 43000,
            "bottleneck": true,
            "limitation": "4KB packet size",
            "solution": "increase_packet_size",
            "estimated_improvement": "2x"
        },
        {
            "component": "registry",
            "current": 553000,
            "bottleneck": false,
            "limitation": "none",
            "solution": "no_action_needed",
            "estimated_improvement": "none"
        }
    ]
}
```

---

## 6. API Specifications

### 6.1 Task Tool API

#### Task Execution
```javascript
/**
 * @param {string} name - Agent name
 * @param {string} description - Task description
 * @param {string} type - Agent type
 * @param {Object} options - Additional options
 * @returns {Promise<TaskResult>}
 */
function Task(name, description, type, options = {}) {
    return {
        id: generateTaskId(),
        name: name,
        description: description,
        type: type,
        options: {
            priority: options.priority || 'medium',
            timeout: options.timeout || 3600,
            memoryLimit: options.memoryLimit || 1024,
            ...options
        },
        status: 'pending',
        startTime: new Date().toISOString(),
        result: null
    };
}
```

#### TodoWrite API
```javascript
/**
 * @param {Array<Todo>} todos - Array of todo items
 * @param {Object} options - Additional options
 */
function TodoWrite({ todos, options = {} }) {
    const minimumTodos = 5;  // Minimum 5 todos per call

    if (todos.length < minimumTodos) {
        throw new Error(`Minimum ${minimumTodos} todos required per call`);
    }

    return {
        timestamp: new Date().toISOString(),
        todos: todos.map(todo => ({
            id: todo.id,
            content: todo.content,
            status: todo.status || 'pending',
            priority: todo.priority || 'medium',
            assigned_to: todo.assigned_to || 'unassigned',
            estimated_hours: todo.estimated_hours || 0,
            ...todo
        })),
        metadata: {
            batch_size: todos.length,
            minimum_enforced: minimumTodos,
            ...options
        }
    };
}
```

### 6.2 MCP Tools API

#### Swarm Init
```javascript
/**
 * @param {Object} config - Swarm configuration
 * @returns {Promise<SwarmConfig>}
 */
async function swarmInit(config) {
    const defaultConfig = {
        topology: 'mesh',
        maxAgents: 6,
        memoryRetention: true,
        clustering: {
            nodes: 1,
            loadBalancing: 'round-robin',
            failover: false
        }
    };

    const finalConfig = { ...defaultConfig, ...config };

    // Validate configuration
    validateSwarmConfig(finalConfig);

    // Initialize swarm
    return {
        id: generateSwarmId(),
        config: finalConfig,
        status: 'initialized',
        agents: [],
        createdAt: new Date().toISOString()
    };
}
```

#### Memory Management API
```javascript
/**
 * @param {string} key - Memory key
 * @param {any} data - Data to store
 * @param {Object} options - Storage options
 */
function memoryStore(key, data, options = {}) {
    const defaultOptions = {
        type: 'json',
        persist: true,
        encrypt: true,
        compress: true
    };

    const finalOptions = { ...defaultOptions, ...options };

    // Store data
    return {
        key: key,
        data: data,
        options: finalOptions,
        timestamp: new Date().toISOString(),
        checksum: generateChecksum(data)
    };
}
```

---

## 7. Error Handling

### 7.1 Error Codes

#### Task Execution Errors
```javascript
const TaskErrors = {
    AGENT_NOT_FOUND: {
        code: 'AGENT_001',
        message: 'Specified agent type not found',
        severity: 'error',
        recovery: 'retry_with_fallback'
    },
    TASK_TIMEOUT: {
        code: 'TASK_001',
        message: 'Task execution timeout',
        severity: 'warning',
        recovery: 'retry_with_longer_timeout'
    },
    MEMORY_FAILURE: {
        code: 'MEMORY_001',
        message: 'Memory operation failed',
        severity: 'error',
        recovery: 'reset_memory_and_retry'
    },
    QUALITY_GATE_FAILED: {
        code: 'QUALITY_001',
        message: 'Quality gate validation failed',
        severity: 'critical',
        recovery: 'manual_intervention_required'
    }
};
```

#### MCP Tool Errors
```javascript
const MCPErrorCodes = {
    SWARM_INIT_FAILED: {
        code: 'MCP_001',
        message: 'Swarm initialization failed',
        severity: 'error',
        recovery: 'check_configuration'
    },
    AGENT_SPAWN_FAILED: {
        code: 'MCP_002',
        message: 'Agent spawning failed',
        severity: 'error',
        recovery: 'verify_capabilities'
    },
    HOOK_EXECUTION_FAILED: {
        code: 'MCP_003',
        message: 'Hook execution failed',
        severity: 'warning',
        recovery: 'continue_without_hook'
    },
    MEMORY_ACCESS_DENIED: {
        code: 'MCP_004',
        message: 'Memory access denied',
        severity: 'error',
        recovery: 'check_permissions'
    }
};
```

### 7.2 Error Recovery Patterns

#### Task Recovery
```javascript
async function handleTaskError(error, task) {
    switch (error.code) {
        case TaskErrors.AGENT_NOT_FOUND.code:
            return retryWithFallback(task);

        case TaskErrors.TASK_TIMEOUT.code:
            return retryWithLongerTimeout(task);

        case TaskErrors.MEMORY_FAILURE.code:
            await resetMemory();
            return retryTask(task);

        case TaskErrors.QUALITY_GATE_FAILED.code:
            await notifyQualityTeam();
            throw new Error('Manual intervention required');

        default:
            logError(error);
            throw error;
    }
}
```

#### Swarm Recovery
```javascript
async function handleSwarmError(error, swarm) {
    switch (error.code) {
        case MCPErrorCodes.SWARM_INIT_FAILED.code:
            await validateConfiguration(swarm.config);
            return retrySwarmInit(swarm.config);

        case MCPErrorCodes.AGENT_SPAWN_FAILED.code:
            await verifyCapabilities(swarm.config);
            return spawnFallbackAgents(swarm);

        case MCPErrorCodes.HOOK_EXECUTION_FAILED.code:
            return continueWithoutHooks(swarm);

        case MCPErrorCodes.MEMORY_ACCESS_DENIED.code:
            await resetMemoryPermissions();
            return retrySwarmOperation(swarm);

        default:
            logSwarmError(error);
            throw error;
    }
}
```

---

## 8. Security Specifications

### 8.1 Security Validation

#### Agent Security
```javascript
const AgentSecurity = {
    sandbox: {
        enabled: true,
        isolation_level: 'process',
        allowed_operations: [
            'file_read',
            'file_write',
            'network_request',
            'process_spawn'
        ],
        denied_operations: [
            'system_access',
            'raw_network_socket',
            'memory_access'
        ]
    },
    validation: {
        code_scan: true,
        dependency_scan: true,
        runtime_validation: true
    }
};
```

#### Memory Security
```javascript
const MemorySecurity = {
    encryption: {
        algorithm: 'aes-256-gcm',
        key_rotation_days: 30,
        storage_encryption: true,
        transmission_encryption: true
    },
    access_control: {
        authentication: 'oauth2',
        authorization: 'rbac',
        audit_logging: true
    },
    retention: {
        default_days: 90,
        sensitive_data_days: 7,
        compression_after_days: 30
    }
};
```

### 8.2 Compliance Standards

#### GDPR Compliance
```javascript
const GDPRCompliance = {
    data_processing: {
        lawfulness_basis: 'consent',
        data_minimization: true,
        purpose_limitation: true
    },
    data_subject_rights: {
        access: true,
        rectification: true,
        erasure: true,
        portability: true,
        objection: true
    },
    breach_notification: {
        within_72_hours: true,
        supervisory_authority: true
    }
};
```

#### SOC 2 Compliance
```javascript
const SOC2Compliance = {
    security: {
        access_controls: true,
        monitoring: true,
        change_management: true
    },
    availability: {
        system_monitoring: true,
        disaster_recovery: true,
        backup_processes: true
    },
    processing_integrity: {
        data_validation: true,
        quality_monitoring: true
    }
};
```

---

## Conclusion

This reference section provides comprehensive technical specifications for Claude Code CLI. It covers agent types, MCP tools, coordination protocols, memory management, performance metrics, APIs, error handling, and security requirements.

The specifications are designed for integrators, advanced users, and system architects who need precise details for implementation, integration, and optimization of Claude Code CLI in production environments.

All specifications follow industry best practices and provide the technical foundation for building robust, scalable, and secure AI-assisted development systems.

---

*Previous: Practical implementation patterns in the [HOWTO](../HOWTO.md) section.*