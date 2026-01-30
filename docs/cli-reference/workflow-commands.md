# CLI Reference: Task Orchestration and Workflow Commands

This document provides a comprehensive reference for all CLI commands related to task orchestration and workflow management in erlmcp.

## Core Orchestration Commands

### `/claude-flow-swarm` - Multi-Agent Coordination

**Purpose**: Coordinate complex tasks across multiple agents with intelligent load balancing and fault tolerance.

**Syntax**:
```bash
./claude-flow-swarm "task description" [options]
```

**Options**:
- `--strategy <type>` - Execution strategy (auto, development, research, analysis, testing, optimization, maintenance)
- `--mode <type>` - Coordination mode (centralized, distributed, hierarchical, mesh, hybrid)
- `--max-agents <n>` - Maximum concurrent agents (default: 5)
- `--timeout <minutes>` - Timeout in minutes (default: 60)
- `--background` - Run in background for long tasks
- `--monitor` - Enable real-time monitoring
- `--ui` - Launch terminal UI interface
- `--parallel` - Enable parallel execution
- `--distributed` - Enable distributed coordination
- `--review` - Enable peer review process
- `--testing` - Include automated testing
- `--encryption` - Enable data encryption
- `--verbose` - Detailed logging
- `--dry-run` - Preview without executing

**Strategies**:
- **auto** - Automatic strategy selection based on task analysis
- **development** - Code implementation with review and testing
- **research** - Information gathering and synthesis
- **analysis** - Data processing and pattern identification
- **testing** - Comprehensive quality assurance
- **optimization** - Performance tuning and refactoring
- **maintenance** - System updates and bug fixes

**Examples**:
```bash
# Development swarm with monitoring
./claude-flow-swarm "Build e-commerce REST API" \
  --strategy development \
  --monitor \
  --review \
  --testing

# Background optimization swarm
./claude-flow-swarm "Optimize system performance" \
  --strategy optimization \
  --background

# Distributed research swarm
./claude-flow-swarm "Analyze AI market trends" \
  --strategy research \
  --distributed \
  --ui \
  --max-agents 8
```

### `/sparc` - SPARC Methodology Workflows

**Purpose**: Execute SPARC methodology workflows with proper phase delegation.

**Syntax**:
```bash
./sparc [phase] [task]
```

**Phases**:
- `spec` - Specification phase (requirements analysis)
- `architect` - Architecture phase (system design)
- `code` - Code implementation phase
- `test` - Testing phase (TDD)
- `review` - Review phase (quality validation)
- `docs` - Documentation phase
- `deploy` - Deployment phase
- `integrate` - Integration phase

**Examples**:
```bash
# Run full SPARC workflow
./sparc "Build complete authentication system"

# Run specific phase
./sparc architect "Design API structure"
./sparc test "Implement user authentication"
./sparc code "Build REST API endpoints"

# With options
./sparc code "Build microservices" \
  --non-interactive \
  --enable-permissions
```

### `/tcps-pull` - Just-In-Time Work Order Creation

**Purpose**: Create work orders from demand signals (marketplace, GitHub, security advisories).

**Syntax**:
```bash
/tcps-pull [signal-source] [signal-id]
```

**Signal Sources**:
- `marketplace-install` - Customer installation demand
- `marketplace-refund` - Quality issue signal
- `github-issue` - Bug/feature request
- `github-pull-request` - Code review request
- `security-advisory` - CVE notification
- `performance-alert` - Monitoring system alert

**Examples**:
```bash
# From marketplace
/tcps-pull marketplace-install product-42

# From GitHub issue
/tcps-pull github-issue 123

# From security advisory
/tcps-pull security-advisory CVE-2026-1234

# From performance alert
/tcps-pull performance-alert "memory-usage-exceeded"
```

### `/tcps-heijunka` - Production Leveling

**Purpose**: Balance production workload across different work types.

**Syntax**:
```bash
/tcps-heijunka [schedule-period]
```

**Schedule Periods**:
- `daily` - Daily production leveling
- `weekly` - Weekly production leveling
- `sprint` - Sprint-based leveling
- `custom` - Custom leveling configuration

**Work Distribution**:
- 40% Reliability (bug fixes, performance)
- 30% Security (patches, audits)
- 20% Cost Reduction (optimize resources)
- 10% New Features (marketplace demands)

**Examples**:
```bash
# Weekly production leveling
/tcps-heijunka weekly

# Sprint-based leveling
/tcps-heijunka sprint

# Custom leveling with specific ratios
/tcps-heijunka custom --reliability 50 --security 25
```

## Task Management Commands

### `/tcps-kanban` - Work-in-Progress Management

**Purpose**: Manage work-in-progress limits and visualize workflow.

**Syntax**:
```bash
/tcps-kanban [action] [work-order-id]
```

**Actions**:
- `check` - Check current WIP limits
- `move` - Move work order between buckets
- `limits` - Display current WIP limits
- `stats` - Show Kanban statistics

**Buckets**:
- `backlog` - Unplanned work
- `ready` - Ready to start
- `in_progress` - Currently being worked on
- `review` - Code review in progress
- `testing` - Testing phase
- `done` - Completed

**WIP Limits**:
- Design: max 3 concurrent
- Code: max 5 concurrent
- Test: max 7 concurrent

**Examples**:
```bash
# Check current WIP status
/tcps-kanban check

# Move work order
/tcps-kanban move WO-123 to-testing

# Show detailed stats
/tcps-kanban stats --verbose

# Custom WIP limits
/tcps-kanban limits --code 8 --testing 10
```

### `/tcps-build` - Standard Work Execution

**Purpose**: Execute build and compile following standard work procedures.

**Syntax**:
```bash
/tcps-build [work-order-id] [options]
```

**Options**:
- `--no-cache` - Clean build, no cached results
- `--dry-run` - Preview build steps without executing
- `--verbose` - Detailed build output
- `--parallel` - Enable parallel compilation

**Standard Work Steps**:
1. Checkout work order branch
2. Run rebar3 compile
3. Run format check
4. Run xref + dialyzer
5. Generate build receipt

**Examples**:
```bash
# Build work order
/tcps-build WO-123

# Clean build
/tcps-build WO-123 --no-cache

# Preview build steps
/tcps-build WO-123 --dry-run

# Parallel build
/tcps-build WO-123 --parallel --verbose
```

### `/tcps-jidoka` - Built-in Quality Checks

**Purpose**: Run quality checks with stop-the-line authority.

**Syntax**:
```bash
/tcps-jidoka [work-order-id] [options]
```

**Stop-the-Line Triggers**:
- SHACL validation failure
- Compilation error
- Test failure (unit, integration, property)
- Coverage drop below 80%
- Security scan failure (Bandit/Dialyzer)
- Missing receipt in chain
- Non-deterministic build output
- Performance regression

**Options**:
- `--full` - Run complete quality suite
- `--fast` - Quick quality checks only
- `--specific <tests>` - Run specific test suites
- `--coverage` - Force coverage check

**Examples**:
```bash
# Run quality checks
/tcps-jidoka WO-123

# Full quality suite
/tcps-jidoka WO-123 --full

# Quick check
/tcps-jidoka WO-123 --fast

# Specific tests
/tcps-jidoka WO-123 --specific unit,integration
```

## Error Handling Commands

### `/tcps-andon` - Stop-the-Line Signaling

**Purpose**: Trigger visible problem signaling and automatic quarantine.

**Syntax**:
```bash
/tcps-andon [action] [work-order-id] [reason]
```

**Actions**:
- `trigger` - Activate Andon signal
- `resolve` - Resume after fix
- `status` - Check Andon status
- `history` - View Andon history

**Severity Levels**:
- `critical` - Immediate stop required
- `warning` - Investigate but continue
- `info` - Monitor but no action needed

**Examples**:
```bash
# Trigger Andon
/tcps-andon trigger WO-123 "test-failure"

# Check status
/tcps-andon status WO-123

# Resolve Andon
/tcps-andon resolve WO-123

# View history
/tcps-andon history --limit 10
```

### `/5-whys-analyze` - Root Cause Analysis

**Purpose**: Perform 5 Whys analysis to identify root causes.

**Syntax**:
```bash
/5-whys-analyze [problem-description]
```

**Analysis Flow**:
1. Describe problem
2. Why did it happen? (1st why)
3. Why did that happen? (2nd why)
4. Why did that happen? (3rd why)
5. Why did that happen? (4th why)
6. Why did that happen? (5th why - root cause)
7. Document countermeasures
8. Update standard work

**Examples**:
```bash
# Analyze test failure
/5-whys-analyze "test failure in tcp transport"

# Analyze performance regression
/5-whys-analyze "build time increased 40%"

# Analyze security incident
/5-whys-analyze "unauthorized access to customer data"
```

### `/poka-yoke-validate` - Error-Proofing Validation

**Purpose**: Validate against error-proofing gates.

**Syntax**:
```bash
/poka-yoke-validate [sku]
```

**Validation Gates**:
1. Schema validation (required fields present)
2. Envelope consistency (bounds realistic)
3. Refusal codes exist (1001-1089)
4. Evidence requirements met
5. Price monotonicity (Team ≤ Enterprise ≤ Gov)

**Examples**:
```bash
# Validate SKU
/poka-yoke-validate dist/v1.4.0/team

# Validate Enterprise tier
/poka-yoke-validate dist/v1.4.0/enterprise

# Strict validation
/poka-yoke-validate dist/v1.4.0/gov --strict
```

## Work Order Management Commands

### `/work-order-create` - Create Work Order

**Purpose**: Create work order from demand signal.

**Syntax**:
```bash
/work-order-create [signal-source] [signal-id]
```

**Examples**:
```bash
# From marketplace
/work-order-create marketplace install-42

# From GitHub issue
/work-order-create github issue-123

# From security advisory
/work-order-create security CVE-2026-1234
```

### `/work-order-assign` - Assign Work Order

**Purpose**: Assign work order to specific agent.

**Syntax**:
```bash
/work-order-assign [work-order-id] [agent-id]
```

**Auto-Assignment Rules**:
- `.erl` → `erlang-otp-developer`
- `_tests.erl` → `erlang-test-engineer`
- `.md` → `erlang-researcher`
- `Makefile`, `rebar.config` → `erlang-architect`

**Examples**:
```bash
# Manual assignment
/work-order-assign WO-123 erlang-otp-developer

# Auto-assignment
/work-order-assign WO-123 auto

# Assignment by skill
/work-order-assign WO-123 performance-optimization
```

### `/work-order-status` - Check Work Order Status

**Purpose**: Show work order status and WIP limits.

**Syntax**:
```bash
/work-order-status [work-order-id]
```

**Examples**:
```bash
# Show all work orders
/work-order-status

# Show specific work order
/work-order-status WO-123

# Detailed view
/work-order-status WO-123 --verbose
```

### `/work-order-schedule` - Schedule Work Orders

**Purpose**: Schedule work orders via heijunka (production leveling).

**Syntax**:
```bash
/work-order-schedule [schedule-batch]
```

**Examples**:
```bash
# Schedule next batch
/work-order-schedule next-batch

# Weekly schedule
/work-order-schedule weekly

# Custom schedule
/work-order-schedule custom --priority critical,high
```

## Monitoring and Debugging Commands

### `/claude-flow monitor` - Real-Time Monitoring

**Purpose**: Monitor swarm activity and system health.

**Syntax**:
```bash
/claude-flow monitor [options]
```

**Options**:
- `--focus <component>` - Focus on specific component
- `--interval <seconds>` - Update interval (default: 5)
- `--output <format>` - Output format (json, table, html)
- `--filter <criteria>` - Filter by criteria

**Examples**:
```bash
# Monitor all components
/claude-flow monitor

# Monitor specific component
/claude-flow monitor --focus swarm

# JSON output
/claude-flow monitor --output json --interval 10

# Filter active agents
/claude-flow monitor --filter "status=active"
```

### `/claude-flow agent` - Agent Management

**Purpose**: List and inspect agents.

**Syntax**:
```bash
/claude-flow agent [action] [options]
```

**Actions**:
- `list` - List all agents
- `info` - Get agent information
- `stats` - Agent statistics
- `health` - Health check

**Examples**:
```bash
# List all agents
/claude-flow agent list

# Agent information
/claude-flow agent info erlang-otp-developer

# Statistics
/claude-flow agent stats --agent erlang-otp-developer

# Health check
/claude-flow agent health --all
```

### `/claude-flow memory` - Memory Management

**Purpose**: Manage swarm memory and persistence.

**Syntax**:
```bash
/claude-flow memory [action] [options]
```

**Actions**:
- `store` - Store value in memory
- `query` - Query memory
- `export` - Export memory to file
- `import` - Import memory from file
- `cleanup` - Clean up expired data

**Options**:
- `--namespace <name>` - Memory namespace
- `--key <key>` - Memory key
- `--value <value>` - Memory value
- `--limit <n>` - Result limit
- `--ttl <seconds>` - Time to live

**Examples**:
```bash
# Store value
/claude-flow memory store "project_goal" "Build scalable API" --namespace swarm

# Query memory
/claude-flow memory query "project_goal" --namespace swarm

# Export memory
/claude-flow memory export swarm-backup.json --namespace swarm

# Cleanup expired
/claude-flow memory cleanup --ttl 86400 --namespace swarm
```

## Advanced Commands

### `/tcps-receipt` - Receipt Chain Generation

**Purpose**: Generate immutable audit trail receipts.

**Syntax**:
```bash
/tcps-receipt [work-order-id] [options]
```

**Options**:
- `--chain` - Show full receipt chain
- `--verify` - Verify receipt integrity
- `--export` - Export receipt to file
- `--hash <hash>` - Find receipt by hash

**Receipt Contents**:
- SHA-256 hash of previous receipt
- Timestamp (ISO 8601)
- Work order ID
- Build outputs (deterministic)
- Test results (bench, chaos, conformance, refusal)
- Evidence bundle path

**Examples**:
```bash
# Generate receipt
/tcps-receipt WO-123

# Show chain
/tcps-receipt WO-123 --chain

# Verify integrity
/tcps-receipt WO-123 --verify

# Export receipt
/tcps-receipt WO-123 --export receipt.json
```

### `/poka-yoke-test` - Conformance Testing

**Purpose**: Run conformance test suites.

**Syntax**:
```bash
/poka-yoke-test [suite] [options]
```

**Suites**:
- `bench` - Benchmark suite (9 environments × 9 workloads)
- `chaos` - Chaos suite (failure injection)
- `conformance` - Conformance suite (JSON-RPC, MCP protocol)
- `refusal` - Refusal suite (86 refusal codes)

**Options**:
- `--parallel` - Run tests in parallel
- `--verbose` - Detailed output
- `--timeout <seconds>` - Test timeout
- `--retries <n>` - Number of retries

**Examples**:
```bash
# Run benchmark suite
/poka-yoke-test bench

# Run chaos tests
/poka-yoke-test chaos --parallel --verbose

# Run specific suite
/poka-yoke-test conformance --timeout 300

# Comprehensive testing
/poka-yoke-test all --parallel --verbose
```

### `/tcps-kaizen` - Continuous Improvement

**Purpose**: Document and track improvements.

**Syntax**:
```bash
/tcps-kaizen [type] [description] [options]
```

**Types**:
- `process` - Process improvement (reduce waste)
- `tool` - Tool improvement (better automation)
- `quality` - Quality improvement (fewer defects)
- `documentation` - Documentation improvement

**Options**:
- `--roi <percentage>` - Estimated ROI percentage
- `--timeline <weeks>` - Implementation timeline
- `--owner <name>` - Improvement owner

**Examples**:
```bash
# Process improvement
/tcps-kaizen process "reduce build time 20%" --roi 15

# Tool improvement
/tcps-kaizen tool "automate code review" --owner dev-team

# Quality improvement
/tcps-kaizen quality "reduce test failures by 50%" --timeline 4
```

## Configuration Files

### `.claude/settings.json`

```json
{
  "hooks": {
    "post-task": {
      "enabled": true,
      "command": "make check",
      "description": "Run tests, dialyzer, xref before completion"
    }
  },
  "quality-gates": {
    "test-pass-rate": 1.0,
    "coverage-threshold": 0.8,
    "dialyzer-warnings": 0,
    "xref-issues": 0
  },
  "swarm": {
    "max-agents": 5,
    "timeout": 60,
    "strategy": "auto",
    "mode": "centralized"
  }
}
```

### `rebar.config` (Test Profiles)

```erl
{cover_enabled, true},
{cover_opts, [verbose]},
{cover_export_dir, "logs/coverage"},
{eunit_opts, [verbose]},
{ct_opts, [logdir, "logs/ct"]},
{proper_opts, [long_result, noshrink]},
{xref_warnings, true},
{dialyzer_opts, [warnings_as_errors, {error_handling, warnings}]}.
```

This comprehensive CLI reference provides all the commands needed for sophisticated task orchestration and workflow management in erlmcp.