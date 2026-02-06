# erlmcp Development Documentation Index

**Last Updated**: 2026-02-06 | **Status**: Complete | **Scope**: All Development Documentation

Complete reference guide for erlmcp development. All guides emphasize Docker-only execution per the DOCKER-ONLY CONSTITUTION.

---

## Quick Links by Role

### For New Contributors

Start here to get up and running quickly:

1. **[Docker Local Development](./DOCKER_LOCAL_DEVELOPMENT.md)** - Set up your development environment
   - Docker Compose architecture
   - Running your first tests
   - Common development tasks
   - Troubleshooting

2. **[Testing Guide](./TESTING_GUIDE.md)** - Understanding erlmcp's testing strategy
   - EUnit unit tests
   - Common Test integration tests
   - PropEr property-based tests
   - Coverage requirements

3. **[OTP Design Patterns](./OTP_DESIGN_PATTERNS.md)** - Learning OTP in erlmcp
   - gen_server patterns
   - Supervisor patterns
   - Error handling
   - Message passing

### For Implementing Features

When building new features:

1. **[OTP Design Patterns](./OTP_DESIGN_PATTERNS.md)** - How to design with OTP
   - gen_server pattern
   - Behavior-based design
   - State management
   - Best practices and anti-patterns

2. **[Supervision Tree Design](./SUPERVISION_TREE_DESIGN.md)** - Building fault-tolerant systems
   - Supervision hierarchy
   - Restart strategies
   - Per-connection processes
   - Testing supervision

3. **[Testing Guide](./TESTING_GUIDE.md)** - Writing comprehensive tests
   - Unit tests (EUnit)
   - Integration tests (CT)
   - Property-based tests (PropEr)

### For Adding Transports

When implementing new protocols:

1. **[Adding Transports](./ADDING_TRANSPORTS.md)** - Complete transport implementation guide
   - Transport behavior interface
   - Step-by-step implementation
   - HTTP/2 example
   - Testing your transport
   - Integration with erlmcp

2. **[OTP Design Patterns](./OTP_DESIGN_PATTERNS.md)** - Understanding gen_server
   - Implement transport behavior
   - Handle messages correctly

### For Debugging Issues

When troubleshooting:

1. **[Debugging Guide](./DEBUGGING_GUIDE.md)** - Comprehensive debugging techniques
   - Interactive debugging in Docker
   - Logging and tracing
   - Process inspection
   - Common issues and solutions
   - Performance profiling

2. **[Docker Local Development](./DOCKER_LOCAL_DEVELOPMENT.md)** - Container troubleshooting
   - Common Docker issues
   - Checking logs
   - Verifying setup

---

## Complete Guide Map

### Development Setup and Tools

```
Docker Local Development (DOCKER_LOCAL_DEVELOPMENT.md)
├── Quick Start
├── Docker Compose Architecture
├── Development Workflow
│   ├── Typical development cycle
│   ├── Multi-stage workflow
│   └── Volume mapping
├── Common Development Tasks
│   ├── Building and compilation
│   ├── Running tests
│   ├── Code quality
│   ├── Interactive development
│   └── Debugging
├── Stopping Containers
├── Troubleshooting
└── Environment Variables
```

### Testing Strategies

```
Testing Guide (TESTING_GUIDE.md)
├── Testing Architecture
│   ├── Test pyramid
│   └── Execution pipeline
├── Unit Tests with EUnit
│   ├── What is EUnit?
│   ├── Test file structure
│   ├── Assertions
│   ├── Running in Docker
│   └── Best practices
├── Integration Tests with CT
│   ├── What is Common Test?
│   ├── Test suite structure
│   ├── Real processes (no mocks)
│   ├── Running in Docker
│   └── Best practices
├── Property-Based Tests with PropEr
│   ├── What is PropEr?
│   ├── Property test structure
│   ├── Generators
│   ├── Running in Docker
│   └── Common patterns
├── Coverage Requirements
│   ├── Thresholds (≥80%)
│   ├── Running coverage
│   └── Coverage workflow
├── Test Organization
│   ├── Directory structure
│   ├── Naming conventions
│   ├── Grouping tests
│   └── Running grouped tests
└── Troubleshooting Tests
```

### OTP and Design Patterns

```
OTP Design Patterns (OTP_DESIGN_PATTERNS.md)
├── Erlang Philosophy
│   ├── Let it crash principle
│   └── Key principles
├── gen_server Pattern
│   ├── Lifecycle
│   ├── Complete implementation
│   └── Best practices
├── Supervisor Pattern
│   ├── Hierarchy
│   └── Implementation
├── Application Pattern
│   ├── Lifecycle
│   └── Configuration
├── Behavior-Based Design
│   ├── Implementing custom behaviors
│   └── Transport example
├── Error Handling and Recovery
│   └── Defensive code structure
├── Message Passing Patterns
│   ├── Request/Response
│   └── Publish/Subscribe
├── State Management
│   ├── Immutable state pattern
│   └── State versioning
└── Anti-Patterns
```

### Supervision Tree Architecture

```
Supervision Tree Design (SUPERVISION_TREE_DESIGN.md)
├── Supervision Philosophy
│   ├── Process isolation
│   └── Design patterns
├── erlmcp v3 Supervision Hierarchy
│   ├── Complete supervision tree (4 tiers)
│   └── Tier responsibilities
├── Designing Your Tree
│   ├── Decision tree for strategy choice
│   └── Core supervisor implementation
├── Restart Strategies
│   ├── one_for_one
│   ├── one_for_all
│   ├── rest_for_one
│   ├── simple_one_for_one
│   └── Restart policies
├── Per-Connection Processes
│   ├── Accept incoming connection pattern
│   └── Per-connection server pattern
├── Cluster-Safe Supervision
│   └── Multi-node supervision
├── Testing Supervision Trees
│   ├── Unit tests
│   └── Integration tests
└── Best Practices
```

### Transport Implementation

```
Adding Transports (ADDING_TRANSPORTS.md)
├── Transport Architecture
│   ├── Transport layer design
│   └── Message flow
├── Transport Behavior Interface
│   ├── init/1 callback
│   ├── send/2 callback
│   ├── close/1 callback
│   └── Optional callbacks
├── Implementation Steps
│   ├── Step 1: Create transport module
│   ├── Step 2: Implement gen_server wrapper
│   ├── Step 3: Add to app file
│   └── Step 4: Integration
├── Example: HTTP/2 Transport
│   └── Complete working example
├── Testing Your Transport
│   ├── Unit tests
│   ├── Integration tests
│   └── Running tests in Docker
├── Registering Your Transport
│   ├── Update transport registry
│   └── Update supervisor
├── Integration with Transports App
│   ├── Compile and test
│   └── Verification
└── Implementation Checklist
```

### Debugging and Troubleshooting

```
Debugging Guide (DEBUGGING_GUIDE.md)
├── Debugging Philosophy
│   ├── Erlang principles
│   └── Error categories
├── Interactive Debugging in Docker
│   ├── Starting environment
│   ├── Erlang shell commands
│   ├── Debugging gen_server
│   └── Interactive workflow
├── Logging and Tracing
│   ├── Erlang logger configuration
│   ├── Setting log level
│   ├── Runtime tracing
│   ├── Message tracing
│   └── Comprehensive example
├── Process Inspection
│   ├── Observer GUI
│   ├── Command-line inspection
│   ├── recon tool
│   └── Finding specific processes
├── Common Issues and Solutions
│   ├── Silent crashes
│   ├── Deadlocks
│   ├── Memory leaks
│   └── High CPU usage
├── Visual Debugging Tools
│   ├── Observer (GUI debugger)
│   ├── System monitor
│   └── Trace tool (ttb)
├── Performance Profiling
│   ├── CPU profiling
│   ├── Memory profiling
│   └── Benchmarking
└── Debugging Checklist
```

---

## Development Workflow at a Glance

### Day-to-Day Development

```bash
# 1. Setup (first time only)
docker compose build erlmcp-build

# 2. Start feature work
git checkout -b feature/my-feature

# 3. Write test first (TDD)
docker compose run --rm erlmcp-unit make eunit

# 4. Implement feature
# (edit code)

# 5. Run tests
docker compose run --rm erlmcp-unit make eunit

# 6. Run integration tests
docker compose run --rm erlmcp-ct make ct

# 7. Quality gates
docker compose run --rm erlmcp-check make check

# 8. Commit
git add -A
git commit -m "feat: implement feature"
git push
```

### Quality Gate Flow

```
Compile (erlmcp-build)
    ↓
Unit Tests (erlmcp-unit) - ≥80% coverage
    ↓
Integration Tests (erlmcp-ct) - Real processes
    ↓
Property Tests (erlmcp-ct) - PropEr
    ↓
Quality Analysis (erlmcp-check) - Dialyzer, Xref
    ↓
All Tests Pass ✓
```

---

## Docker Service Mapping

| Service | Gate | Command | Purpose |
|---------|------|---------|---------|
| `erlmcp-build` | compile | `make compile` | Build and compile |
| `erlmcp-unit` | eunit | `make eunit` | Unit tests + coverage |
| `erlmcp-ct` | ct | `make ct` | Integration tests |
| `erlmcp-ct` | proper | `rebar3 proper` | Property tests |
| `erlmcp-check` | check | `make check` | Dialyzer + Xref |
| `erlmcp-bench` | benchmark | `make benchmark` | Performance tests |
| `erlmcp-node` | cluster | `make test-cluster` | Cluster tests |
| `erlmcp-dev` | - | `/bin/bash` | Interactive development |

---

## Key Concepts

### DOCKER-ONLY Constitution

All development must run via Docker containers:

```bash
# CORRECT - Use Docker
docker compose run --rm erlmcp-build make compile

# INCORRECT - Direct execution (forbidden)
make compile          # ❌ VIOLATES CONSTITUTION
rebar3 compile        # ❌ VIOLATES CONSTITUTION
erlc src/*.erl        # ❌ VIOLATES CONSTITUTION
```

### Chicago School TDD

- No mocks - use real processes
- Black-box testing
- Focus on behavior, not implementation
- Write test first, make it fail, implement

### OTP Best Practices

- **Let it crash**: Embrace failures, design recovery
- **Supervision**: Use supervisors to manage restarts
- **Isolation**: Each process has its own state
- **Explicit dependencies**: No hidden globals
- **Quick init**: Don't block during initialization

---

## Common Tasks

### Writing a Simple Feature

1. Create test file: `apps/erlmcp_core/test/my_feature_tests.erl`
2. Write failing EUnit tests using `?assert`, `?assertEqual`
3. Implement in: `apps/erlmcp_core/src/my_feature.erl`
4. Write gen_server or supervisor as needed
5. Add integration tests in: `test/my_feature_SUITE.erl`
6. Run: `docker compose run --rm erlmcp-unit make eunit`
7. Run: `docker compose run --rm erlmcp-ct make ct`
8. Check coverage: `docker compose run --rm erlmcp-unit bash -c "rebar3 cover"`

### Debugging a Problem

1. Check logs: `docker compose logs erlmcp-build`
2. Enable debug logging: `-e ERLMCP_LOG_LEVEL=debug`
3. Start interactive shell: `docker compose run --rm -it erlmcp-dev /bin/bash`
4. Use Erlang debugger: `rebar3 shell` then trace commands
5. Inspect processes: `erlang:process_info(Pid)`
6. Monitor with Observer: `observer:start()`

### Adding a New Transport

1. Read [Adding Transports](./ADDING_TRANSPORTS.md)
2. Create: `apps/erlmcp_transports/src/erlmcp_transport_newproto.erl`
3. Implement `erlmcp_transport` behavior
4. Write tests in: `apps/erlmcp_transports/test/` and `test/`
5. Register in: `erlmcp_transports.erl` and supervisor
6. Verify: `docker compose run --rm erlmcp-ct make ct`

---

## Documentation Structure

```
docs/development/
├── INDEX.md                           # This file
├── DOCKER_LOCAL_DEVELOPMENT.md        # Docker-based development
├── TESTING_GUIDE.md                   # Testing (EUnit, CT, PropEr)
├── DEBUGGING_GUIDE.md                 # Debugging techniques
├── OTP_DESIGN_PATTERNS.md            # OTP patterns and best practices
├── ADDING_TRANSPORTS.md               # New transport implementation
├── SUPERVISION_TREE_DESIGN.md         # Supervision architecture
├── README.md                          # Development overview (existing)
├── coding-standards.md                # Code style (existing)
└── tooling-automation.md              # Tooling guide (existing)
```

---

## Learning Path Recommendations

### Level 1: Absolute Beginner

**Goal**: Get erlmcp running locally, write first test

1. Read: [Docker Local Development - Quick Start](./DOCKER_LOCAL_DEVELOPMENT.md#quick-start)
2. Do: Run `docker compose run --rm erlmcp-unit make eunit`
3. Read: [Testing Guide - Unit Tests](./TESTING_GUIDE.md#unit-tests-with-eunit)
4. Do: Write and run your first EUnit test
5. Reference: Existing test files for examples

### Level 2: Feature Contributor

**Goal**: Implement a new feature following conventions

1. Read: [OTP Design Patterns - gen_server](./OTP_DESIGN_PATTERNS.md#gen_server-pattern)
2. Read: [Testing Guide - Test Organization](./TESTING_GUIDE.md#test-organization)
3. Do: Implement a feature using TDD
   - Write failing test
   - Implement feature
   - Run all tests
   - Check quality gates
4. Reference: [DOCKER_LOCAL_DEVELOPMENT.md](./DOCKER_LOCAL_DEVELOPMENT.md#common-development-tasks)

### Level 3: Architecture Designer

**Goal**: Design supervision trees, add transports

1. Read: [Supervision Tree Design](./SUPERVISION_TREE_DESIGN.md)
2. Read: [OTP Design Patterns - Supervisor](./OTP_DESIGN_PATTERNS.md#supervisor-pattern)
3. Read: [Adding Transports](./ADDING_TRANSPORTS.md)
4. Do: Design and implement supervision tree or transport
5. Reference: Existing architecture in [erlmcp v3 Supervision Hierarchy](./SUPERVISION_TREE_DESIGN.md#erlmcp-v3-supervision-hierarchy)

### Level 4: Performance Optimizer

**Goal**: Profile, optimize, and scale

1. Read: [Debugging Guide - Performance Profiling](./DEBUGGING_GUIDE.md#performance-profiling)
2. Read: [Testing Guide - Property-Based Tests](./TESTING_GUIDE.md#property-based-tests-with-proper)
3. Do: Profile critical paths, identify bottlenecks
4. Reference: Benchmark results and thresholds

---

## Getting Help

### Documentation Search

- **Docker issues**: [DOCKER_LOCAL_DEVELOPMENT.md - Troubleshooting](./DOCKER_LOCAL_DEVELOPMENT.md#troubleshooting)
- **Test failures**: [TESTING_GUIDE.md - Troubleshooting](./TESTING_GUIDE.md#troubleshooting-tests)
- **Process hangs**: [DEBUGGING_GUIDE.md - Deadlock](./DEBUGGING_GUIDE.md#issue-deadlock-process-hangs)
- **Memory leaks**: [DEBUGGING_GUIDE.md - Memory Leak](./DEBUGGING_GUIDE.md#issue-memory-leak-process-grows)
- **OTP patterns**: [OTP_DESIGN_PATTERNS.md - gen_server](./OTP_DESIGN_PATTERNS.md#gen_server-pattern)

### Common Commands

```bash
# View all available documentation
ls -la docs/development/

# Search for topic
grep -r "topic" docs/development/

# Read specific guide
less docs/development/TESTING_GUIDE.md

# Interactive help in container
docker compose run --rm -it erlmcp-dev /bin/bash
cd /workspace
rebar3 help compile
```

---

## Version Information

- **erlmcp Version**: 3.0.0
- **OTP Minimum**: 27.0 (Recommended: 28+)
- **Rebar3**: 3.22+
- **Docker**: 20.10+
- **Docker Compose**: 2.0+
- **Last Updated**: 2026-02-06

---

## Contributing to Documentation

If you find issues or want to improve documentation:

1. Create a branch: `git checkout -b docs/improvement`
2. Edit the appropriate markdown file
3. Test Docker commands mentioned
4. Commit: `git commit -m "docs: improve X guide"`
5. Push and create PR

---

## Maintainers

- **erlmcp development team**
- **Last Updated**: 2026-02-06
- **Status**: Active and Complete

---

**Remember**: All execution runs via Docker. The DOCKER-ONLY CONSTITUTION is not optional.
