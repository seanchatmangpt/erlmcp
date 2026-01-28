# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**erlmcp** is an Erlang/OTP implementation of the Model Context Protocol (MCP) SDK. It provides both client and server implementations that enable AI assistants to communicate with local services through a standardized protocol.

- **Language**: Erlang/OTP 25+
- **Build System**: rebar3
- **Dependencies**: jsx (JSON), jesse (JSON Schema validation)
- **Testing**: EUnit, Common Test (CT), Proper (property-based testing)

## Build & Development Commands

### Common Development Tasks

```bash
# Compile the project
make compile
rebar3 compile

# Run all tests (unit, integration, property-based)
make test
rebar3 do eunit, ct, proper -c

# Run specific test suites
make test-unit              # EUnit only
make test-integration       # Common Test only
make test-property          # Property-based tests only
make test-local             # Local examples (simple, calculator, weather)

# Code quality
make lint                   # Run xref + dialyzer
make xref                   # Cross-reference analysis
make dialyzer               # Type analysis
make format                 # Format code with rebar3_format
make check                  # Full check: compile, xref, dialyzer, tests

# Development & debugging
make console                # Start Erlang shell with app loaded
make dev-console            # Start with sys.config and vm.args
make observer                # Start observer for live monitoring
make profile                # Trace and profile erlmcp_client calls

# Code coverage
make coverage-report        # Generate HTML coverage report

# Release & deployment
make release                # Build production release
make docker-build           # Build Docker image
make docker-run             # Run Docker container

# Cleanup
make clean                  # Clean build artifacts
make distclean              # Deep clean (including _build)

# Examples (interactive testing)
make test-server            # Run simple_server example
make test-client            # Run simple_client example
make test-advanced-client   # Run advanced client example
```

### Single Test Execution

```bash
# Run a specific test file
rebar3 eunit --module=erlmcp_server_tests

# Run tests from testlocal profile (includes examples)
rebar3 as testlocal eunit -v

# Run specific property test
rebar3 proper -c --module=erlmcp_server_tests
```

## Directory Structure

### Core Source Code (`src/`)

**Protocol & JSON-RPC:**
- `erlmcp_json_rpc.erl` - JSON-RPC 2.0 message encoding/decoding
- `erlmcp.hrl` - Protocol type definitions and records

**Client Implementation:**
- `erlmcp_client.erl` - Main client gen_server with request-response correlation
- `erlmcp_client_sup.erl` - Supervisor for managing multiple client connections

**Server Implementation:**
- `erlmcp_server.erl` - Main server gen_server (primary implementation)
- `erlmcp_server_new.erl` - Alternative server implementation (in development)
- `erlmcp_server_sup.erl` - Supervisor for managing server instances

**Transport Layer:**
- `erlmcp_transport.erl` - Transport behavior interface
- `erlmcp_transport_stdio.erl` - Standard I/O transport
- `erlmcp_transport_stdio_new.erl` - New stdio implementation
- `erlmcp_transport_tcp.erl` - TCP socket transport
- `erlmcp_transport_http.erl` - HTTP transport
- `erlmcp_transport_sup.erl` - Transport supervisor

**Registry & Infrastructure:**
- `erlmcp_registry.erl` - Central registry for message routing
- `erlmcp_stdio_server.erl` - Stdio-specific server (legacy)
- `erlmcp_stdio.erl` - Stdio wrapper (legacy)
- `erlmcp_app.erl` - Application callback
- `erlmcp_sup.erl` - Main application supervisor

### Headers (`include/`)
- Protocol definitions, type specs, record definitions

### Tests (`test/`)
- `erlmcp_server_tests.erl` - Server functionality tests
- `erlmcp_client_advanced_tests.erl` - Advanced client tests
- `erlmcp_registry_tests.erl` - Registry tests
- `erlmcp_json_rpc_tests.erl` - JSON-RPC encoding/decoding tests
- `erlmcp_advanced_tests.erl` - Integration and advanced scenarios

### Examples (`examples/`)
- **simple/** - Basic client/server examples
- **calculator/** - Calculator service client
- **weather/** - Weather service with full MCP protocol

### Documentation (`docs/`)
- `architecture.md` - System design and component overview
- `otp-patterns.md` - OTP best practices used throughout codebase
- `protocol.md` - MCP protocol implementation details
- `api-reference.md` - Complete API documentation
- `*-redesign*.md` - Phase implementation plans and architecture decisions

### Configuration
- `rebar.config` - Build configuration (profiles, deps, tools, formatting)
- `config/sys.config` - Runtime system configuration
- `vm.args` - Erlang VM arguments

## Architecture & Design Patterns

### Supervision Tree Structure

```
erlmcp_sup (one_for_all - application supervisor)
├── erlmcp_registry (gen_server)
│   └── Central registry for all message routing
├── erlmcp_client_sup (simple_one_for_one)
│   └── erlmcp_client (dynamic workers)
└── erlmcp_server_sup (simple_one_for_one)
    └── erlmcp_server (dynamic workers)
```

### Key Design Principles

1. **OTP Compliance**: All components follow gen_server and supervisor behaviors
2. **Process Isolation**: Each connection runs in its own process for fault tolerance
3. **Supervision Strategy**: `one_for_all` for application, `simple_one_for_one` for dynamic children
4. **Message Correlation**: Request IDs track pending client requests across async responses
5. **Registry-Based Routing**: Central registry handles message delivery between clients/servers
6. **Error Handling**: Let-it-crash philosophy with supervisor recovery; graceful degradation for I/O

### Request-Response Pattern

The client implements sophisticated request tracking:
- Each request gets a unique ID (`request_id` counter)
- Client stores pending requests in `#state.pending` map: `{RequestId => {CallerPid, CallerRef}}`
- Async response received from transport triggers pending request lookup
- Caller is notified with `gen_server:reply(CallerPid, Result)`

### Resource & Tool Management

**Server State Records:**
- `resources :: map()` - Maps URI → handler function
- `tools :: map()` - Maps tool name → {handler, JSON schema}
- `prompts :: map()` - Maps prompt name → prompt definition
- `subscriptions :: map()` - Maps URI → set of subscriber pids

Handlers are user-provided functions: `fun(Uri) -> Content` or `fun(Args) -> Content`

### Transport Abstraction

All transports implement common behavior with standardized messages:
- `{transport_data, Binary}` - Incoming data from network
- `{transport_connected, Info}` - Connection established
- `{transport_disconnected, Reason}` - Connection lost
- `{transport_error, Type, Reason}` - Transport-level error

### State Management Pattern

Both client and server use record-based state:
```erlang
-record(state, {
    transport :: module(),        % Transport module (stdlib, tcp, http)
    transport_state :: term(),    % State managed by transport
    capabilities :: map(),        % MCP capabilities
    request_id = 1 :: integer(),  % For request correlation
    pending = #{} :: map()        % Pending requests for client
}).
```

## Development Workflow

### Before Making Changes

1. **Understand the architecture** - Read `docs/architecture.md` and `docs/otp-patterns.md`
2. **Check existing patterns** - Look for similar functionality in `src/erlmcp_*.erl`
3. **Review test structure** - Check how functionality is tested in `test/`

### Adding New Features

1. **Create corresponding tests first** - Tests live in `test/` with naming convention `*_tests.erl`
2. **Follow existing gen_server patterns** - Match structure of `erlmcp_client.erl` or `erlmcp_server.erl`
3. **Implement transport behavior if needed** - Match interface in `erlmcp_transport.erl`
4. **Update supervision tree** - Register new workers in appropriate supervisor
5. **Run full test suite** - `make check` verifies code quality and tests pass

### Code Style & Quality

- **Formatting**: `make format` enforces rebar3_format configuration (100-char paper width, 90-char ribbon)
- **Type Checking**: Dialyzer warnings are treated as errors in production profile
- **Cross-Reference**: xref ensures all calls are valid (whitelist dynamic calls in `rebar.config`)
- **Test Coverage**: Coverage reports available via `make coverage-report`

### Common Pitfalls (from OTP Patterns)

1. **Don't block in `init/1`** - Do async initialization via cast
2. **Avoid large messages** - Use references to shared data instead
3. **Monitor critical processes** - Clean up when dependencies die
4. **Set proper timeouts** - Prevent hanging calls (5000ms default for clients)
5. **Use supervisors** - Never spawn unsupervised processes

## Transport Layer Design

### Adding a New Transport

1. Create `erlmcp_transport_TYPE.erl` with `-behaviour(erlmcp_transport)`
2. Implement required callbacks: `init/2`, `send/2`, `close/1`
3. Send standardized messages to registry (see Transport Abstraction above)
4. Add supervisor entry in `erlmcp_transport_sup.erl`
5. Update `rebar.config` xref_ignores if using dynamic calls

### Current Transport Status

- **stdio** (`erlmcp_transport_stdio.erl`, `erlmcp_transport_stdio_new.erl`) - Read/write standard I/O
- **tcp** (`erlmcp_transport_tcp.erl`) - Socket-based communication
- **http** (`erlmcp_transport_http.erl`) - HTTP/1.1 client and server

## Testing Strategy

### Test Profiles in rebar.config

- **test** - Full test suite, generates coverage, excludes examples
- **testlocal** - Includes examples (simple, calculator, weather) for integration testing
- **proper** - Property-based testing configuration

### Running Tests by Category

```bash
# Unit tests (EUnit) - Fast, isolated functionality
rebar3 eunit

# Integration tests (Common Test) - Multi-process scenarios
rebar3 ct

# Property-based tests (Proper) - Generated test cases
rebar3 proper -c

# All tests with coverage
rebar3 do eunit, ct, proper -c, cover
```

### Writing Tests

- Use EUnit for unit tests (`test/*.erl` with `-include_lib("eunit/include/eunit.hrl")`)
- Use Common Test for integration scenarios (CT framework for multi-process testing)
- Use Proper for property-based testing (verify invariants across many input combinations)
- See `test/erlmcp_server_tests.erl` for comprehensive server testing examples
- See `test/erlmcp_registry_tests.erl` for registry coordination patterns

## Current Development Focus (v0.6.0)

This project is actively evolving toward a production-ready OTP architecture with library integration:

- **Phase 1** ✅ - Core OTP application structure
- **Phase 2** ✅ - Registry-based routing and server decoupling
- **Phase 3** 🚧 - Library Integration and Transport Standardization
  - ✅ Dependencies added (gproc, gun, ranch, poolboy)
  - 🚧 Registry migration to gproc
  - 🚧 HTTP transport refactor with gun
  - 🚧 TCP transport refactor with ranch
  - 🚧 Connection pooling with poolboy
  - 🚧 Standardizing transport interfaces across stdio/TCP/HTTP
  - See `docs/v0.6.0-FINAL-PLAN.md` for detailed plan

### Library Usage Patterns

When working with transports in v0.6.0:

**gproc Registry:**
```erlang
%% Register (automatic monitoring)
gproc:add_local_name({mcp, server, ServerId}),

%% Lookup
case gproc:lookup_local_name({mcp, server, ServerId}) of
    undefined -> {error, not_found};
    Pid -> {ok, Pid}
end.
```

**gun HTTP Client:**
```erlang
%% Connect
{ok, GunPid} = gun:open(Host, Port, #{protocols => [http2, http]}),

%% Send request
StreamRef = gun:post(GunPid, Path, Headers, Body),

%% Handle response
handle_info({gun_data, GunPid, StreamRef, IsFin, Data}, State).
```

**ranch TCP Handler:**
```erlang
%% Start listener
ranch:start_listener(Name, ranch_tcp, #{port => Port},
                    erlmcp_transport_tcp, [TransportId, Config]).
```

**poolboy Connection Pool:**
```erlang
%% Use pool
poolboy:transaction(pool_name, fun(Worker) ->
    erlmcp_http_worker:request(Worker, Request)
end).
```

## Debugging & Monitoring

### Using Observer
```bash
make observer
# Connects to running Erlang node to visualize process tree and monitor state
```

### Profiling Client Calls
```bash
make profile
# Uses recon_trace to capture erlmcp_client calls
```

### Development Console with Full Config
```bash
make dev-console
# Includes sys.config and vm.args for complete runtime environment
```

### Crash Dumps
- Check `erl_crash.dump` for detailed crash information
- Clean with `make clean`

## Key Files by Task

**Want to understand MCP protocol?** → `docs/protocol.md` + `src/erlmcp_json_rpc.erl`

**Want to add a tool to server?** → `src/erlmcp_server.erl:add_tool*` functions

**Want to call a tool from client?** → `src/erlmcp_client.erl:call_tool`

**Want to add new transport?** → Copy `src/erlmcp_transport_tcp.erl` as template

**Want to understand message routing?** → `src/erlmcp_registry.erl` + `docs/otp-patterns.md`

**Want to debug a test failure?** → Run `rebar3 eunit --module=MODULE --verbose`

## Dependencies

**Core (v0.6.0):**
- `jsx` - JSON encoding/decoding
- `jesse` - JSON Schema validation
- `gproc` 0.9.0 - Process registry and discovery (replaces custom registry)
- `gun` 2.0.1 - HTTP/1.1 and HTTP/2 client (replaces inets)
- `ranch` 2.1.0 - TCP connection handler (replaces custom gen_tcp)
- `poolboy` 1.5.2 - Connection pooling

**Testing (profile-specific):**
- `proper` - Property-based testing
- `meck` - Mocking framework
- `coveralls` - Coverage reporting

**Development (profile-specific):**
- `recon` - Debugging and profiling
- `observer_cli` - Terminal-based observer
- `rebar3_format` - Code formatting
- `rebar3_lint` - Linting plugin

### Library Integration (v0.6.0)

erlmcp v0.6.0 replaces ~770 LOC of custom code with production-grade libraries:

| Component | Before | After | Savings |
|-----------|--------|-------|---------|
| Registry | 411 LOC custom gen_server | 120 LOC with gproc | -291 LOC |
| HTTP Transport | 461 LOC custom inets | 180 LOC with gun | -281 LOC |
| TCP Transport | 349 LOC custom gen_tcp | 150 LOC with ranch | -199 LOC |

**Why these libraries?**

1. **gproc**: Distributed registry with automatic monitoring, eliminates manual process tracking
2. **gun**: Modern HTTP/1.1 + HTTP/2 support, better connection reuse
3. **ranch**: Battle-tested TCP handler (used by EMQX, Cowboy)
4. **poolboy**: Efficient connection pooling and queue management

## Version Information

- **Current Version**: 0.5.0 (see `src/erlmcp.app.src`)
- **Erlang Requirement**: OTP 25+
- **Node.js** (in `.tool-versions`): 22.13.0

## Erlang/OTP-Specific Agents (v1.0.0)

erlmcp has 10 focused agents following Anthropic 2026 best practices (consolidated from 57).

### Quick Agent Selection

**I need to...**
- Implement gen_server/supervisor → **erlang-otp-developer**
- Add transport (stdio/tcp/http) → **erlang-transport-builder**
- Write tests (Chicago School TDD) → **erlang-test-engineer**
- Understand codebase → **erlang-researcher** (haiku, context preservation)
- Design architecture → **erlang-architect**
- Benchmark/optimize → **erlang-performance**
- Create PR/release → **erlang-github-ops**
- Run SPARC workflow → **sparc-orchestrator**
- Plan implementation → **plan-designer** (Research → Plan → Execute)
- Review before merge → **code-reviewer**

### Agent Quality Gates (Mandatory)

ALL agents must pass pre-completion verification:
```bash
✅ Tests: rebar3 do eunit, ct, proper -c (0 failures)
✅ Quality: rebar3 compile && rebar3 dialyzer && rebar3 xref (clean)
✅ Format: rebar3 format --verify
✅ Coverage: ≥80% minimum (85%+ for core modules)
✅ Benchmarks: (if applicable) performance documented
```

Post-task hook in `settings.json` enforces quality gates automatically.

### Related Documentation
- `.claude/AGENT_INDEX.md` - Master agent directory
- `.claude/ERLANG_OTP_AGENT_GUIDE.md` - Erlang-specific workflows
- `.claude/SYSTEM_GUIDE.md` - Commands vs Agents vs Roo rules
- `.claude/agents/` - 10 core agent files

## Command Quick Reference (v1.0.0)

erlmcp has 30 focused commands (consolidated from 91) organized in 6 categories for fast discoverability (<15s).

### Quick Command Finder

| Need to... | Use Command | Invokes Agent | Category |
|------------|-------------|---------------|----------|
| Start SPARC workflow | `/sparc` | `sparc-orchestrator` | SPARC |
| Write specification | `/sparc spec` | `plan-designer` + `erlang-researcher` | SPARC |
| Design architecture | `/sparc architect` | `erlang-architect` | SPARC |
| Implement code | `/sparc code` | `erlang-otp-developer` | SPARC |
| Write tests (TDD) | `/sparc test` | `erlang-test-engineer` | SPARC |
| Review code/security | `/sparc review` | `code-reviewer` | SPARC |
| Generate docs | `/sparc docs` | `erlang-otp-developer` | SPARC |
| Deploy/DevOps | `/sparc deploy` | `erlang-github-ops` | SPARC |
| Integration/MCP | `/sparc integrate` | `erlang-otp-developer` | SPARC |
| Initialize swarm | `/swarm init` | `sparc-orchestrator` | Swarm |
| Spawn agents | `/swarm spawn` | (dynamic) | Swarm |
| Check status | `/swarm status` | (monitoring) | Swarm |
| Orchestrate tasks | `/swarm orchestrate` | `sparc-orchestrator` | Swarm |
| Create/review PR | `/github pr` | `erlang-github-ops` | GitHub |
| Manage issues | `/github issue` | `erlang-github-ops` | GitHub |
| Analyze repository | `/github repo` | `erlang-github-ops` | GitHub |
| Analyze performance | `/perf analyze` | `erlang-performance` | Performance |
| Monitor system | `/perf monitor` | `erlang-performance` | Performance |
| Optimize code | `/perf optimize` | `erlang-performance` | Performance |

### Command Categories (30 Total)

**Top-Level (4)**: `/claude-flow-help`, `/claude-flow-memory`, `/claude-flow-swarm`, `/sparc`

**SPARC Methodology (8)**: spec, architect, code, test, review, docs, deploy, integrate

**Swarm Coordination (6)**: init, spawn, status, orchestrate, memory, consensus

**GitHub Operations (3)**: pr, issue, repo

**Performance & Optimization (4)**: analyze, monitor, optimize, train

**Utility Commands (5)**: hooks list, agent list, memory search, workflow execute, automate

### Consolidation Benefits

- **67% reduction**: 91 → 30 commands (improved discoverability)
- **Fast discovery**: <15 seconds to find right command (was 5+ minutes)
- **Zero data loss**: All 61 archived commands preserved in `.claude/commands-archive/`
- **Clear categories**: 6 focused categories vs 15 overlapping ones
- **Migration guide**: See `.claude/COMMAND_INDEX.md` for old→new mappings

### Command Documentation

- **[.claude/COMMAND_INDEX.md](.claude/COMMAND_INDEX.md)** - Complete command directory with migration guide
- **[.claude/SYSTEM_GUIDE.md](.claude/SYSTEM_GUIDE.md)** - Commands vs Agents vs Roo rules integration
- **[.claude/commands/README.md](.claude/commands/README.md)** - Command system overview
- **[.claude/commands-archive/README.md](.claude/commands-archive/README.md)** - Consolidation history

## Getting Help

- **Architecture questions** → Read `docs/architecture.md` and related design documents
- **API usage** → Check `docs/api-reference.md` and example code in `examples/`
- **Test examples** → Review test suite in `test/` for pattern implementations
- **OTP patterns** → Consult `docs/otp-patterns.md` for Erlang best practices
- **Agent usage** → Read `.claude/AGENT_INDEX.md` for agent quick reference
