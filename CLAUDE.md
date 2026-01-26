# Erlang MCP (Model Context Protocol) Implementation - Developer Guide

## Project Overview

**erlmcp** is a production-grade Erlang/OTP implementation of the Model Context Protocol (MCP) SDK. It enables seamless communication between AI assistants and local Erlang services through a standardized protocol. The project provides complete client and server implementations with full OTP compliance, supporting multiple transport mechanisms (stdio, TCP, HTTP) and comprehensive monitoring/telemetry.

**Repository**: https://github.com/banyan-platform/erlmcp
**Version**: 0.6.0+ (Phase 3+ development)
**License**: Apache 2.0
**Requires**: Erlang/OTP 25 or later

---

## Directory Structure

```
erlmcp/
├── src/                          # Core source code (54 Erlang modules)
│   ├── erlmcp.erl               # Main public API (servers, transports, config)
│   ├── erlmcp_server.erl         # MCP server implementation
│   ├── erlmcp_client.erl         # MCP client implementation
│   ├── erlmcp_registry.erl       # Process registry and management
│   ├── erlmcp_json_rpc.erl       # JSON-RPC protocol handling
│   ├── erlmcp_binding.erl        # Transport-server binding
│   ├── erlmcp_config*.erl        # Configuration and validation (4 modules)
│   ├── erlmcp_transport*.erl     # Transport implementations
│   ├── erlmcp_health*.erl        # Health monitoring and checks
│   ├── erlmcp_metrics*.erl       # Performance metrics collection
│   ├── erlmcp_chaos*.erl         # Chaos engineering and resilience testing
│   ├── erlmcp_graceful*.erl      # Graceful degradation and recovery
│   ├── erlmcp_otel.erl           # OpenTelemetry instrumentation
│   └── ... (monitoring, routing, report generation modules)
│
├── test/                          # Common Test suites and integration tests (49 test files)
│   ├── erlmcp_integration_SUITE.erl      # Full system integration tests
│   ├── erlmcp_comprehensive*.erl         # Comprehensive test suites
│   ├── erlmcp_failure_recovery*.erl      # Failure and recovery scenarios
│   ├── erlmcp_load_SUITE.erl             # Load testing
│   ├── erlmcp_benchmark.erl              # Performance benchmarks
│   ├── erlmcp_validation*.erl            # Configuration validation tests
│   ├── PHASE3_*.md                       # Test reports and assessments
│   └── ... (extensive test coverage)
│
├── include/                       # Erlang header files
│   └── erlmcp.hrl                # Type definitions, records, constants
│
├── examples/                      # Reference implementations
│   ├── simple/                    # Minimal stdio MCP example
│   ├── calculator/                # TCP transport example
│   └── weather/                   # HTTP transport example with resources
│
├── docs/                          # Technical documentation
│   ├── architecture.md            # System design and components
│   ├── protocol.md                # MCP protocol implementation details
│   ├── transport_behavior.md      # Transport interface and extensibility
│   ├── transport_configuration.md # Configuration examples
│   ├── migration_guide.md         # Upgrade guide (v0.5.x → v0.6.0)
│   ├── api-reference.md           # Complete API documentation
│   └── ... (performance, security, validation reports)
│
├── config/                        # Configuration files
│   ├── sys.config                 # Erlang system configuration
│   └── vm.args                    # VM arguments
│
├── priv/                          # Private resources
│   ├── test/                      # Test data and fixtures
│   ├── scripts/                   # Utility scripts
│   ├── static/                    # Static resources
│   └── release-notes/             # Release documentation
│
├── memory/                        # Claude Flow memory and session storage
│   ├── agents/                    # Agent state snapshots
│   └── sessions/                  # Development session records
│
├── rebar.config                   # Build configuration and dependencies
├── rebar.lock                      # Locked dependency versions
├── Makefile                        # Convenient make targets
├── README.md                       # Project overview
└── CLAUDE.md                       # This file - AI assistant guide

```

---

## Technology Stack

### Core Dependencies
- **jsx** (3.1.0) - Erlang JSON parser and generator
- **jesse** (1.8.1) - JSON Schema validator
- **opentelemetry** - Observability and tracing
- **opentelemetry_exporter** - Metrics/traces export

### Testing Dependencies
- **proper** (1.4.0) - Property-based testing
- **meck** (0.9.2) - Mocking and stubbing
- **coveralls** (2.2.0) - Code coverage reporting

### Development Tools
- **rebar3** - Build and dependency management
- **recon** (2.5.3) - Runtime introspection
- **observer_cli** (1.7.4) - Monitoring CLI
- **rebar3_format** - Code formatting
- **rebar3_lint** - Code linting (xref, dialyzer)

### Protocols
- **JSON-RPC 2.0** - Request-response protocol
- **MCP (Model Context Protocol)** - AI integration protocol

---

## Key Modules and Responsibilities

### Core Server/Client
| Module | Purpose |
|--------|---------|
| `erlmcp.erl` | Public API gateway; server/transport/config management |
| `erlmcp_server.erl` | MCP server implementation; resource/tool/prompt management |
| `erlmcp_client.erl` | MCP client; async operations with state management |
| `erlmcp_registry.erl` | Global process registry for servers and transports |

### Protocol & Communication
| Module | Purpose |
|--------|---------|
| `erlmcp_json_rpc.erl` | JSON-RPC 2.0 protocol encoding/decoding |
| `erlmcp_router.erl` | Message routing and dispatch |
| `erlmcp_binding.erl` | Bind transports to servers; manage bidirectional communication |

### Transport Layer
| Module | Purpose |
|--------|---------|
| `erlmcp_transport_*` | Transport implementations (stdio, TCP, HTTP) |
| `erlmcp_transport_registry` | Manage active transport instances |
| Transport behaviors | Pluggable interface for custom transports |

### Configuration & Validation
| Module | Purpose |
|--------|---------|
| `erlmcp_config.erl` | Configuration loading and management |
| `erlmcp_config_validation.erl` | Validate configs against schema |
| `erlmcp_config_schema.erl` | Schema definitions for validation |

### Monitoring & Health
| Module | Purpose |
|--------|---------|
| `erlmcp_health_monitor.erl` | Process health tracking |
| `erlmcp_metrics.erl` | Performance metrics collection |
| `erlmcp_registry_health_check.erl` | Registry health verification |
| `erlmcp_chaos_monitor.erl` | Chaos testing and resilience monitoring |

### Advanced Features
| Module | Purpose |
|--------|---------|
| `erlmcp_graceful_degradation.erl` | Degraded-mode operation support |
| `erlmcp_recovery_manager.erl` | Automatic recovery mechanisms |
| `erlmcp_otel.erl` | OpenTelemetry instrumentation |
| `erlmcp_chaos.erl` | Fault injection and chaos engineering |

---

## Build & Development Commands

### Common Build Tasks
```bash
# Compile project
make compile
rebar3 compile

# Run all tests
make test
rebar3 do eunit, ct, proper -c

# Static analysis
make dialyzer    # Type checking
make xref        # Cross-reference analysis
make lint        # Run all linting

# Code formatting
make format
rebar3 format

# Full validation
make check       # clean + compile + xref + dialyzer + test

# Test variations
make test-unit            # EUnit only
make test-integration     # Common Test suites only
make test-property        # PropEr property-based tests
make test-local           # Local examples compilation
```

### Development & Debugging
```bash
# Interactive Erlang shell
make console
rebar3 shell

# Development shell with recon/observer
make dev-console
ERL_FLAGS="-config config/sys.config" rebar3 as dev shell

# Observer GUI (remote debugging)
make observer
erl -name debug@127.0.0.1 -setcookie erlmcp_secret_cookie -run observer

# Performance profiling
make profile
rebar3 as dev shell --eval "recon_trace:calls({erlmcp_client, '_', '_'}, 100)."

# Coverage report
make coverage-report
```

### Testing Examples in Shell
```bash
# Run simple example
make test-server
rebar3 shell --eval "simple_server:start()."

# Run calculator example
make test-client
rebar3 shell --eval "simple_client:run()."

# Run advanced client
make test-advanced-client
rebar3 shell --eval "simple_client:run_advanced()."
```

### Release & Deployment
```bash
# Production release
make release
rebar3 as prod release

# Docker support
make docker-build
make docker-run

# Package installation
make install
rebar3 do compile, escriptize

# Hex package publishing
make publish
rebar3 hex publish package
```

---

## Testing Patterns

### Test Organization
The project uses three complementary testing approaches:

#### 1. EUnit - Module-level unit tests
```bash
rebar3 eunit
# Tests in: test/*_test.erl
# Focus: Individual function behavior, edge cases
```

#### 2. Common Test (CT) - Integration and system tests
```bash
rebar3 ct
# Tests in: test/*_SUITE.erl
# Focus: Component interaction, protocol correctness, transport behavior
# Key suites:
#   - erlmcp_integration_SUITE.erl (end-to-end)
#   - erlmcp_failure_recovery_SUITE.erl (error scenarios)
#   - erlmcp_load_SUITE.erl (performance under load)
#   - erlmcp_comprehensive*.erl (subsystem coverage)
```

#### 3. PropEr - Property-based testing
```bash
rebar3 proper -c
# Tests in: test/*_properties.erl
# Focus: Invariants, protocol compliance, randomized inputs
```

### Test Coverage
- **Unit tests**: Core logic, error handling
- **Integration tests**: Client-server communication, transport behavior
- **Failure scenarios**: Connection loss, timeouts, malformed messages
- **Load testing**: Concurrent clients, sustained throughput, backpressure
- **Property testing**: Protocol invariants, config validation

### Running Specific Tests
```bash
# Single test module
rebar3 eunit -m erlmcp_config_validation

# Specific test function
rebar3 ct -s erlmcp_integration_SUITE -t initialization_test

# With verbose output
rebar3 ct -v

# With configuration
rebar3 ct --config priv/test/ct.config
```

---

## Erlang/OTP Conventions

### Module Naming
- **Core API modules**: `erlmcp_*.erl` (no behavior suffix)
- **Behavior implementations**: `erlmcp_*_*behavior*.erl`
- **Test modules**: `*_test.erl` (EUnit) or `*_SUITE.erl` (Common Test)
- **Transport modules**: `erlmcp_transport_<name>.erl`

### Supervision Trees
- **Root supervisor**: `erlmcp_sup` - manages all top-level services
- **Subsystem supervisors**: `erlmcp_*_sup` (health, metrics, registry)
- **Strategy**: Mix of `one_for_one` and `one_for_all` based on dependencies
- **Supervision tree depth**: Typically 2-3 levels

### Process Registration
- **Global registry**: `:erlmcp_registry` - registered servers/transports
- **Named processes**: Used for singleton services (health monitor, metrics)
- **Local names**: Used within supervisors for restart tracking

### Type Specifications
- **All exported functions** have `-spec` declarations
- **Type definitions** in header file `erlmcp.hrl`
- **Custom types**: Prefer semantic types over generic terms
  - `server_id() :: atom()`
  - `transport_id() :: atom()`
  - `transport_type() :: stdio | tcp | http`

### Error Handling
- **Return tuples**: `{ok, Result} | {error, Reason}`
- **Reason types**: Atom or term with error context
- **Exceptions**: Used for invariant violations only
- **Logging**: Always include context (server ID, transport ID, etc.)

### Logging
- **Logger module**: `:logger` (Erlang 21+)
- **Levels**: `:debug`, `:info`, `:warning`, `:error`
- **Format**: Include relevant context (IDs, operation, reason)
- **Configuration**: `config/sys.config`

---

## Transport Architecture

### Pluggable Transport System
The transport layer is behavior-based, allowing custom transports:

```erlang
-callback start_link(Transport, Config) -> {ok, Pid} | {error, Reason}.
-callback send(Transport, Data) -> ok | {error, Reason}.
-callback close(Transport) -> ok.
```

### Built-in Transports
| Transport | Use Case | Config |
|-----------|----------|--------|
| `stdio` | Local processes, demos | `{stdio, []}` |
| `tcp` | Network clients/servers | `{tcp, [{host, ...}, {port, ...}]}` |
| `http` | Web-based integrations | `{http, [{url, ...}, {headers, ...}]}` |

### Transport Selection
```erlang
% Stdio - single process, synchronous
{ok, Server} = erlmcp:start_server(my_server,
    #{transport => {stdio, []}}).

% TCP - networked, async with reconnection
{ok, Transport} = erlmcp:start_transport(my_transport,
    {tcp, [{host, "localhost"}, {port, 5000}]}).

% HTTP - stateless, request-response
{ok, Transport} = erlmcp:start_transport(my_transport,
    {http, [{url, "http://api.example.com"}]}).
```

### Transport Configuration Validation
```erlang
% Validate transport config before starting
{ok, ValidConfig} = erlmcp:validate_transport_config({tcp, Config}),

% Enhanced validation with error details
{ok, Details} = erlmcp:comprehensive_transport_validation(
    {tcp, Config},
    erlmcp_config_validation).
```

---

## Configuration Management

### Configuration Files
- **sys.config**: Application environment variables
- **vm.args**: VM startup parameters
- **Transport configs**: Passed to `start_transport/2-3`

### Configuration Schema Validation
```erlang
% Get schema for transport type
{ok, Schema} = erlmcp:get_config_schema(tcp),

% Validate against schema
{ok, Config} | {error, Errors} = erlmcp_config_validation:validate(Config, Schema),

% Format validation errors for display
Formatted = erlmcp:format_validation_error(Field, Error, ErrorType).
```

### Common Configuration Options

#### TCP Transport
```erlang
#{
    host => "localhost",
    port => 5000,
    backlog => 128,
    nodelay => true,
    keepalive => true,
    buffer_size => 8192,
    timeout => 30000,
    reconnect_delay => 1000,
    max_reconnect_attempts => 5
}
```

#### HTTP Transport
```erlang
#{
    url => "http://api.example.com",
    headers => [{<<"Accept">>, <<"application/json">>}],
    method => <<"POST">>,
    timeout => 30000,
    ssl_options => [],
    pool_size => 10
}
```

#### Server Configuration
```erlang
#{
    capabilities => #mcp_server_capabilities{
        resources => #mcp_capability{enabled => true},
        tools => #mcp_capability{enabled => true},
        prompts => #mcp_capability{enabled => true}
    },
    metadata => #{
        name => "My MCP Server",
        version => "1.0.0"
    },
    max_message_size => 1048576
}
```

---

## Development Workflow

### Adding New Features

1. **Design Phase**
   - Understand existing patterns in similar modules
   - Design public API (functions in main module)
   - Plan internal implementation module(s)

2. **Test First** (TDD)
   - Write test cases in `test/*_SUITE.erl` or `test/*_test.erl`
   - Define both happy path and error cases
   - Use property-based tests for invariants

3. **Implementation**
   - Implement module(s) in `src/`
   - Add type specifications to all exported functions
   - Follow existing code style (see Makefile: `make format`)

4. **Documentation**
   - Add module documentation comments
   - Document function behavior in comments
   - Update relevant files in `docs/`

5. **Testing & Validation**
   ```bash
   make test                # All tests
   make lint               # Code quality
   make check              # Full validation
   ```

6. **Commit & Push**
   ```bash
   git add src/module_name.erl test/module_name_*_test.erl
   git commit -m "feat: description of feature"
   git push origin claude/add-claude-documentation-yPDH9
   ```

### Code Style
- **Line length**: Max 100 characters (see rebar.config: `paper: 100`)
- **Indentation**: 4 spaces (configurable, see rebar.config)
- **Formatting**: Use `make format` (rebar3_format plugin)
- **Comments**: Use `%%` for module-level, `%` for inline
- **Naming**: Snake_case for modules/functions, CamelCase for types/records

### Performance Considerations
- **Avoid** deep recursion; use iterative patterns or tail-call optimization
- **Message passing**: Prefer patterns over alternatives
- **ETS tables**: Use for fast lookups (e.g., registry implementation)
- **Binary handling**: Use binary patterns and string:concat for efficiency
- **Monitor metrics**: Use OpenTelemetry for instrumentation

---

## Key Implementation Details

### Server Lifecycle
```
start_server(ServerId, Config)
    → register with registry
    → initialize capabilities
    → await transport binding
    → start message handling loop
    → handle shutdown gracefully
```

### Client Lifecycle
```
start_client(TransportConfig)
    → establish transport connection
    → send initialization request
    → await server capabilities
    → ready for calls/queries
    → automatic reconnection on failure
```

### Message Flow
```
Client → send JSON-RPC request → Transport
    ↓
    Transport → deliver to Server
    ↓
    Server → process request (protocol dispatch)
    ↓
    Server → send JSON-RPC response → Transport
    ↓
    Transport → deliver to Client
    ↓
    Client → parse response → invoke callback/return result
```

### Resource/Tool/Prompt Registration
```erlang
% Resources (read-only data)
erlmcp_server:add_resource(Server, <<"weather://nyc">>, ReadFn).

% Tools (callable functions with schema)
erlmcp_server:add_tool(Server, <<"get_weather">>, ImplementationFn, Schema).

% Prompts (templates with arguments)
erlmcp_server:add_prompt(Server, <<"summarize">>, PromptTemplate, Meta).
```

---

## Common Development Scenarios

### Scenario 1: Add New Transport Type
1. Create `src/erlmcp_transport_custom.erl`
2. Implement transport behavior (start_link, send, close)
3. Register in transport supervisor
4. Add tests in `test/erlmcp_transport_custom_test.erl`
5. Update docs/transport_configuration.md with examples

### Scenario 2: Add Monitoring Feature
1. Create `src/erlmcp_feature_monitor.erl` (or extend existing)
2. Add metrics collection using `erlmcp_metrics`
3. Create supervisor for monitoring process (if needed)
4. Add integration in root supervisor if global monitoring needed
5. Write tests and update health check procedures

### Scenario 3: Extend Configuration Validation
1. Update schema in `src/erlmcp_config_schema.erl`
2. Add validation rules in `src/erlmcp_config_validation.erl`
3. Add test cases in `test/erlmcp_config_validation_SUITE.erl`
4. Update error formatting functions in main module

### Scenario 4: Fix Protocol Issue
1. Write test case reproducing the issue in Common Test suite
2. Update JSON-RPC handling in `erlmcp_json_rpc.erl` or protocol dispatch
3. Update server/client message handling if needed
4. Run full test suite including load tests
5. Document protocol change in `docs/protocol.md` if needed

---

## Testing Production Readiness

### Checklist Before Release
- [ ] All tests passing: `make check`
- [ ] Coverage above baseline (see reports in `_build/test/cover/`)
- [ ] Performance benchmarks stable (compare with baseline)
- [ ] Load tests with expected concurrent load
- [ ] Failure recovery tests passing
- [ ] Memory leak checks with recon
- [ ] Code formatting: `make format`
- [ ] No new warnings from dialyzer
- [ ] Documentation updated
- [ ] Examples working correctly
- [ ] Phase test reports in `test/` directory

### Performance Baseline
Check recent test reports:
- `test/PHASE3_COMPREHENSIVE_TEST_REPORT.md`
- `test/PERFORMANCE_ANALYSIS_SUMMARY.md`
- `test/LOAD_TESTING.md`

---

## Git Workflow

### Branch Naming
- Feature: `claude/feature-name-sessionId`
- Bug fix: `claude/fix-name-sessionId`
- Refactor: `claude/refactor-name-sessionId`
- **Current branch**: `claude/add-claude-documentation-yPDH9`

### Commit Messages
```
feat: add new feature with brief description
fix: resolve issue with clear explanation
refactor: reorganize code for clarity
test: add or update test cases
docs: update documentation
chore: dependency updates, minor fixes
```

### Push Protocol
```bash
# Always push with upstream tracking
git push -u origin <branch-name>

# For existing branch
git push origin <branch-name>

# Force push ONLY if explicitly requested
# NEVER force push to main/master
git push --force-with-lease origin <branch-name>
```

### Handling Network Failures
```bash
# Automatic retry with exponential backoff (2s, 4s, 8s, 16s)
git push -u origin branch-name  # Retry up to 4 times
```

---

## Troubleshooting

### Build Issues
```bash
# Clean rebuild
make clean
make compile

# Deep clean (including deps)
make distclean
rebar3 get-deps && rebar3 compile
```

### Test Failures
```bash
# Run specific failing test with verbose output
rebar3 ct -s erlmcp_integration_SUITE -t test_name -v

# Check test log
cat _build/test/logs/erlmcp_integration_SUITE.logs
```

### Runtime Issues
```bash
# Enable debug logging
erl -pa _build/default/lib/*/ebin -config config/sys.config

% In shell:
> logger:set_handler_config(default, level, debug).

# Attach observer
rebar3 as dev shell
%% then in shell:
> observer:start().
```

### Performance Problems
```bash
# Profile specific function
rebar3 as dev shell --eval "recon_trace:calls({erlmcp_server, '_', '_'}, 100)."

# Memory analysis
> recon:memory(processes, 10).
> recon:bin_leak([]).

# Generate performance report
rebar3 ct -s erlmcp_load_SUITE -t sustained_load_test
```

---

## Important Rules for AI Assistants

### Do's
- Read source code before making changes
- Batch file operations in single messages
- Follow existing code patterns and style
- Write tests for new functionality
- Use existing modules before creating new ones
- Document changes appropriately
- Run `make check` before committing

### Don'ts
- Don't modify code without understanding context
- Don't create new files when editing existing ones would work
- Don't skip testing or type specifications
- Don't force-push to main branches
- Don't hardcode configuration or secrets
- Don't mix unrelated changes in single commit
- Don't commit to wrong branches

### Parallel Execution
```bash
# ✅ CORRECT: Batch all operations in one message
Task 1: Read source files
Task 2: Write tests
Task 3: Run build
Task 4: Execute tests

# ❌ WRONG: Sequential messages
Message 1: Read file
Message 2: Edit file
Message 3: Run test
Message 4: Commit
```

---

## Additional Resources

### Documentation
- **Architecture**: `docs/architecture.md`
- **Protocol**: `docs/protocol.md`
- **Transport Guide**: `docs/transport_behavior.md`
- **Configuration**: `docs/transport_configuration.md`
- **API Reference**: `docs/api-reference.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Migration**: `docs/migration_guide.md`

### Examples
- **Simple**: `examples/simple/` - Minimal stdio example
- **Calculator**: `examples/calculator/` - TCP client/server
- **Weather**: `examples/weather/` - HTTP with resources

### External References
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [JSON Schema](https://json-schema.org/)

---

**Last Updated**: January 2026
**Current Phase**: Phase 3+ (Transport refactoring, monitoring, validation)
**Maintainers**: Community Contributors
