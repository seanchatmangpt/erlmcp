# Test Strategy: erlmcp + TAIEA Integration

**Version**: v1.0.0
**Date**: 2026-01-26
**Scope**: Unified test infrastructure for erlmcp (Model Context Protocol) and TAIEA (Autonomic System)

---

## Overview

This document defines the testing strategy for the erlmcp + TAIEA workspace, establishing expectations for unit tests, integration tests, performance benchmarks, and test organization.

**Test Architecture**:
```
┌─────────────────────────────────────────────┐
│  CI/CD Pipeline (GitHub Actions)            │
│  - Unit tests (fast feedback)               │
│  - Integration tests (on main/releases)     │
│  - Performance benchmarks                   │
└─────────────────────────────────────────────┘
         ↓                           ↓
   ┌───────────┐          ┌──────────────────┐
   │Unit Tests │          │Integration Tests │
   ├───────────┤          ├──────────────────┤
   │EUnit      │          │Common Test Suite │
   │Proper     │          │E2E Workflows     │
   │Mocks      │          │Failure Scenarios │
   └───────────┘          │Performance       │
                          └──────────────────┘
```

---

## 1. Unit Test Expectations (Chicago TDD)

### 1.1 Test Framework & Tools

- **Framework**: EUnit (Erlang)
- **Property-based Testing**: Proper
- **Mocking**: Meck
- **Assertion Style**: Chicago School (state-based, not mockist)

### 1.2 Test Structure

```erlang
%% ✓ CORRECT: Chicago TDD Pattern
test_resource_lookup() ->
    % Arrange: Set up real objects, not mocks
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    erlmcp_server:add_resource_template(Server, Uri, Desc, Handler),

    % Act: Execute behavior
    Result = erlmcp_server:read_resource(Server, Uri),

    % Assert: Verify observable output
    ?assertMatch({ok, #mcp_content{...}}, Result).
```

### 1.3 Coverage Requirements

- **Minimum**: 80% code coverage
- **Target**: 85%+ coverage
- **Measured**: Via `rebar3 cover`
- **Tracked**: Per-module in CI/CD

### 1.4 Test Organization

```
test/
├── erlmcp_server_tests.erl         # Server core (unit)
├── erlmcp_transport_stdio_tests.erl  # Stdio transport
├── erlmcp_transport_tcp_tests.erl    # TCP transport
├── erlmcp_transport_http_tests.erl   # HTTP transport
├── erlmcp_json_rpc_tests.erl        # JSON-RPC protocol
├── erlmcp_registry_tests.erl        # Resource/Tool registry
├── erlmcp_config_validation_tests.erl # Config validation
│
├── integration_SUITE.erl           # Integration tests (CT)
├── test_utils.erl                  # Shared fixtures & helpers
└── test_log_handler.erl           # Test logging
```

### 1.5 Mocking Strategy

- **Use mocks ONLY for**:
  - External services (HTTP clients, databases)
  - Side effects (file I/O, network)
  - Third-party APIs

- **DO NOT mock**:
  - Internal modules (use real objects)
  - Behavior under test
  - Erlang built-ins

```erlang
%% ✓ CORRECT: Real objects, mock external
test_tool_invocation() ->
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Caps),
    erlmcp_server:add_tool(Server, <<"tool">>, fun(Args) -> handle(Args) end),

    Result = erlmcp_server:call_tool(Server, <<"tool">>, #{}),
    ?assertMatch({ok, _}, Result).

%% ✗ WRONG: Mocking internal behavior
test_tool_invocation_bad() ->
    meck:new(erlmcp_server, []),
    meck:expect(erlmcp_server, call_tool, fun(_, _, _) -> {ok, fake} end),

    % This tests meck, not your code
    Result = erlmcp_server:call_tool(mock_server, <<"tool">>, #{}),
    meck:unload().
```

### 1.6 Test Data & Fixtures

Provided by `test_utils.erl`:

```erlang
% Capabilities
test_mcp_capabilities()              % Full capabilities
test_mcp_capabilities(resources_only)  % Resources only
test_mcp_capabilities(tools_only)      % Tools only

% Schemas & handlers
test_tool_schema()                   % Valid JSON schema
test_resource_template()             % Resource template
test_prompt_args()                   % Prompt arguments

% Data generators
gen_random_uri()                     % Random resource URI
gen_random_tool()                    % Random tool name
gen_test_message(Type)               % Sample JSON-RPC messages
```

### 1.7 Error Handling Tests

All error paths must be tested:

```erlang
test_invalid_json_parsing() ->
    % Invalid JSON must be rejected
    ?assertMatch({error, _}, erlmcp_json_rpc:parse_request(<<"invalid">>)).

test_missing_resource() ->
    % Non-existent resources must return error
    ?assertMatch({error, not_found},
                 erlmcp_server:read_resource(Server, <<"nonexistent://">>)).

test_tool_invocation_failure() ->
    % Tool failures must be caught
    erlmcp_server:add_tool(Server, <<"fail">>, fun(_) -> {error, oops} end),
    ?assertMatch({error, oops}, erlmcp_server:call_tool(Server, <<"fail">>, #{})).
```

---

## 2. Integration Test Coverage

### 2.1 Integration Test Framework

- **Framework**: Common Test (CT)
- **Test Suites**: `integration_SUITE.erl`
- **Scope**: Multi-component workflows, failure recovery, performance

### 2.2 Integration Test Groups

#### 2.2.1 erlmcp_group (Sequenced)
Tests erlmcp functionality in isolation:

1. **Server Lifecycle**: Start, register, stop
2. **Client-Server Handshake**: Capability negotiation
3. **Resource Management**: List, read, templates
4. **Tool Invocation**: Register, call, results
5. **Prompt Execution**: Register, execute with arguments
6. **Error Handling**: Invalid JSON, missing resources, failures
7. **Concurrent Operations**: Multiple simultaneous requests

#### 2.2.2 taiea_group (Sequenced)
Tests TAIEA autonomic functionality:

1. **System Startup**: Initialization, status checking
2. **Autonomic Response**: Stimulus handling
3. **Self-Healing**: Fault detection and recovery
4. **Adaptive Learning**: Pattern recognition

#### 2.2.3 e2e_group (Sequenced, with shared state)
Tests full erlmcp + TAIEA integration:

1. **Complete Workflow**: Tool invocation triggering TAIEA response
2. **Failure Recovery**: Error handling and system resilience
3. **Load Distribution**: Multiple concurrent tools, agents

#### 2.2.4 performance_group (No sequencing)
Performance benchmarks and baselines:

1. **Erlmcp Baseline**: Throughput, latency per operation
2. **Load Test**: Sustained performance under load

### 2.3 Integration Test Expectations

All integration tests must:

- ✓ Have clear test names describing the scenario
- ✓ Use `ct:comment()` for documentation
- ✓ Use `ct:pal()` for debug output
- ✓ Handle async operations with timeouts
- ✓ Assert observable outcomes (not implementation)
- ✓ Clean up resources in `end_per_testcase`

```erlang
erlmcp_resource_management(Config) ->
    ct:comment("Testing resource management operations"),

    Server = test_utils:setup_erlmcp([{capabilities, ...}]),

    {Template, Desc, Handler} = test_utils:test_resource_template(),
    erlmcp_server:add_resource_template(Server, Template, Desc, Handler),

    {ok, Resources} = erlmcp_server:list_resources(Server),
    ?assert(lists:keymember(Template, 1, Resources)),

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_resource_management: PASS").
```

### 2.4 Failure Scenario Testing

Integration tests must cover failure modes:

1. **Invalid Inputs**: Malformed JSON, missing fields
2. **Resource Failures**: Missing resources, timeout
3. **Transport Failures**: Connection drops, partial writes
4. **System Overload**: Excessive concurrent requests
5. **Recovery**: System recovery after fault injection

### 2.5 Performance Baselines

Baseline expectations (adjustable based on hardware):

| Operation | Target | Tolerance |
|-----------|--------|-----------|
| Tool invocation latency | <5ms | <10ms |
| Resource list (100 items) | <10ms | <20ms |
| Concurrent ops (10 clients) | success rate >95% | min 90% |
| Sustained load (5 sec) | all ops succeed | >80% success |

---

## 3. Test Organization

### 3.1 Directory Structure

```
/erlmcp
├── test/                           # Test suite root
│   ├── erlmcp_*.erl               # Unit tests (EUnit)
│   ├── integration_SUITE.erl      # Integration tests (CT)
│   ├── test_utils.erl             # Shared utilities
│   └── test_log_handler.erl       # Log capture for tests
│
├── Makefile                        # Build targets
├── rebar.config                    # Rebar3 config (test profile)
└── TEST_STRATEGY.md                # This document
```

### 3.2 Test Execution

```bash
# Run all tests
make test

# Run specific test types
make test-unit                      # Fast unit tests only
make test-int                       # Integration tests
make test-perf                      # Performance benchmarks
make test-quick                     # Quick smoke tests (<10s)

# Generate coverage report
make coverage                        # Opens _build/test/cover/index.html

# Run with verbose output
rebar3 ct -v
rebar3 eunit -v
```

### 3.3 CI/CD Pipeline

GitHub Actions workflow (`.github/workflows/test.yml`):

```yaml
# Unit tests: Every push/PR (fast feedback, <2 min)
- run: make test-unit
  timeout-minutes: 2

# Integration tests: On main/releases only (comprehensive, <5 min)
- run: make test-int
  if: github.ref == 'refs/heads/main'
  timeout-minutes: 5

# Performance benchmarks: On releases
- run: make test-perf
  if: startsWith(github.ref, 'refs/tags/')
  timeout-minutes: 10

# Coverage report
- run: make coverage
  if: always()
```

---

## 4. Flakiness Handling

### 4.1 Async/Timing Issues

Tests involving async operations must use proper timeouts:

```erlang
%% ✓ CORRECT: Proper timeout handling
test_async_operation() ->
    % Send request
    taiea_system:stimulus(System, {load, high}),

    % Wait for async response with timeout
    test_utils:wait_for_condition(
        fun() ->
            case taiea_system:get_status(System) of
                {ok, Status} -> maps:get(adapting, Status, false);
                {error, _} -> false
            end
        end,
        5000,  % 5 second timeout
        "System adaptation"
    ).

%% ✗ WRONG: No timeout, race conditions
test_async_operation_bad() ->
    taiea_system:stimulus(System, {load, high}),
    timer:sleep(100),  % Fixed sleep, unreliable
    Status = taiea_system:get_status(System),
    ?assertEqual(adapting, maps:get(adapting, Status)).
```

### 4.2 Determinism

Tests must be deterministic:

- No time-dependent assertions (use elapsed time ranges)
- No random data without seeds
- No global state dependencies
- Proper cleanup in `end_per_testcase`

### 4.3 Flaky Test Investigation

When tests fail intermittently:

1. **Identify root cause**: Timing, resource exhaustion, race condition?
2. **Add retries**: `ct:fail()` is immediate, add `eventually()` wrapper
3. **Increase timeouts**: Conservative timeouts during development
4. **Log extensively**: Use `ct:pal()` to understand timing
5. **Document**: Comment why test might be flaky

---

## 5. Test Metrics & Reporting

### 5.1 Coverage Reporting

Coverage is generated automatically:

```bash
make coverage
# Generates: _build/test/cover/index.html
```

Coverage dashboard shows:
- Per-module coverage %
- Missing lines per module
- Functional vs clause coverage

### 5.2 Test Metrics Tracked

- **Pass Rate**: Should be 100%
- **Execution Time**: Per-test and aggregate
- **Coverage**: Per-module and workspace
- **Flakiness**: Failed then passed (tracked in CI)

### 5.3 Test Reporting in CI

Each test run reports:
- ✓/✗ Pass/fail status
- Execution time (unit tests <2 min, integration <5 min)
- Coverage summary (% lines, functions, clauses covered)
- Failures with full error context

---

## 6. Best Practices

### 6.1 Writing Tests

1. **One assertion per test case** (or related assertions)
2. **Clear test names**: `test_<what>_<condition>_<expected>`
3. **Setup is minimal**: Use fixtures from `test_utils`
4. **Cleanup is comprehensive**: All processes stopped, files deleted
5. **No test interdependencies**: Tests run independently

### 6.2 Test Maintenance

- Keep tests close to code (same directory structure)
- Refactor test code like production code
- Use constants, not magic numbers
- Document complex test logic
- Review tests in code reviews (not less important than code)

### 6.3 Test-Driven Development (TDD)

For new features, follow RED → GREEN → REFACTOR:

```erlang
%% Step 1: RED - Write failing test
test_new_feature() ->
    {ok, Resource} = erlmcp_server:read_resource(Server, Uri),
    ?assertMatch(#mcp_content{caching = cached}, Resource).  % NEW FEATURE

%% Step 2: Minimal implementation to make test GREEN
read_resource(Server, Uri) ->
    % ...existing code...
    Content#mcp_content{caching = cached}.  % Add field

%% Step 3: REFACTOR - Improve without breaking test
% Extract caching logic to separate module, etc.
```

---

## 7. Known Limitations & Future Improvements

### 7.1 Current Limitations

- ✗ No distributed testing (single node only)
- ✗ No chaos engineering (limited fault injection)
- ✗ Limited performance profiling
- ✗ No stress testing for memory leaks

### 7.2 Future Improvements

- [ ] Distributed test suite (multi-node clusters)
- [ ] Fault injection framework (network partitions, crashes)
- [ ] Memory profiling (EQC real property-based tests)
- [ ] Continuous performance regression detection
- [ ] Test result dashboard (trends over time)

---

## 8. Running Tests Locally

### 8.1 Quick Start

```bash
# Install dependencies
rebar3 get-deps

# Run all tests
make test

# Run specific test file
rebar3 eunit test/erlmcp_server_tests.erl

# Run integration tests
rebar3 ct

# Check coverage
make coverage
```

### 8.2 Development Workflow

```bash
# Watch and test (using rebar3 shell)
rebar3 shell

# In shell:
> rebar3:do(eunit, ct).

# Or specific test
> eunit:test(erlmcp_server_tests).
```

### 8.3 Debugging Test Failures

```bash
# Run with verbose output
rebar3 eunit -v

# Run with full stack traces
rebar3 eunit --verbose

# Common Test verbose
rebar3 ct -v

# Inspect CT logs
cat ct_logs/run.2024*/erlmcp_server_tests_3.log
```

---

## 9. Test Checklist for New Features

Before merging new code:

- [ ] All unit tests pass (`make test-unit`)
- [ ] All integration tests pass (`make test-int`)
- [ ] Code coverage >= 80% for new code
- [ ] No compiler warnings
- [ ] No Dialyzer errors
- [ ] Performance baseline met or explained
- [ ] Error cases tested (happy path + error path)
- [ ] Concurrent access tested
- [ ] Documentation updated in TEST_STRATEGY.md if needed

---

## 10. Contact & Questions

For questions about test strategy:
- Review this document
- Check `test_utils.erl` for available fixtures
- Check existing tests in `test/` directory
- Run `make help` for available targets

---

**Last Updated**: 2026-01-26
**Maintained By**: Agent 13 (Unified Test Infrastructure)
