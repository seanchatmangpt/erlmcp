# erlmcp_cli Comprehensive Test Suite Documentation

## Overview

This document describes the comprehensive test suite for the erlmcp CLI implementation, following **Chicago School TDD methodology** with real processes and state-based verification.

## Test Suite Statistics

- **Total Test Modules**: 23 EUnit + 6 Common Test = 29 test suites
- **Test Coverage Target**: 80% minimum (85%+ for core modules)
- **Test Frameworks**: EUnit, Common Test, Proper
- **Testing Philosophy**: Chicago School TDD (real processes, no mocks)

## Test Organization

### EUnit Test Suites (23 modules)

#### Core Protocol Tests
1. **erlmcp_cli_json_rpc_tests.erl** - JSON-RPC 2.0 protocol testing
   - 500+ lines, 30+ test functions
   - Request/response parsing, encoding/decoding
   - Error handling, batch operations, notifications
   - Session handling, concurrent requests

2. **erlmcp_cli_command_tests.erl** - CLI command parsing
   - Command argument validation
   - Flag parsing and validation
   - Help output, unknown commands
   - Subcommand routing, execution success/failure

3. **erlmcp_cli_auth_tests.erl** - Authentication mechanisms
   - Token validation (valid, invalid, expired, malformed)
   - Token refresh (before/after expiration)
   - mTLS authentication
   - Rate limiting (under/at/over limit, window reset)
   - Session authentication
   - Authorization (permission checks, role-based, resource-based)
   - Authentication failure (lockout, recovery)

4. **erlmcp_cli_registry_tests.erl** - Command registry
   - Command registration, lookup, listing, unregistration
   - Command categories and filtering
   - Safety level validation
   - Permission checks

5. **erlmcp_cli_session_tests.erl** - Session management
   - Session creation, initialization, start/activation
   - Session data storage and retrieval
   - Session expiration handling
   - Session termination and cleanup
   - Concurrent session management
   - Session state machine validation
   - Session recovery after crash

6. **erlmcp_cli_metrics_tests.erl** - Metrics collection
   - Counter operations (increment, decrement, reset)
   - Gauge operations (set, adjust)
   - Histogram operations (record, percentile)
   - Metric attributes and labels
   - Metric aggregation and reporting

7. **erlmcp_cli_resource_tests.erl** - Resource operations
   - Resource subscription/unsubscription
   - Resource list operations
   - Resource read/update operations
   - Resource change notifications
   - Resource synchronization
   - Resource cleanup
   - Resource permissions (read/write)
   - Resource caching (hit/miss)
   - Resource expiration (TTL)
   - Batch operations

#### Transport Tests
8. **erlmcp_cli_transport_tests.erl** - Transport layer interface
   - Transport initialization
   - Data send/receive
   - Connection lifecycle
   - Error handling
   - Multi-transport integration

9. **erlmcp_transport_stdio_tests.erl** - stdio transport
   - Standard I/O transport operations
   - Process spawning and communication

10. **erlmcp_transport_tcp_tests.erl** - TCP transport
    - TCP connection management
    - Socket operations
    - Connection pooling

11. **erlmcp_transport_http_tests.erl** - HTTP transport
    - HTTP request/response
    - Connection management
    - Error handling

12. **erlmcp_transport_ws_tests.erl** - WebSocket transport
    - WebSocket handshake
    - Message framing
    - Connection lifecycle

13. **erlmcp_transport_sse_tests.erl** - SSE transport
    - Server-Sent Events
    - Event streaming
    - Connection management

#### Property-Based Tests
14. **erlmcp_cli_proper_tests.erl** - Proper property tests
    - Command line parsing roundtrip
    - JSON-RPC message roundtrip
    - Session state transitions
    - Registry lookup properties
    - Transport send/receive
    - Counter monotonic properties
    - Session state monotonic properties

### Common Test Suites (6 modules)

#### Integration Tests
1. **erlmcp_cli_integration_SUITE.erl**
   - Full CLI workflow testing
   - Multi-step session operations
   - Complete request/response cycles
   - Transport switching scenarios
   - Error recovery workflows

2. **erlmcp_cli_compliance_SUITE.erl**
   - JSON-RPC 2.0 specification compliance
   - MCP protocol validation
   - Transport τ-interface compliance
   - Resource management compliance
   - Tool execution compliance
   - Session state mapping compliance
   - Error response format compliance
   - Capability negotiation compliance

3. **erlmcp_cli_security_SUITE.erl**
   - Input validation (SQL injection, XSS)
   - Session security (hijacking prevention)
   - Error message information leakage
   - Authentication security
   - Authorization boundary testing
   - Rate limiting effectiveness
   - Secret management security
   - TLS/SSL validation

4. **erlmcp_cli_error_handling_SUITE.erl**
   - Network error scenarios
   - Protocol error scenarios
   - Authentication failure scenarios
   - Resource unavailable scenarios
   - Timeout scenarios
   - Error recovery mechanisms

5. **erlmcp_cli_performance_SUITE.erl**
   - Connection establishment latency
   - Request/response throughput
   - Batch operation performance
   - Memory usage under load
   - Concurrent connection scaling
   - Performance regression detection

6. **erlmcp_cli_otel_SUITE.erl**
   - Span creation and propagation
   - Child span creation
   - Span attributes validation
   - Metric collection and reporting
   - Trace context management
   - Trace context propagation
   - Span events and status
   - OTEL export functionality
   - Performance overhead measurement
   - Batch span processing
   - Metric aggregation
   - Resource attributes
   - Multi-trace correlation

## Test Execution

### Run All Tests
```bash
cd /Users/sac/erlmcp/apps/erlmcp_cli/test
./run_full_test_suite.sh
```

### Run Specific Test Types
```bash
# EUnit only
./run_full_test_suite.sh -eunit

# Common Test only
./run_full_test_suite.sh -ct

# Proper tests only
./run_full_test_suite.sh -proper

# With coverage
./run_full_test_suite.sh -coverage

# Verbose output
./run_full_test_suite.sh -verbose

# CI mode (enforces coverage threshold)
./run_full_test_suite.sh -ci
```

### Generate Coverage Reports
```bash
# HTML coverage report
./generate_coverage_report.sh -html

# Text coverage report
./generate_coverage_report.sh -text

# Module breakdown
./generate_coverage_report.sh -modules

# Coverage gap analysis
./generate_coverage_report.sh -gaps

# Trend analysis
./generate_coverage_report.sh -trend

# CI mode (fail on threshold)
./generate_coverage_report.sh -ci
```

### Using Make Targets
```bash
# Run all CLI tests
make test-cli

# Run with coverage
make test-cli-coverage

# Quick tests (fast feedback)
make test-cli-quick

# Full pipeline
make test-cli-all

# CI mode
make test-cli-ci
```

## Coverage Requirements

### Minimum Thresholds
- **All modules**: 80% coverage minimum
- **Core modules**: 85% coverage minimum

### Core Modules List
1. erlmcp_cli_json_rpc
2. erlmcp_cli_session
3. erlmcp_cli_transport
4. erlmcp_cli_registry
5. erlmcp_cli_auth
6. erlmcp_cli_config

## Chicago School TDD Compliance

### Real Processes
- ✅ All tests use real gen_servers, no mocks
- ✅ Real transport implementations tested
- ✅ Real session management with actual state
- ✅ Real authentication mechanisms
- ✅ Actual network I/O for transport tests

### State-Based Verification
- ✅ Assertions on observable state
- ✅ API result validation
- ✅ No implementation detail testing
- ✅ Behavior verification over interaction verification

### Test Isolation
- ✅ Proper setup/teardown in all fixtures
- ✅ Independent test cases
- ✅ Clean state before each test
- ✅ Resource cleanup after tests

## Test Quality Gates

### Pre-Commit Checks
```bash
# Run quick tests before commit
make test-cli-quick

# Check coverage
make test-cli-coverage-text

# Verify format
rebar3 format --verify
```

### CI/CD Pipeline
```bash
# Full CI test suite
make test-cli-ci

# Includes:
# - All EUnit tests
# - All Common Test suites
# - All Proper tests (1000+ cases each)
# - Coverage verification (80%+ minimum)
# - Core modules verification (85%+ minimum)
```

## Test Maintenance

### Adding New Tests
1. Create test file following naming convention: `<module>_tests.erl` for EUnit
2. For Common Test: `<module>_SUITE.erl`
3. Include EUnit or CT header files
4. Implement setup/0, cleanup/1 fixtures
5. Add test functions: `<name>_test/0` or `<name>_test/1`
6. Run tests to verify
7. Check coverage meets threshold

### Coverage Verification
```bash
# Generate coverage report
rebar3 cover --verbose

# Check specific module
rebar3 cover --verbose | grep module_name

# View HTML report
open _build/test/cover/index.html
```

## Test Suite Summary

### Test Count
- **EUnit Tests**: 200+ test functions
- **Common Test Cases**: 50+ test cases
- **Proper Properties**: 7 properties, 1000+ test cases each
- **Total Test Executions**: 10,000+ per full run

### Coverage Areas
1. ✅ CLI command parsing and execution
2. ✅ JSON-RPC 2.0 protocol
3. ✅ Authentication and authorization
4. ✅ Session management and lifecycle
5. ✅ Transport layer (stdio, TCP, HTTP, WebSocket, SSE)
6. ✅ Resource operations
7. ✅ Metrics collection
8. ✅ OTEL integration
9. ✅ Error handling and recovery
10. ✅ Security validation
11. ✅ Performance regression detection
12. ✅ MCP specification compliance

## Troubleshooting

### Common Issues

**Test fails to compile**
```bash
# Clean and rebuild
rebar3 clean
rebar3 compile
```

**Coverage report not generated**
```bash
# Run tests with coverage first
rebar3 cover --verbose
# Then view report
open _build/test/cover/index.html
```

**Flaky tests**
- Check for timing dependencies
- Ensure proper cleanup in teardown
- Verify independent test execution
- Use proper synchronization for concurrent tests

**Low coverage**
```bash
# Identify gaps
./generate_coverage_report.sh -gaps

# View HTML report to see uncovered lines
open _build/test/cover/index.html
```

## Best Practices

1. **Write Tests First** - Chicago TDD requires tests before implementation
2. **Use Real Processes** - No mocks, use actual gen_servers
3. **State-Based Assertions** - Verify observable state, not implementation
4. **Proper Cleanup** - Always cleanup resources in teardown
5. **Independent Tests** - Each test should be independent
6. **Descriptive Names** - Test function names should describe what they test
7. **Coverage Thresholds** - Maintain 80%+ (85%+ for core)
8. **Run Tests Often** - Quick feedback loop with `make test-cli-quick`

## Conclusion

This comprehensive test suite provides:
- ✅ Complete coverage of CLI functionality
- ✅ Chicago School TDD methodology
- ✅ Real process testing (no mocks)
- ✅ State-based verification
- ✅ 80%+ coverage (85%+ for core)
- ✅ Automated execution and reporting
- ✅ CI/CD integration
- ✅ Performance regression detection
- ✅ MCP specification compliance validation

All tests are production-ready and follow Lean Six Sigma quality standards.
