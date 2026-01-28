# ErlMCP Validation Test Suite

## Overview

This comprehensive validation test suite ensures that the ErlMCP system is production-ready by testing all aspects of the OTP architecture:

- **Integration Tests** - End-to-end system functionality validation
- **Load Tests** - High concurrency and throughput testing
- **Performance Tests** - Response time and resource usage benchmarks
- **Memory Tests** - Memory leak detection and stability validation
- **Health Tests** - System monitoring and recovery validation

## Quick Start

```bash
# Run complete validation suite
./test/run_validation.sh

# Run specific validation type
./test/run_validation.sh --integration
./test/run_validation.sh --load
./test/run_validation.sh --performance

# Quick validation (abbreviated tests)
./test/run_validation.sh --quick --verbose
```

## Test Suites

### 1. Integration Tests (`erlmcp_integration_SUITE.erl`)

Validates complete system integration and end-to-end workflows:

- **System Startup/Shutdown** - Supervisor hierarchy and graceful termination
- **Message Flow** - Complete client → transport → registry → server → response flow
- **Multi-transport Coordination** - Simultaneous stdio, TCP, HTTP transport handling
- **Configuration Management** - Hot-reload and validation
- **Failure Recovery** - Crash recovery and system resilience
- **Real Client Interaction** - MCP protocol compliance testing

**Key Test Cases:**
- `test_complete_message_flow/1` - Full message routing validation
- `test_multi_transport_coordination/1` - Multiple transport integration
- `test_real_mcp_client_interaction/1` - Protocol compliance testing
- `test_tool_execution_end_to_end/1` - Tool execution validation
- `test_resource_access_end_to_end/1` - Resource handling validation

### 2. Load Tests (`erlmcp_load_SUITE.erl`)

Tests system behavior under realistic and extreme load conditions:

- **Concurrent Connections** - 1000+ simultaneous client connections
- **High Throughput** - 1000+ messages per second processing
- **Memory Stability** - Extended operation with memory monitoring
- **Burst Traffic** - Traffic spike handling and recovery
- **System Degradation** - Graceful degradation under extreme load

**Performance Targets:**
- Concurrent connections: 1000+
- Message throughput: 1000+ msg/sec
- Response time P95: <100ms
- Memory growth: <100MB over 5 minutes
- Success rate: >95%

**Key Test Cases:**
- `test_concurrent_connections_1000/1` - Large-scale connection testing
- `test_high_message_throughput/1` - Throughput validation
- `test_memory_stability_under_load/1` - Memory leak detection
- `test_graceful_degradation/1` - System limits and recovery

### 3. Validation Runner (`validation_runner.erl`)

Comprehensive test orchestration and reporting:

- **Test Suite Execution** - Automated test suite running
- **Performance Benchmarking** - Real-world performance measurement
- **Memory Leak Detection** - Long-running memory stability tests
- **System Health Validation** - Health monitoring integration testing
- **Report Generation** - Detailed validation reports

**Usage Examples:**

```erlang
% Run all tests
erl -pa ebin -s validation_runner run_all_tests

% Run performance validation only
erl -pa ebin -s validation_runner run_performance_validation

% Run with custom options
erl -pa ebin -s validation_runner run_with_options '[{skip_performance, true}]'
```

## Validation Shell Script (`run_validation.sh`)

User-friendly wrapper for running validation tests:

```bash
# Complete validation suite
./test/run_validation.sh --all

# Individual test types
./test/run_validation.sh --integration
./test/run_validation.sh --load
./test/run_validation.sh --performance
./test/run_validation.sh --memory
./test/run_validation.sh --health

# Quick validation (subset of tests)
./test/run_validation.sh --quick

# With verbose output
./test/run_validation.sh --verbose
```

## Production Readiness Criteria

The validation suite enforces these production readiness criteria:

### Functionality Requirements
- ✅ All integration tests pass
- ✅ Complete message flow works end-to-end
- ✅ Multiple transport types work simultaneously
- ✅ Configuration can be loaded and hot-reloaded
- ✅ System recovers gracefully from failures
- ✅ MCP protocol compliance is maintained

### Performance Requirements
- ✅ Handle 1000+ concurrent connections
- ✅ Process 1000+ messages per second
- ✅ Response times P95 < 100ms
- ✅ Memory growth < 100MB over 5 minutes
- ✅ Success rate > 95% under load

### Reliability Requirements
- ✅ No memory leaks during extended operation
- ✅ Graceful degradation under extreme load
- ✅ System health monitoring works correctly
- ✅ Recovery from component failures
- ✅ Supervisor hierarchy maintains stability

### Scalability Requirements
- ✅ Linear scalability with additional servers
- ✅ Transport isolation under load
- ✅ Registry performance under high registration load
- ✅ Resource cleanup after load scenarios

## Test Configuration

Key configuration constants used across the test suites:

```erlang
% Integration test configuration
-define(TEST_TIMEOUT, 30000).           % 30 second test timeout
-define(MESSAGE_BURST_SIZE, 100).       % Messages in burst tests
-define(CONCURRENT_CLIENTS, 50).        % Concurrent test clients

% Load test configuration
-define(MAX_CONCURRENT_CONNECTIONS, 1000).    % Maximum connections
-define(HIGH_THROUGHPUT_TARGET, 1000).        % Target msg/sec
-define(LOAD_TEST_DURATION, 300000).          % 5 minute load tests
-define(MEMORY_SAMPLE_INTERVAL, 5000).        % Memory sampling interval

% Performance thresholds
-define(MAX_RESPONSE_TIME_95TH, 100).          % 100ms P95 response time
-define(MAX_MEMORY_GROWTH_MB, 100).           % 100MB memory growth limit
-define(MIN_SUCCESS_RATE, 0.95).              % 95% minimum success rate
```

## Continuous Integration Integration

The validation suite is designed for CI/CD integration:

```yaml
# Example GitHub Actions integration
- name: Run ErlMCP Validation
  run: |
    cd erlmcp
    ./test/run_validation.sh --quick
```

**Exit Codes:**
- `0` - All validation tests passed
- `1` - One or more validation tests failed

**CI-Friendly Features:**
- Structured output with clear pass/fail indicators
- Configurable test timeouts
- Quick mode for faster CI execution
- Detailed error reporting and stack traces

## Troubleshooting

### Common Issues

**Test Timeouts:**
```bash
# Increase timeout for slow environments
export ERL_MAX_PORTS=32768
ulimit -n 4096
```

**Memory Issues:**
```bash
# Ensure adequate memory for load tests
export ERL_MAX_ETS_TABLES=32768
```

**Permission Issues:**
```bash
# Ensure script is executable
chmod +x ./test/run_validation.sh
```

### Performance Issues

If performance tests fail, check:

1. **System Resources** - Ensure adequate CPU, memory, and file descriptors
2. **Network Configuration** - Check localhost connectivity and port availability
3. **Erlang VM Settings** - Verify appropriate VM flags for performance
4. **Concurrent Limits** - Check system limits for processes and connections

### Test Development

Adding new validation tests:

1. **Integration Tests** - Add to `erlmcp_integration_SUITE.erl`
2. **Load Tests** - Add to `erlmcp_load_SUITE.erl`
3. **Custom Validation** - Extend `validation_runner.erl`
4. **Shell Integration** - Update `run_validation.sh`

Example new test case:

```erlang
test_new_feature_validation(Config) ->
    ct:pal("Testing new feature validation"),

    % Test setup
    ServerId = new_feature_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),

    % Test execution
    Result = test_new_feature(ServerId),

    % Validation
    ?assertEqual(expected_result, Result),

    % Cleanup
    ok = erlmcp:stop_server(ServerId),
    Config.
```

## Reporting and Monitoring

### Test Reports

Validation generates detailed reports including:

- **Test Execution Summary** - Pass/fail counts and timing
- **Performance Metrics** - Response times, throughput, resource usage
- **Memory Analysis** - Memory growth patterns and leak detection
- **System Health** - Component health and monitoring data
- **Error Details** - Stack traces and diagnostic information

### Monitoring Integration

The validation suite integrates with system monitoring:

- **Health Monitor** - Component health status tracking
- **Metrics Collection** - Performance and resource metrics
- **Tracing Integration** - OpenTelemetry span recording
- **Alert Generation** - Automated issue detection and reporting

## Best Practices

### Running Validation Tests

1. **Clean Environment** - Start with a clean system state
2. **Adequate Resources** - Ensure sufficient system resources
3. **Isolation** - Run tests in isolated environments when possible
4. **Consistent Conditions** - Use consistent hardware/software configurations
5. **Regular Execution** - Run validation regularly in CI/CD pipelines

### Interpreting Results

1. **Performance Trends** - Monitor performance trends over time
2. **Failure Patterns** - Analyze failure patterns for systemic issues
3. **Resource Usage** - Track resource usage patterns and growth
4. **Recovery Behavior** - Validate recovery mechanisms work correctly

### Maintenance

1. **Update Thresholds** - Adjust performance thresholds based on requirements
2. **Add Coverage** - Extend test coverage for new features
3. **Improve Diagnostics** - Enhance error reporting and diagnostics
4. **Optimize Performance** - Optimize test execution time while maintaining coverage

---

For questions or issues with the validation suite, please check the troubleshooting section above or consult the project documentation.
