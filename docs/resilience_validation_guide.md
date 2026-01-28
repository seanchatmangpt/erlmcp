# System Resilience Validation Guide

## 🛡️ Overview

The ErlMCP System Resilience Validation framework provides comprehensive testing and validation of system resilience under various failure conditions. This guide covers the complete resilience testing capabilities including failure recovery, degradation handling, chaos engineering, and real-world scenario simulations.

## 🎯 Validation Capabilities

### 1. Failure Recovery Testing
- **Automatic Reconnection**: Validates connection resilience and recovery timing
- **Failover Mechanisms**: Tests primary/backup service switching with data consistency
- **Circuit Breaker Validation**: Ensures proper circuit breaker operation and recovery
- **Retry Logic Testing**: Validates exponential backoff and retry strategies
- **Byzantine Fault Tolerance**: Tests consensus resilience with malicious nodes

### 2. System Degradation Testing
- **Graceful Degradation**: Validates service priority maintenance under load
- **Partial Failure Handling**: Tests system operation with component failures
- **Service Isolation**: Validates failure containment between service domains
- **Backpressure Mechanisms**: Tests flow control and queue management
- **Load Shedding**: Validates selective request dropping under pressure

### 3. Chaos Engineering
- **Process Chaos**: Random process termination and supervisor recovery
- **Network Chaos**: Packet loss, latency injection, bandwidth limiting
- **Resource Chaos**: Memory, CPU, and disk pressure testing
- **Cascade Failure Simulation**: Tests failure propagation prevention
- **Byzantine Chaos**: Consensus testing with Byzantine behavior injection

### 4. Advanced Failure Scenarios
- **Network Partition Recovery**: Split-brain prevention and data consistency
- **Cascade Failure Prevention**: Circuit breaker and bulkhead effectiveness
- **Memory Pressure Recovery**: Garbage collection and memory leak prevention
- **Concurrent Failure Recovery**: Multiple simultaneous failure handling
- **Security Under Stress**: Authentication and authorization under load

### 5. Real-World Simulations
- **Datacenter Outage**: Complete infrastructure failure with RTO/RPO measurement
- **Traffic Spike Simulation**: Black Friday-style load testing with auto-scaling
- **DDoS Attack Response**: Malicious traffic filtering and legitimate traffic protection
- **Hardware Degradation**: Progressive hardware failure simulation
- **Network Congestion**: Bandwidth constraints and quality of service testing

## 📊 Key Metrics and Validation

### Recovery Metrics
```erlang
test_resilience(Scenario) ->
    SpanCtx = otel_tracer:start_span(<<"resilience_test">>),

    % Inject failure
    inject_failure(Scenario),

    % Measure recovery
    RecoveryStart = erlang:monotonic_time(),
    wait_for_recovery(),
    RecoveryTime = erlang:monotonic_time() - RecoveryStart,

    otel_span:set_attributes(SpanCtx, [
        {<<"recovery.time_ms">>, RecoveryTime div 1000000},
        {<<"recovery.data_loss">>, measure_data_loss()},
        {<<"recovery.success">>, true}
    ]),

    otel_span:end_span(SpanCtx).
```

### Measured Metrics
- **Recovery Time**: Time to detect and recover from failures
- **Data Loss**: Percentage of data lost during failures
- **Availability**: System uptime during testing
- **Throughput Impact**: Performance degradation measurement
- **Error Rate**: Failure detection and handling effectiveness

## 🚀 Quick Start

### Running Complete Validation
```bash
# Run full resilience test suite
./scripts/run_resilience_validation.escript full

# Run quick essential tests
./scripts/run_resilience_validation.escript quick

# Run chaos engineering experiments only
./scripts/run_resilience_validation.escript chaos

# Run specific test
./scripts/run_resilience_validation.escript specific circuit_breaker

# Generate report from previous results
./scripts/run_resilience_validation.escript report
```

### Using Common Test Framework
```erlang
% Run complete test suite
ct:run_test([{suite, erlmcp_resilience_SUITE}]).

% Run specific test group
ct:run_test([{suite, erlmcp_resilience_SUITE},
             {group, failure_recovery_tests}]).

% Run individual test
ct:run_test([{suite, erlmcp_resilience_SUITE},
             {testcase, test_automatic_reconnection}]).
```

## 🔧 Test Implementation

### Example Test Structure
```erlang
test_automatic_reconnection(Config) ->
    SpanCtx = proplists:get_value(span_ctx, Config),

    ct:pal("Testing automatic reconnection mechanisms"),

    ReconnSpan = otel_tracer:start_span(<<"automatic_reconnection">>, SpanCtx),

    try
        %% Setup connection monitoring
        ConnectionPid = start_connection_monitor(),

        %% Inject network failure
        FailureStart = erlang:monotonic_time(),
        inject_network_failure(temporary_disconnect),

        otel_span:add_event(ReconnSpan, <<"network_failure_injected">>),

        %% Wait for automatic reconnection
        {ok, RecoveryTime} = wait_for_recovery(connection, ?RECOVERY_TIMEOUT),

        %% Validate reconnection success
        ?assert(is_connection_healthy(ConnectionPid)),

        %% Measure recovery metrics
        Metrics = measure_recovery_metrics(FailureStart, connection),

        otel_span:set_attributes(ReconnSpan, [
            {<<"recovery.time_ms">>, RecoveryTime},
            {<<"recovery.attempts">>, maps:get(attempts, Metrics)},
            {<<"recovery.success_rate">>, maps:get(success_rate, Metrics)},
            {<<"connection.stable">>, true}
        ]),

        ct:pal("Automatic reconnection validated: ~p ms recovery time",
               [RecoveryTime]),

        ok
    catch
        Class:Reason:Stacktrace ->
            otel_span:record_exception(ReconnSpan, Class, Reason, Stacktrace),
            otel_span:set_status(ReconnSpan, opentelemetry:status(error, <<"test_failed">>)),
            ct:fail({test_failed, Class, Reason})
    after
        otel_span:end_span(ReconnSpan)
    end.
```

## 🌪️ Chaos Engineering

### Creating Chaos Experiments
```erlang
create_network_chaos_experiment() ->
    chaos_engineering:create_chaos_experiment(<<"network_chaos">>, #{
        hypothesis => <<"System handles network instability">>,
        scenarios => [
            #{type => packet_loss, percentage => 10},
            #{type => network_delay, latency_ms => 1000},
            #{type => bandwidth_limit, mbps => 1}
        ],
        duration_ms => 45000,
        success_criteria => #{max_response_time => 5000}
    }).
```

### Running Chaos Experiments
```erlang
Experiment = create_network_chaos_experiment(),
Result = chaos_engineering:run_chaos_experiment(Experiment, Context),

case maps:get(success, Result) of
    true ->
        ResilienceScore = maps:get(resilience_score, Result),
        io:format("Chaos experiment passed: score ~.2f~n", [ResilienceScore]);
    false ->
        io:format("Chaos experiment failed: ~p~n", [maps:get(error, Result)])
end.
```

## 📈 Observability and Tracing

### OpenTelemetry Integration
All tests are fully instrumented with OpenTelemetry tracing:

- **Test Execution Spans**: Each test creates detailed execution traces
- **Failure Injection Events**: Precise timing of failure injection
- **Recovery Measurement**: Accurate recovery time tracking
- **Metrics Collection**: Comprehensive performance and resilience metrics
- **Error Tracking**: Detailed exception and error information

### Trace Example
```
resilience_test_session
├── failure_recovery_phase
│   ├── automatic_reconnection
│   │   ├── network_failure_injected (event)
│   │   ├── recovery_detected (event)
│   │   └── metrics: recovery_time_ms=1847
│   └── failover_mechanisms
│       ├── primary_service_failed (event)
│       ├── backup_activated (event)
│       └── metrics: failover_time_ms=1205, data_loss=0
└── chaos_engineering_phase
    └── network_chaos
        ├── packet_loss_injected (event)
        ├── system_degradation_detected (event)
        └── metrics: survival_rate=0.94, blast_radius=0.15
```

## 🎯 Validation Criteria

### Success Thresholds
- **Availability**: ≥ 99.0% during testing
- **Recovery Time**: ≤ 30 seconds for most scenarios
- **Data Loss**: ≤ 1% for any single failure
- **Throughput Impact**: ≥ 80% of baseline during degradation
- **Error Rate**: ≤ 5% during normal operation

### Resilience Scoring
```
Overall Score = (Availability × 0.25) +
                (Reliability × 0.25) +
                (Recoverability × 0.20) +
                (Robustness × 0.15) +
                (Adaptability × 0.15)
```

- **0.9-1.0**: Excellent resilience (production ready)
- **0.8-0.9**: Good resilience (acceptable with monitoring)
- **0.7-0.8**: Moderate resilience (needs improvement)
- **<0.7**: Poor resilience (requires significant work)

## 📋 Test Reports

### HTML Report Generation
The system generates comprehensive HTML reports including:
- Executive summary with resilience score
- Phase-by-phase breakdown
- Individual test results
- Failure pattern analysis
- Recovery time distributions
- Recommendations for improvement

### Report Contents
- **Executive Dashboard**: High-level resilience metrics
- **Detailed Results**: Per-test execution details
- **Trace Links**: Direct links to OpenTelemetry traces
- **Trend Analysis**: Comparison with previous runs
- **Action Items**: Specific recommendations for improvement

## 🔬 Testing Components

### Core Test Files
- `erlmcp_resilience_SUITE.erl`: Main Common Test suite
- `resilience_helpers.erl`: Helper functions for failure injection and measurement
- `chaos_engineering.erl`: Chaos engineering experiment framework
- `resilience_test_runner.erl`: Test orchestration and reporting

### Support Scripts
- `run_resilience_validation.escript`: Command-line test runner
- Test data generators and mock services
- Failure injection utilities
- Recovery verification functions

## 🛠️ Configuration

### Test Configuration
```erlang
-define(RECOVERY_TIMEOUT, 10000).
-define(MAX_RETRIES, 5).
-define(BACKOFF_BASE, 100).
-define(DEFAULT_TIMEOUT, 30000).
```

### Chaos Configuration
```erlang
ChaosConfig = #{
    max_concurrent_failures => 3,
    blast_radius_limit => 0.3,
    auto_abort_conditions => [
        {availability_below, 0.5},
        {data_loss_above, 0.1},
        {recovery_time_above, 300000}
    ]
}
```

## 🔒 Safety Measures

### Built-in Safety Controls
- **Blast Radius Limits**: Prevent system-wide damage
- **Auto-abort Conditions**: Stop tests if critical thresholds exceeded
- **Recovery Verification**: Ensure system returns to healthy state
- **Data Backup**: Protect against data loss during testing
- **Rollback Mechanisms**: Quick restoration if needed

### Testing Best Practices
1. **Start Small**: Begin with isolated component testing
2. **Gradual Escalation**: Increase failure complexity progressively
3. **Monitor Continuously**: Watch system health throughout testing
4. **Document Everything**: Maintain detailed test logs and traces
5. **Plan Recovery**: Always have rollback procedures ready

## 📚 Further Reading

- [Chaos Engineering Principles](https://principlesofchaos.org/)
- [Site Reliability Engineering](https://sre.google/)
- [Building Resilient Systems](https://www.oreilly.com/library/view/building-resilient-systems/9781492058847/)
- [OpenTelemetry Documentation](https://opentelemetry.io/docs/)

## 🤝 Contributing

To contribute to the resilience validation framework:

1. Add new test scenarios to the appropriate test suites
2. Implement additional chaos experiments in `chaos_engineering.erl`
3. Extend failure injection capabilities in `resilience_helpers.erl`
4. Improve observability and metrics collection
5. Add real-world simulation scenarios

## 📞 Support

For questions or issues with resilience validation:
- Review test logs and OpenTelemetry traces
- Check system health and resource utilization
- Validate test environment setup
- Consult troubleshooting guides
- Report bugs with full trace information

---

**🎯 Remember**: The goal is to prove the system can handle ANY failure scenario and recover gracefully. Every test should validate that the system remains operational and data remains consistent under adverse conditions.
