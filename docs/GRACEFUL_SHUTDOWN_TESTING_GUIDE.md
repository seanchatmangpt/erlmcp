# Graceful Shutdown Testing - Production Validation

## Overview

This document describes the comprehensive graceful shutdown testing implementation for erlmcp v3. The tests ensure zero-dropped requests during deployment and validate connection draining behavior under load.

## Test Architecture

### Test Components

1. **Common Test Suite** (`erlmcp_graceful_shutdown_SUITE.erl`)
   - 12 comprehensive test cases
   - Load testing simulation
   - Rolling update scenarios

2. **Load Test Script** (`test_graceful_shutdown_load.sh`)
   - Docker-based execution
   - 8 test phases
   - Production-like load simulation

3. **Validation Script** (`validate_graceful_shutdown.sh`)
   - Static analysis
   - prep_stop/1 verification
   - Helper function validation

### Test Coverage

| Test Case | Description | Validation |
|-----------|-------------|------------|
| `test_prep_stop_implementation` | Verifies prep_stop/1 across all apps | All apps export prep_stop/1 |
| `test_connection_draining_under_load` | 100 concurrent connections during shutdown | All connections drain properly |
| `test_zero_dropped_requests` | In-flight request completion | Zero requests dropped |
| `test_shutdown_timeout_enforcement` | Timeout with hung connections | Forced termination after timeout |
| `test_priority_shutdown_latency` | OTP 28 priority signal delivery | <10ms average latency |
| `test_concurrent_shutdown_signals` | Multiple concurrent shutdowns | All handled gracefully |
| `test_sigterm_handling` | Signal handling verification | SIGTERM processed correctly |
| `test_rolling_update_simulation` | 3-node rolling update | All nodes terminate properly |
| `test_pool_draining` | Connection pool draining | Pools drain completely |
| `test_transport_stop_accepting` | Transport supervisor behavior | New connections rejected |
| `test_registry_graceful_shutdown` | Registry shutdown sequence | Registry terminates cleanly |
| `test_observability_flush` | Telemetry flush on shutdown | OTEL data flushed |

## Implementation Details

### prep_stop/1 Callbacks

All three main applications implement `prep_stop/1`:

#### erlmcp_app
```erlang
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_core for graceful shutdown"),
    try
        case whereis(erlmcp_registry) of
            undefined -> ok;
            _Pid -> erlmcp_registry:graceful_shutdown()
        end
    catch _:_ -> ok
    end,
    State.
```

#### erlmcp_transports_app
```erlang
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_transports for graceful shutdown"),
    try
        Pid = whereis(erlmcp_transport_sup),
        case Pid of
            undefined -> ok;
            _ -> erlmcp_transport_sup:stop_accepting()
        end
    catch _:_ -> ok
    end,
    try
        PoolPid = whereis(erlmcp_connection_pool),
        case PoolPid of
            undefined -> ok;
            _ -> erlmcp_connection_pool:drain()
        end
    catch _:_ -> ok
    end,
    State.
```

#### erlmcp_observability_app
```erlang
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_observability for graceful shutdown"),
    try
        case application:get_env(erlmcp_observability, otel_enabled, true) of
            true -> erlmcp_otel:flush();
            false -> ok
        end
    catch _:_ -> ok
    end,
    State.
```

### Graceful Drain Service

The `erlmcp_graceful_drain` gen_server manages connection draining:

- **Connection Tracking**: Monitors active connections via `connection_started/1` and `connection_finished/1`
- **Shutdown Initiation**: Priority-based shutdown signals (OTP 28)
- **Timeout Enforcement**: Forced termination after timeout expires
- **Metrics Collection**: Tracks priority message latency

### Connection Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Stop Request                      │
│                          (SIGTERM/SIGINT)                        │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                    application:stop(State)                       │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                    prep_stop(State) [NEW!]                       │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ 1. Registry: graceful_shutdown()                           │ │
│  │ 2. Transport: stop_accepting()                             │ │
│  │ 3. Connection Pool: drain()                                │ │
│  │ 4. Observability: flush()                                  │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│              Graceful Drain Service (erlmcp_graceful_drain)       │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ • Set shutdown_requested = true                            │ │
│  │ • Reject new connections                                   │ │
│  │ • Track active connections                                 │ │
│  │ • Wait for drain or timeout                                │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                ┌─────────────┴─────────────┐
                │                           │
                v                           v
    ┌───────────────────────┐   ┌──────────────────────┐
    │  All Connections     │   │   Timeout Expires    │
    │  Drained             │   │   (Force Shutdown)   │
    └───────────┬───────────┘   └───────────┬──────────┘
                │                           │
                └─────────────┬─────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                    Normal Termination                            │
└─────────────────────────────────────────────────────────────────┘
```

## Running Tests

### Via Docker Compose (Recommended)

```bash
# Run full graceful shutdown load test suite
docker compose run --rm erlmcp-shutdown

# Run specific test
docker compose run --rm erlmcp-shutdown make test-shutdown

# Run validation only
docker compose run --rm erlmcp-shutdown make validate-shutdown
```

### Via Makefile

```bash
# From within Docker container
make test-shutdown          # Run load tests
make validate-shutdown      # Run static validation
make shutdown-load-test     # Alias for test-shutdown
```

### Direct Script Execution

```bash
# From within Docker container
./scripts/test_graceful_shutdown_load.sh
./scripts/validate_graceful_shutdown.sh
```

## Test Results Interpretation

### Success Criteria

| Metric | Threshold | Description |
|--------|-----------|-------------|
| Dropped Requests | 0 | Zero requests lost during shutdown |
| Shutdown Latency | <10ms (avg) | Priority signal processing time |
| Connection Drain | 100% | All connections complete or timeout |
| Timeout Enforcement | <timeout+1000ms | Forced termination occurs |

### Example Output

```
======================================================================
GRACEFUL SHUTDOWN LOAD TEST SUMMARY
======================================================================

Test Results:
  1. prep_stop/1 Implementation:  PASS
  2. Compilation:                 PASS
  3. Unit Tests:                  PASS
  4. Common Test Suite:           PASS
  5. Load During Shutdown:        PASS
  6. Zero Dropped Requests:       PASS
  7. Priority Latency (OTP 28):   PASS
  8. Rolling Update Simulation:   PASS

Result: PASSED

All graceful shutdown tests passed successfully!
The system is ready for production deployment.
======================================================================
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `LOAD_TEST_DURATION` | 30 | Load test duration (seconds) |
| `CONCURRENT_REQUESTS` | 50 | Number of concurrent requests |
| `SHUTDOWN_TIMEOUT` | 5000 | Shutdown timeout (ms) |
| `ERLMCP_SHUTDOWN_CPU_LIMIT` | 2.0 | Docker CPU limit |
| `ERLMCP_SHUTDOWN_MEMORY_LIMIT` | 2G | Docker memory limit |

## Troubleshooting

### Test Failures

1. **Compilation Errors**
   - Check that all applications have `prep_stop/1` exported
   - Verify rebar3 compile succeeds

2. **Timeout Failures**
   - Increase `SHUTDOWN_TIMEOUT` for slow systems
   - Check for hung connections/processes

3. **Dropped Requests**
   - Verify connection tracking is working
   - Check that new connections are rejected during drain

### Debug Mode

```bash
# Enable debug logging
export ERLMCP_LOG_LEVEL=debug
docker compose run --rm erlmcp-shutdown

# Check logs
docker compose logs erlmcp-shutdown
```

## Production Deployment Checklist

Before deploying to production:

- [ ] All graceful shutdown tests pass
- [ ] Zero dropped requests validated
- [ ] Connection draining verified
- [ ] Timeout enforcement tested
- [ ] Rolling update simulation successful
- [ ] SIGTERM handling confirmed
- [ ] Telemetry flush working

## References

- `apps/erlmcp_validation/test/erlmcp_graceful_shutdown_SUITE.erl` - CT test suite
- `scripts/test_graceful_shutdown_load.sh` - Load test script
- `scripts/validate_graceful_shutdown.sh` - Validation script
- `apps/erlmcp_core/src/erlmcp_graceful_drain.erl` - Drain service implementation
- `apps/erlmcp_core/src/erlmcp_app.erl` - Core app with prep_stop/1
- `apps/erlmcp_transports/src/erlmcp_transports_app.erl` - Transports app with prep_stop/1
- `apps/erlmcp_observability/src/erlmcp_observability_app.erl` - Observability app with prep_stop/1
