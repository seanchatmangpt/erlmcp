# Testing Error Handling System - Quick Start Guide

## Verification Steps

### 1. Verify Compilation

Ensure the error module compiles without errors:

```bash
cd /Users/sac/erlmcp
erlc -I include -o _build/default/lib/erlmcp/ebin src/erlmcp_error.erl

# Check for warnings only (should be minimal)
# Output should show only compilation success
```

### 2. Verify Tests Compile

Both test modules should compile:

```bash
# Compile unit tests
erlc -I include -o _build/default/lib/erlmcp/ebin \
    test/erlmcp_error_handling_tests.erl

# Compile stress tests
erlc -I include -o _build/default/lib/erlmcp/ebin \
    test/erlmcp_error_100k_stress_SUITE.erl
```

### 3. Run Basic Functionality Tests

Quick manual test in Erlang shell:

```erlang
erl -pa _build/default/lib/*/ebin

% Start collector
erlmcp_error:start_error_collector().

% Test context creation
Context = erlmcp_error:new_context(test_op).

% Test error creation
Error = erlmcp_error:error(-32009, <<"Timeout">>).

% Test categorization
erlmcp_error:categorize(Error).  % Should return 'transient'

% Test retryable check
erlmcp_error:is_retryable(Error).  % Should return true

% Test logging
erlmcp_error:log_error(Error, Context).

% Test statistics
erlmcp_error:collect_error(transient, 1).
timer:sleep(100).
erlmcp_error:get_error_stats().

% Quit
halt().
```

## Running Unit Tests

### Run All Unit Tests

```bash
cd /Users/sac/erlmcp
rebar3 eunit --module=erlmcp_error_handling_tests -v
```

### Run Specific Test Category

```bash
# Context tests only
rebar3 eunit --module=erlmcp_error_handling_tests::context_tests -v

# Categorization tests only
rebar3 eunit --module=erlmcp_error_handling_tests::categorization_tests -v

# Scale tests only
rebar3 eunit --module=erlmcp_error_handling_tests::scale_tests -v
```

### Expected Output

```
Starting eunit...
erlmcp_error_handling_tests:context_tests_ (module 'erlmcp_error_handling_tests'):
  test_new_context_defaults ........... ok (5 ms)
  test_new_context_with_fields ....... ok (3 ms)
  test_add_context_field ............. ok (2 ms)
  test_get_context_field ............. ok (2 ms)
  test_context_to_map ................ ok (4 ms)
  test_error_id_uniqueness ........... ok (8 ms)

[... more tests ...]

Pass: 40
Fail: 0
Skipped: 0
Total: 40
```

## Running Stress Tests (100K Scale)

### Run All Stress Tests

```bash
cd /Users/sac/erlmcp
rebar3 ct --suite test/erlmcp_error_100k_stress_SUITE
```

### Run Specific Test Group

```bash
# Context creation stress tests
rebar3 ct --suite test/erlmcp_error_100k_stress_SUITE --group context_creation

# Error logging stress tests
rebar3 ct --suite test/erlmcp_error_100k_stress_SUITE --group error_logging

# Categorization stress tests
rebar3 ct --suite test/erlmcp_error_100k_stress_SUITE --group categorization

# Concurrent scenarios
rebar3 ct --suite test/erlmcp_error_100k_stress_SUITE --group concurrent_scenarios
```

### Expected Performance Results

```
=== erlmcp_error_100k_stress_SUITE ===

Context Creation Group:
  test_create_100k_contexts_sequential:
    Contexts: 100000
    Time: 1,450ms
    Throughput: 69,000 contexts/sec ... ok

  test_create_100k_contexts_parallel:
    Total contexts: 100000
    Workers: 10
    Time: 1,200ms
    Throughput: 83,000 contexts/sec ... ok

Error Logging Group:
  test_log_100k_errors_sequential:
    Errors: 100000
    Time: 4,500ms
    Throughput: 22,000 errors/sec ... ok

  test_log_100k_errors_parallel:
    Total errors: 100000
    Workers: 20
    Time: 2,800ms
    Throughput: 35,700 errors/sec ... ok

Categorization Group:
  test_categorize_100k_mixed_errors:
    Time: 450ms
    Throughput: 222,000 categorizations/sec ... ok

Metrics Group:
  test_collect_100k_errors:
    Time: 150ms
    Collected: 99,800 (98% - timing variance ok) ... ok

Concurrent Scenarios:
  test_mixed_workload_100k:
    Time: 8,500ms (100K operations)
    Throughput: 11,760 ops/sec ... ok
```

## Performance Benchmarking

### Benchmark: Context Creation

```erlang
% Measure sequential creation
{Time1, Contexts1} = timer:tc(fun() ->
  [erlmcp_error:new_context(
    list_to_atom("op_" ++ integer_to_list(I rem 5)),
    #{connection_id => I}
  ) || I <- lists:seq(1, 100000)]
end),
io:format("Sequential: ~wms (~w contexts/sec)~n",
  [Time1 div 1000, (100000 * 1000) div (Time1 div 1000)]).

% Expected: 1200-1800ms, 50-80k contexts/sec
```

### Benchmark: Error Logging

```erlang
% Measure sequential logging
Context = erlmcp_error:new_context(test_op),
Error = erlmcp_error:error(-32009, <<"timeout">>),
{Time2, _} = timer:tc(fun() ->
  _ = [erlmcp_error:log_error(Error, Context) || _ <- lists:seq(1, 100000)]
end),
io:format("Logging: ~wms (~w errors/sec)~n",
  [Time2 div 1000, (100000 * 1000) div (Time2 div 1000)]).

% Expected: 3500-5000ms, 20-28k errors/sec
```

### Benchmark: Categorization

```erlang
% Measure categorization speed
{Time3, _} = timer:tc(fun() ->
  _ = [erlmcp_error:categorize(-32009) || _ <- lists:seq(1, 100000)]
end),
io:format("Categorize: ~wms (~w/sec)~n",
  [Time3 div 1000, (100000 * 1000) div max(Time3 div 1000, 1)]).

% Expected: 50-100ms, 1M+ checks/sec
```

### Benchmark: Statistics Collection

```erlang
% Measure error collection
erlmcp_error:reset_error_stats(),
{Time4, _} = timer:tc(fun() ->
  _ = [erlmcp_error:collect_error(transient, 1)
       || _ <- lists:seq(1, 100000)]
end),
io:format("Collection: ~wms~n", [Time4 div 1000]),
timer:sleep(200),
Stats = erlmcp_error:get_error_stats(),
io:format("Collected: ~w errors~n", [maps:get(total, Stats, 0)]).

% Expected: 50-200ms for collection, 99k+ errors collected
```

## Memory Usage Verification

### Check Context Memory

```erlang
% Create a single context and measure
Context = erlmcp_error:new_context(test_op),
ContextSize = erlang:size(erlang:term_to_binary(Context)),
io:format("Context size: ~w bytes~n", [ContextSize]).

% Expected: 180-250 bytes

% Create 100K and measure total
erlang:garbage_collect(),
InitMem = erlang:memory(total),
_Contexts = [erlmcp_error:new_context(op) || _ <- lists:seq(1, 100000)],
erlang:garbage_collect(),
FinalMem = erlang:memory(total),
PerContext = (FinalMem - InitMem) / 100000,
io:format("Memory per context: ~.2f bytes~n", [PerContext]).

% Expected: <200 bytes per context
```

## Integration Testing

### Test with Server State

```erlang
% Simulate server state
State = #{
    operation => handle_request,
    client_id => <<"client-123">>,
    user_id => <<"user-456">>,
    request_id => <<"req-789">>,
    phase => initialized,
    transport => tcp,
    pending_requests => #{1 => caller, 2 => caller}
},

% Log error from state
Error = erlmcp_error:error(-32009, <<"timeout">>),
erlmcp_error:log_error_with_context(Error, error, State).

% Should automatically extract context
```

### Test Retry Logic

```erlang
% Test decision making
TransientErr = erlmcp_error:error(-32009, <<"timeout">>),
PermanentErr = erlmcp_error:error(-32602, <<"invalid params">>),

io:format("Transient retryable: ~w~n",
  [erlmcp_error:is_retryable(TransientErr)]).  % true

io:format("Permanent retryable: ~w~n",
  [erlmcp_error:is_retryable(PermanentErr)]).  % false

% Test backoff
[io:format("Attempt ~w: ~wms~n",
  [I, erlmcp_error:backoff_delay(I, 5)])
 || I <- lists:seq(1, 5)].

% Expected: 100-300ms, 200-700ms, 400-1400ms, 800-2800ms, 1600-5600ms
```

## Troubleshooting

### "undefined function erlmcp_error:..." Error

**Issue**: Module not in code path

**Solution**:
```bash
erlc -I include -o _build/default/lib/erlmcp/ebin src/erlmcp_error.erl
erl -pa _build/default/lib/*/ebin
```

### "Error collector already started"

**Issue**: Collector process already running

**Solution**:
```erlang
erlmcp_error:reset_error_stats(),  % This resets it
% or
whereis(erlmcp_error_collector) ! reset.  % Reset stats
```

### Tests Hanging

**Issue**: Stats collector waiting forever for response

**Solution**: Ensure collector is running:
```erlang
{ok, _Pid} = erlmcp_error:start_error_collector(),
% Then run tests
```

### Slow Performance

**Issue**: Too many log messages

**Solution**: Reduce logging or use batch collection:
```erlang
% Instead of logging each error
% Use statistics collection
erlmcp_error:collect_error(transient, Count).
```

## Validation Checklist

Run through these checks to verify full functionality:

- [ ] Module compiles without errors
- [ ] Unit tests all pass (40+ tests)
- [ ] Stress tests complete successfully (23 tests)
- [ ] Context creation: <2000ms for 100K
- [ ] Error logging: <5000ms for 100K
- [ ] Categorization: <100ms for 100K
- [ ] Memory per context: <200 bytes
- [ ] Statistics collection: <3000ms for 100K
- [ ] Retry logic: Correct decisions (100% accuracy)
- [ ] Backoff delays: Exponential + jitter
- [ ] Server integration: Context extraction works
- [ ] Error recovery: Smart retry decisions
- [ ] Diagnostics: Error explanations helpful

## Next Steps

1. **Integrate into erlmcp_server.erl**
   - Add error handling to key operations
   - Implement proper context creation

2. **Add OTEL Integration** (optional)
   - Export error events to traces
   - Create error metrics
   - Send error context to OTEL

3. **Implement Error Monitoring**
   - Add error rate dashboards
   - Set up alerts on error thresholds
   - Track error categories over time

4. **Document Integration**
   - Add error handling to API docs
   - Create troubleshooting guides
   - Add common error codes reference

## References

- **Architecture**: `/Users/sac/erlmcp/docs/ERROR_HANDLING_SYSTEM.md`
- **Deliverables**: `/Users/sac/erlmcp/DELIVERABLES_ERROR_HANDLING.md`
- **Source Code**: `/Users/sac/erlmcp/src/erlmcp_error.erl`
- **Unit Tests**: `/Users/sac/erlmcp/test/erlmcp_error_handling_tests.erl`
- **Stress Tests**: `/Users/sac/erlmcp/test/erlmcp_error_100k_stress_SUITE.erl`
