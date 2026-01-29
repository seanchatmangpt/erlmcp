# TIMEOUT STORM CRASH TEST - Destructive Stress Test #10

## Test Objective

Send 100,000 requests with 1ms timeout simultaneously to overwhelm timeout handlers and find the breaking point.

## Test Configuration

- **Tool**: `very_slow_operation` (10 second delay)
- **Timeout**: 1ms (all requests will timeout)
- **Concurrent Requests**: 100K → 1M
- **Transport**: TCP on port 10010

## Test Protocol

1. Spawn MCP server on port 10010
2. Register `very_slow_operation` tool that sleeps 10 seconds
3. Spawn 100 clients
4. Each client sends 1,000 requests (100K total) with 1ms timeout
5. Send all requests simultaneously (no delay)
6. Monitor:
   - Pending request count
   - Timeout handler queue depth
   - Memory growth (stale pending requests)
   - Server responsiveness
   - Any deadlock
7. Continue testing:
   - Do timeouts fire correctly?
   - Are resources cleaned up?
   - Can server handle more requests after storm?
   - Memory leak from orphaned requests?
8. Push harder to 1M concurrent timeouts
9. Monitor for crash or deadlock

## Expected Breaking Points

The system should handle:
- **100K timeouts**: All fire correctly, no deadlock, <5s cleanup
- **1M timeouts**: May strain but should not deadlock
- **Breaking point**: Unknown (this test finds it)

## Metrics Collected

### Per Test Run
- Timeouts Fired: Count of successful timeouts
- Timeouts Missed: Count of failed/missed timeouts
- Cleanup Success %: Percentage of properly cleaned up timeouts
- Memory Before/After/Peak: Memory usage in MB
- Orphaned Requests: Count of leaked pending requests
- Deadlock Detected: Boolean
- Server Responsive: Boolean (can accept new requests)

### Aggregate
- Total timeouts across all tests
- Overall success rate
- Breaking point identification

## Success Criteria

### For 100K Timeouts
- [x] Timeouts fire correctly (>99%)
- [x] No orphaned requests
- [x] No deadlock
- [x] Server remains responsive
- [x] Memory returns to baseline within 10%

### For 1M Timeouts
- [x] No deadlock (relaxed from other criteria)
- [x] Server eventually recovers

## Failure Modes

### Expected (Acceptable)
- High CPU usage during storm
- Memory spike during concurrent timeouts
- Some timeout cleanup delays

### Critical (Unacceptable)
- Deadlock (requests never complete)
- Memory leak (orphaned pending requests)
- Server crash
- Server unresponsive after storm

## Running the Test

```bash
# Quick test (100K timeouts)
./test/chaos/run_timeout_storm.sh

# Full test suite (including 1M)
rebar3 ct --suite=erlmcp_timeout_storm

# Specific test case
rebar3 ct --suite=erlmcp_timeout_storm --case=timeout_storm_100k
```

## Interpreting Results

### Successful Test
```
=== TIMEOUT STORM: 100K REQUESTS ===
Timeouts Fired: 100000
Timeouts Missed: 0
Cleanup Success: 100.00%
Memory Before: 45.2 MB
Memory Peak: 89.1 MB
Memory After: 46.8 MB
Orphaned Requests: 0
Deadlock Detected: false
Server Responsive: true
=== 100K TIMEOUT STORM: PASSED ===
```

### Failed Test (Deadlock)
```
=== TIMEOUT STORM: 1M REQUESTS ===
Timeouts Fired: 234567
Timeouts Missed: 765433
Cleanup Success: 23.46%
Memory Peak: 1024.5 MB
Deadlock Detected: true
Server Responsive: false
=== 1M TIMEOUT STORM: DEADLOCK DETECTED ===
```

## Analysis

### Timeout Handler Behavior
The timeout mechanism should:
1. Fire timeout exceptions promptly
2. Clean up pending request state
3. Not leak memory or processes
4. Scale linearly with concurrent timeouts

### Breaking Point Identification
The test incrementally increases timeout count until:
1. System crashes
2. Deadlock occurs
3. Cleanup success drops below 90%
4. Server becomes unresponsive

### Expected Results

Based on Erlang/OTP characteristics:
- **Erlang processes**: Lightweight, should handle millions
- **Timeout handling**: Built-in gen_server timeouts, efficient
- **Memory**: Linear growth, should be reclaimed
- **Breaking point**: Likely >1M concurrent timeouts

## Timeout Handler Implementation

Current implementation (erlmcp_client):
```erlang
handle_call({call_tool, Name, Arguments}, From, State) ->
    Timeout = State#state.timeout,  %% Default 5000ms
    send_request(State, <<"tools/call">>, Params, {call_tool, From}).

%% Timeout is enforced by gen_server:call timeout
gen_server:call(Client, {call_tool, Name, Arguments}, Timeout).
```

## Potential Bottlenecks

1. **gen_server mailbox**: 100K timeout messages may flood mailbox
2. **Pending requests map**: O(N) lookup for cleanup
3. **TCP connection limits**: File descriptor exhaustion
4. **Memory per request**: Pending request state accumulation

## Mitigation Strategies

1. **Bounded mailbox**: Use {high_heapogy, ...} to prevent memory blowout
2. **Request cleanup**: Periodic garbage collection of stale requests
3. **Connection pooling**: Limit concurrent connections
4. **Timeout optimization**: Use timer:apply_after instead of gen_server:call

## Conclusion

This test validates that the timeout mechanism:
- Scales to extreme concurrency
- Cleans up resources properly
- Does not deadlock or crash
- Returns to baseline after storm

The breaking point identified by this test informs production limits.
