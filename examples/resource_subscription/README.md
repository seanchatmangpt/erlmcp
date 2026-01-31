# Resource Subscription Integration Example

## Overview

This example demonstrates the MCP resource subscription feature, allowing clients to receive notifications when resources change.

## Features Demonstrated

1. **Starting Resource Subscriptions Manager** - Managing subscription state
2. **Subscribing to Resources** - Registering interest in specific URIs
3. **Resource Change Notifications** - Receiving `resources/updated` messages
4. **Rate Limiting** - Controlling notification frequency
5. **Unsubscribing** - Removing subscriptions

## Prerequisites

- Erlang/OTP 25+
- rebar3 build system
- erlmcp_core and erlmcp_transports applications

## Running the Example

### Method 1: Using escript (Recommended)

```bash
# From the erlmcp root directory
escript examples/resource_subscription/example.erl
```

### Method 2: Using rebar3 shell

```bash
# Start the Erlang shell
rebar3 shell

# Run the example
c("examples/resource_subscription/example.erl").
example:main([]).

# Or compile and run directly
erlc -o examples/resource_subscription/ examples/resource_subscription/example.erl
erl -pa examples/resource_subscription/ -eval "example:main([])" -s init stop
```

### Method 3: Interactive testing

```bash
# Start the Erlang shell
rebar3 shell

# Start applications
application:ensure_all_started(erlmcp_core).
application:ensure_all_started(erlmcp_transports).

# Start resource subscriptions manager
{ok, Pid} = erlmcp_resource_subscriptions:start_link().

# Create a mock server
ServerPid = spawn(fun() -> receive X -> X end end).

# Subscribe to a resource
erlmcp_resource_subscriptions:subscribe_to_resource(
    <<"test://resource">>,
    ServerPid,
    #{rate_limit => 1000}
).

# Trigger a change
erlmcp_resource_subscriptions:notify_resource_changed(
    <<"test://resource">>,
    #{reason => manual_trigger}
).

# Check stats
erlmcp_resource_subscriptions:get_stats().

# Cleanup
erlmcp_resource_subscriptions:stop().
```

## Expected Output

```
=== Resource Subscription Integration Example ===

Step 1: Starting erlmcp applications...
✓ Applications started

Step 2: Starting resource subscriptions manager...
✓ Resource subscriptions manager started: <0.123.0>

Step 3: Creating mock server to receive notifications...
✓ Mock server started: <0.124.0>

Step 4: Starting erlmcp_server with resources...
✓ Server started with 2 resources

Step 5: Subscribing to resource changes...
✓ Subscribed to example://config
✓ Subscribed to example://counter
✓ Total resources: 2, Total subscriptions: 2

Step 6: Triggering resource changes...
Waiting for notifications...

  [NOTIFICATION] Resource example://config changed at 1706630400000
  [NOTIFICATION] Resource example://counter changed at 1706630400500

Step 7: Checking received notifications...
✓ Received 2 notifications
  - Resource changed: example://config
  - Resource changed: example://counter

Step 8: Listing subscribers for resource...
✓ Subscribers to example://config: 1

Step 9: Unsubscribing from resource...
✓ Unsubscribed from example://config

Step 10: Demonstrating rate limiting...
Setting rate limit to 2000ms (2 seconds)
Triggering 5 rapid changes...
✓ Rate limiting active - only first notification delivered

Step 11: Cleaning up...
✓ Cleanup complete

=== Example Complete ===
```

## Key Concepts

### Resource Subscriptions Manager

The `erlmcp_resource_subscriptions` gen_server manages:
- **Subscriptions** - Maps resource URIs to subscriber pids
- **Rate Limiting** - Prevents notification spam (default: 1/sec)
- **Batching** - Groups rapid changes within 100ms window
- **Monitoring** - Automatic cleanup of dead subscribers

### Subscription Lifecycle

```
1. subscribe_to_resource(Uri, Pid, Options)
   - Validates subscriber (local, alive)
   - Monitors subscriber process
   - Adds to subscription map

2. notify_resource_changed(Uri, Metadata)
   - Queues change notification
   - Applies rate limiting
   - Batches rapid changes

3. Notification Delivery
   - Sends {'$mcp_resource', Notification} message
   - Contains: jsonrpc, method, params (uri, timestamp)

4. unsubscribe_from_resource(Uri, Pid)
   - Removes from subscription map
   - Demonitor process
```

### Notification Message Format

```erlang
{'$mcp_resource', #{
    jsonrpc => <<"2.0">>,
    method => <<"resources/updated">>,
    params => #{
        uri => <<"example://config">>,
        timestamp => 1706630400000
    }
}}
```

## Advanced Usage

### Custom Rate Limits per Resource

```erlang
% Set 5-second rate limit for specific resource
erlmcp_resource_subscriptions:set_rate_limit(<<"my://resource">>, 5000).
```

### Subscription Filters (Future)

```erlang
% Filter notifications by metadata (not yet implemented)
erlmcp_resource_subscriptions:subscribe_to_resource(
    <<"my://resource">>,
    ServerPid,
    #{
        filter => fun(Metadata) ->
            maps:get(change_type, Metadata) =:= important
        end
    }
).
```

### URI Templates (Future)

```erlang
% Subscribe to all resources matching pattern
erlmcp_resource_subscriptions:subscribe_to_resource(
    <<"logs://*">>,  % Wildcard pattern
    ServerPid,
    #{}
).
```

## Troubleshooting

### "Resource subscriptions manager not running"

**Solution**: Start the manager before subscribing:
```erlang
erlmcp_resource_subscriptions:start_link().
```

### "No notifications received"

**Possible causes**:
1. Subscriber process not listening for `$mcp_resource` messages
2. Resource not actually triggering `notify_resource_changed/2`
3. Rate limiting blocking notifications (check timestamp)

**Debug**:
```erlang
% Check subscription stats
Stats = erlmcp_resource_subscriptions:get_stats().
maps:get(total_subscriptions, Stats).

% List subscribers
erlmcp_resource_subscriptions:list_resource_subscriptions(
    <<"my://resource">>,
    false
).
```

### "Rate limit exceeded"

**Expected behavior**: This is not an error. Rapid changes are rate-limited to prevent notification spam.

**Adjust rate limit**:
```erlang
% Lower rate limit (more aggressive throttling)
erlmcp_resource_subscriptions:set_rate_limit(<<"my://resource">>, 5000).

% Disable rate limiting (0 = unlimited)
erlmcp_resource_subscriptions:set_rate_limit(<<"my://resource">>, 0).
```

## Integration with MCP Protocol

This example follows the MCP 2025-11-25 specification for resource subscriptions:

- **Method**: `resources/updated` notification
- **Params**: `uri` (required), `timestamp` (optional)
- **Transport**: Sent via same transport as client connection
- **Reliability**: Best-effort delivery (no ack)

## Files

- `example.erl` - Main example script
- `README.md` - This file

## See Also

- [MCP Protocol Specification](../../../docs/protocol.md)
- [Resource Subscriptions Module](../../../apps/erlmcp_core/src/erlmcp_resource_subscriptions.erl)
- [Server Module](../../../apps/erlmcp_core/src/erlmcp_server.erl)
