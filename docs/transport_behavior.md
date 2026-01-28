# Transport Behavior Documentation

## Overview

The ErlMCP transport layer provides a pluggable architecture for different communication protocols. All transports implement the `erlmcp_transport` behavior, which defines a consistent interface for sending and receiving messages.

## Transport Behavior Interface

The transport behavior defines the following callbacks:

### Required Callbacks

#### `init/1`
```erlang
-callback init(Opts :: transport_opts()) ->
    {ok, State :: transport_state()} |
    {error, Reason :: term()}.
```

**Purpose**: Initialize the transport with the provided options.

**Parameters**:
- `Opts`: Transport-specific configuration options

**Returns**:
- `{ok, State}`: Successful initialization with transport state
- `{error, Reason}`: Initialization failed with reason

#### `send/2`
```erlang
-callback send(State :: transport_state(), Data :: iodata()) ->
    ok |
    {error, Reason :: term()}.
```

**Purpose**: Send data through the transport.

**Parameters**:
- `State`: Current transport state (from init or previous operations)
- `Data`: Binary data or iolist to send

**Returns**:
- `ok`: Data sent successfully
- `{error, Reason}`: Send operation failed

### Optional Callbacks

#### `close/1`
```erlang
-callback close(State :: transport_state()) -> ok.
```

**Purpose**: Close the transport connection and clean up resources.

**Parameters**:
- `State`: Current transport state

**Returns**:
- `ok`: Transport closed successfully

**Note**: This callback is optional. If not implemented, the transport is assumed to be stateless or self-managing.

## Standard Behavior Callbacks

All transport implementations should follow these behavioral patterns:

### Connection Management
- **Initialization**: Establish connection and prepare for message transmission
- **Persistence**: Maintain connection state across multiple send operations
- **Cleanup**: Properly close connections and free resources when done

### Error Handling
- **Graceful Degradation**: Handle temporary failures without crashing
- **Error Reporting**: Return descriptive error tuples for debugging
- **Retry Logic**: Implement appropriate retry mechanisms for transient failures

### Message Framing
- **Protocol Compliance**: Follow the expected message format for the transport type
- **Line-Based**: For stdio and TCP transports, ensure messages are newline-terminated
- **HTTP Framing**: For HTTP transports, use appropriate headers and body encoding

## Transport State Management

Transport state should include:

### Required State Elements
- Connection information (sockets, handles, etc.)
- Configuration options
- Buffer management for incomplete messages

### Recommended State Elements
- Connection status flags
- Retry counters and backoff timers
- Message queues for async operations
- Statistics and monitoring data

## Types

### `transport_state()`
```erlang
-type transport_state() :: term().
```
Opaque state maintained by the transport implementation. Can be any Erlang term but typically a record or map containing connection state.

### `transport_opts()`
```erlang
-type transport_opts() :: term().
```
Transport-specific options passed to `init/1`. Structure varies by transport type.

## Implementation Guidelines

### Memory Management
- Avoid memory leaks in long-running connections
- Implement proper cleanup in termination scenarios
- Use appropriate buffer sizes to prevent memory exhaustion

### Concurrency
- Support concurrent send operations if the underlying protocol allows
- Implement proper synchronization for shared state
- Consider using gen_server or similar OTP behaviors for complex transports

### Monitoring
- Log important events (connections, disconnections, errors)
- Provide metrics for monitoring system health
- Support graceful shutdown procedures

### Testing
- Implement test mode detection to avoid blocking in unit tests
- Provide simulation capabilities for testing scenarios
- Mock external dependencies for isolated testing

## Error Codes

Common error reasons returned by transport implementations:

- `not_connected`: Transport is not ready to send data
- `connection_lost`: Connection was unexpectedly terminated
- `timeout`: Operation timed out
- `invalid_data`: Data format is not supported by transport
- `transport_error`: Generic transport-level error
- `{protocol_error, Reason}`: Protocol-specific error

## Best Practices

### Configuration
- Provide sensible defaults for all optional parameters
- Validate configuration options in `init/1`
- Support both simple and advanced configuration formats

### Reliability
- Implement connection pooling for high-throughput scenarios
- Use exponential backoff for retry mechanisms
- Provide circuit breaker patterns for failing connections

### Performance
- Minimize data copying and conversion overhead
- Use binary data types for efficiency
- Implement batching for multiple small messages

### Debugging
- Include detailed error information in failure returns
- Log connection state changes and important events
- Provide introspection capabilities for operational debugging
