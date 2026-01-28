# Migration Guide: Transport Architecture

## Overview

This guide helps you migrate from the old transport architecture to the new behavior-based system introduced in version 0.6.0. The new architecture provides better separation of concerns, improved testability, and support for additional transport types.

## What Changed

### Architecture Overview

#### Before (v0.5.x and earlier)
- Transport logic embedded directly in server/client modules
- Limited to STDIO transport
- Tight coupling between transport and application logic
- Difficult to test and extend

#### After (v0.6.0+)
- Transport behavior interface with pluggable implementations
- Support for STDIO, TCP, HTTP, and custom transports
- Clear separation between transport layer and application logic
- Improved testing with transport mocking capabilities

### Key Changes

1. **Transport Configuration Format**
   - Old: Direct STDIO configuration
   - New: Tuple-based transport specification

2. **Transport Initialization**
   - Old: Automatic STDIO setup
   - New: Explicit transport behavior implementation

3. **Message Handling**
   - Old: Direct stdin/stdout handling
   - New: Transport behavior callbacks

4. **Error Handling**
   - Old: Limited error reporting
   - New: Comprehensive error types and handling

## Migration Steps

### Step 1: Update Dependencies

First, update your `rebar.config` to use the latest version:

```erlang
% rebar.config
{deps, [
    {erlmcp, {git, "https://github.com/banyan-platform/erlmcp.git", {tag, "0.6.0"}}}
]}.
```

### Step 2: Migrate Server Code

#### Old Server Code (v0.5.x)
```erlang
% Old way - direct STDIO
{ok, Server} = erlmcp_server:start_link([], ServerCapabilities),
```

#### New Server Code (v0.6.0+)
```erlang
% New way - explicit transport configuration
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCapabilities),
```

### Step 3: Migrate Client Code

#### Old Client Code (v0.5.x)
```erlang
% Old way - implicit STDIO
{ok, Client} = erlmcp_client:start_link([], ClientOptions),
```

#### New Client Code (v0.6.0+)
```erlang
% New way - explicit transport configuration
{ok, Client} = erlmcp_client:start_link({stdio, []}, ClientOptions),
```

### Step 4: Update Transport Handling

#### Old Transport Message Handling
```erlang
% Old way - direct message handling
handle_info({stdin, Line}, State) ->
    % Process line directly
    process_message(Line, State);
```

#### New Transport Message Handling
```erlang
% New way - transport behavior messages
handle_info({transport_message, Line}, State) ->
    % Process message from any transport
    process_message(Line, State);

handle_info({transport_connected, Transport}, State) ->
    % Handle transport connection
    NewState = State#{transport => Transport},
    {noreply, NewState};

handle_info({transport_disconnected, Transport, Reason}, State) ->
    % Handle transport disconnection
    logger:warning("Transport disconnected: ~p", [Reason]),
    {noreply, State}.
```

### Step 5: Update Configuration

#### Old Configuration
```erlang
% config/sys.config (old)
[
    {my_app, [
        {mcp_server_opts, []}
    ]}
].
```

#### New Configuration
```erlang
% config/sys.config (new)
[
    {my_app, [
        {mcp_transport, {stdio, []}},
        {mcp_server_opts, #{
            capabilities => #{
                resources => #{enabled => true},
                tools => #{enabled => true}
            }
        }}
    ]}
].
```

## Advanced Migration Scenarios

### Migrating to TCP Transport

If you want to upgrade from STDIO to TCP transport:

```erlang
% Before: STDIO only
TransportConfig = {stdio, []}.

% After: TCP with fallback
TransportConfig = case application:get_env(my_app, use_tcp, false) of
    true ->
        {tcp, #{
            host => "localhost",
            port => 8080,
            owner => self(),
            keepalive => true,
            max_reconnect_attempts => 5
        }};
    false ->
        {stdio, []}
end.
```

### Migrating to HTTP Transport

For HTTP-based communication:

```erlang
% HTTP transport configuration
TransportConfig = {http, #{
    url => "https://api.example.com/mcp",
    owner => self(),
    method => post,
    headers => [
        {"Authorization", "Bearer " ++ get_api_token()},
        {"Content-Type", "application/json"}
    ],
    timeout => 15000,
    max_retries => 3
}}.
```

### Custom Transport Implementation

To implement a custom transport:

```erlang
-module(my_custom_transport).
-behaviour(erlmcp_transport).

% Implement required callbacks
-export([init/1, send/2, close/1]).

init(Opts) ->
    % Initialize your custom transport
    State = setup_transport(Opts),
    {ok, State}.

send(State, Data) ->
    % Send data through your transport
    case send_via_custom_protocol(State, Data) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

close(State) ->
    % Clean up transport resources
    cleanup_transport(State),
    ok.
```

## Testing Migration

### Old Test Code
```erlang
% Old way - direct testing
test_server() ->
    {ok, Server} = erlmcp_server:start_link([], #{}),
    % Test by sending to stdin directly (difficult)
```

### New Test Code
```erlang
% New way - transport mocking
test_server() ->
    % Use test mode to avoid stdin conflicts
    put(test_mode, true),
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),

    % Simulate transport messages
    Server ! {transport_message, test_message()},

    % Verify responses
    receive
        {transport_response, Response} ->
            ?assertEqual(expected_response(), Response)
    after 1000 ->
        ?assert(false)  % Test timeout
    end.
```

## Common Migration Issues

### Issue 1: Transport Process Not Found
**Problem**: `{error, transport_not_found}` errors

**Solution**: Ensure transport configuration is provided:
```erlang
% Wrong
{ok, Server} = erlmcp_server:start_link(ServerCapabilities).

% Correct
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCapabilities).
```

### Issue 2: Message Format Changes
**Problem**: Messages not recognized after migration

**Solution**: Update message handlers to expect new format:
```erlang
% Old format
handle_info({stdin_line, Line}, State) -> ...

% New format
handle_info({transport_message, Line}, State) -> ...
```

### Issue 3: Test Environment Issues
**Problem**: Tests hang or fail due to stdin reading

**Solution**: Use test mode in transport configuration:
```erlang
% In test setup
put(test_mode, true),
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCapabilities).
```

### Issue 4: Configuration Loading
**Problem**: Transport options not loaded correctly

**Solution**: Update configuration loading:
```erlang
get_transport_config() ->
    case application:get_env(my_app, mcp_transport) of
        {ok, Config} -> Config;
        undefined -> {stdio, []}  % Default fallback
    end.
```

## Compatibility Mode

For gradual migration, you can use compatibility wrappers:

### Server Compatibility Wrapper
```erlang
-module(erlmcp_server_compat).
-export([start_link/1, start_link/2]).

% Legacy interface support
start_link(Capabilities) ->
    logger:warning("Using deprecated erlmcp_server:start_link/1, "
                  "please migrate to start_link/2"),
    erlmcp_server:start_link({stdio, []}, Capabilities).

start_link(TransportConfig, Capabilities) ->
    erlmcp_server:start_link(TransportConfig, Capabilities).
```

### Client Compatibility Wrapper
```erlang
-module(erlmcp_client_compat).
-export([start_link/1, start_link/2]).

start_link(Opts) ->
    logger:warning("Using deprecated erlmcp_client:start_link/1, "
                  "please migrate to start_link/2"),
    erlmcp_client:start_link({stdio, []}, Opts).

start_link(TransportConfig, Opts) ->
    erlmcp_client:start_link(TransportConfig, Opts).
```

## Verification Checklist

After migration, verify the following:

- [ ] Transport configuration is explicitly specified
- [ ] Message handlers use new transport message format
- [ ] Error handling accounts for transport-specific errors
- [ ] Tests use test mode or transport mocking
- [ ] Configuration loading includes transport settings
- [ ] Logging shows transport connection/disconnection events
- [ ] All transport types work as expected
- [ ] Backward compatibility is maintained where needed

## Performance Considerations

### Transport Selection
- **STDIO**: Best for CLI tools and single-process communication
- **TCP**: Ideal for network services and multi-client scenarios
- **HTTP**: Good for web-based integrations and stateless communication

### Optimization Tips
1. **Buffer Sizes**: Tune transport buffer sizes for your message patterns
2. **Connection Pooling**: Use TCP connection pooling for high-throughput scenarios
3. **Retry Logic**: Configure appropriate retry settings for unreliable networks
4. **Timeouts**: Set realistic timeout values based on your use case

## Getting Help

If you encounter issues during migration:

1. **Check Logs**: Enable debug logging to see transport behavior
2. **Review Examples**: Look at updated examples in the repository
3. **Test Incrementally**: Migrate one component at a time
4. **Use Compatibility Mode**: Gradual migration with compatibility wrappers

## Future Considerations

The new transport architecture enables:

- **WebSocket Support**: Planned for future releases
- **Message Compression**: Transport-level compression options
- **Load Balancing**: Multiple transport instances for scalability
- **Protocol Versioning**: Support for different MCP protocol versions
- **Custom Transports**: Easy implementation of domain-specific transports

This migration guide should help you successfully transition to the new transport architecture while taking advantage of its improved flexibility and extensibility.
