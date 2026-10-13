# erlmcp_transport - MCP Transport Behavior Module

## Overview

The `erlmcp_transport` module defines the standard behavior interface for all MCP transport implementations. It provides a polymorphic transport abstraction that supports multiple transport types (stdio, tcp, http, ws, sse) while ensuring compatibility with the MCP API specification.

## Key Features

- **Standardized Interface**: Consistent `connect/1`, `send/2`, `recv/2`, `close/1`, `get_info/1` callbacks
- **Enterprise Features**: Built-in health monitoring, metrics collection, error handling
- **Transport Polymorphism**: Seamless transport selection through configuration
- **OTP 26+ Optimization**: Modern Erlang/OTP features with backward compatibility
- **Chicago TDD Compliance**: No mocks, real processes only

## Behavior Interface

### Required Callbacks

All transport implementations must implement the following callbacks:

#### `connect(TransportState) -> {ok, NewState} | {error, Reason}`

Establishes a connection to the remote endpoint. For server transports (stdio), returns `{ok, connected}`.

**Parameters:**
- `TransportState`: The transport state/instance
- `Return`: Connected state or error reason

**Example:**
```erlang
connect(#state{socket = undefined, config = Config}) ->
    case gen_tcp:connect(maps:get(host, Config), maps:get(port, Config), [binary, {active, true}], 5000) of
        {ok, Socket} -> {ok, #state{socket = Socket, connected = true}};
        {error, Reason} -> {error, {connection_failed, Reason}}
    end.
```

#### `send(TransportState, Data) -> {ok, NewState} | {error, Reason}`

Sends data through the transport. Must handle message framing and encoding.

**Parameters:**
- `TransportState`: The transport state/instance
- `Data`: Data to send (iodata format)
- `Return`: Updated state or error reason

**Example:**
```erlang
send(#state{socket = Socket, connected = true}, Data) ->
    case gen_tcp:send(Socket, [Data, <<"\n">>]) of
        ok -> {ok, State#state{stats = update_stats(State#state.stats, sent, byte_size(Data))}};
        {error, Reason} -> {error, {send_failed, Reason}}
    end.
```

#### `recv(TransportState, Timeout) -> {ok, {Data, NewState}} | {error, Reason}`

Receives data from the transport with optional timeout.

**Parameters:**
- `TransportState`: The transport state/instance
- `Timeout`: Maximum time to wait (infinity for no timeout)
- `Return`: Data with updated state or error reason

**Example:**
```erlang
recv(#state{socket = Socket, connected = true}, Timeout) ->
    receive
        {tcp, Socket, Data} -> {ok, {Data, State#state{stats = update_stats(State#state.stats, received, byte_size(Data))}}};
        {tcp_closed, Socket} -> {error, {receive_failed, connection_closed}}
    after Timeout ->
        {error, timeout}
    end.
```

#### `close(TransportState) -> ok`

Closes the transport connection and cleans up all resources.

**Parameters:**
- `TransportState`: The transport state/instance
- `Return`: always ok

**Example:**
```erlang
close(#state{socket = Socket, metrics_ref = Ref}) ->
    catch gen_tcp:close(Socket),
    catch erlmcp_metrics:unregister(Ref),
    ok.
```

#### `get_info(TransportState) -> {ok, Info} | {error, Reason}`

Returns comprehensive transport information and statistics.

**Parameters:**
- `TransportState`: The transport state/instance
- `Return`: Transport information map or error reason

**Example:**
```erlang
get_info(#state{transport_id = Id, connected = Connected, stats = Stats}) ->
    {ok, #{id => Id, connected => Connected, stats => Stats}}.
```

## Transport Configuration

### Base Configuration

All transport types accept these base options:

```erlang
#{
    id => binary() | atom(),          % Transport identifier
    type => transport_type(),          % stdio | tcp | http | ws | sse
    owner => pid(),                   % Owning process
    connect_timeout => timeout(),      % Connection timeout
    send_timeout => timeout(),         % Send operation timeout
    recv_timeout => timeout(),         % Receive operation timeout
    max_message_size => pos_integer(), % Maximum message size
    health_check => boolean(),         % Enable health monitoring
    transport_metrics => boolean(),    % Enable metrics collection
    test_mode => boolean()            % Test mode flag
}
```

### Transport-Specific Configuration

#### TCP Transport
```erlang
#{type => tcp,
  host => inet:hostname(),      % Target host
  port => inet:port_number(),    % Target port
  max_connections => pos_integer(), % Max concurrent connections
  ssl_options => [ssl:option()], % TLS options
  keepalive => boolean()         % TCP keepalive
}
```

#### HTTP Transport
```erlang
#{type => http,
  url => binary(),              % Target URL
  method => get | post,         % HTTP method
  headers => [],               % Additional headers
  ssl_options => [ssl:option()], % TLS options
  max_retries => pos_integer(), % Retry attempts
  retry_delay => pos_integer()  % Retry delay
}
```

#### WebSocket Transport
```erlang
#{type => ws,
  host => inet:hostname(),      % Target host
  port => inet:port_number(),    % Target port
  path => binary(),             % WebSocket path
  compression => boolean(),     % Enable compression
  ping_interval => pos_integer() % Ping interval
}
```

#### SSE Transport
```erlang
#{type => sse,
  url => binary(),              % Target URL
  headers => [],               % Additional headers
  ping_interval => pos_integer(), % Ping interval
  reconnect_attempts => pos_integer() % Reconnect attempts
}
```

## Usage Examples

### Basic Transport Usage

```erlang
%% Start a TCP transport
Config = #{
    type => tcp,
    id => my_tcp_transport,
    host => "localhost",
    port => 8080,
    owner => self()
},
{ok, TransportPid} = erlmcp_transport:start_link(Config),

%% Connect to remote
{ok, _} = gen_server:call(TransportPid, {connect, Config}),

%% Send data
ok = gen_server:call(TransportPid, {send, <<"message">>}),

%% Receive data
{ok, Data} = gen_server:call(TransportPid, {recv, 5000}),

%% Get transport info
{ok, Info} = erlmcp_transport:get_transport_info(TransportPid),

%% Stop transport
ok = erlmcp_transport:stop_transport(TransportPid).
```

### Advanced Configuration

```erlang
%% High-level transport start with owner
OwnerPid = self(),
Config = #{
    type => ws,
    id => advanced_ws,
    host => "example.com",
    port => 443,
    path => "/mcp",
    compression => gzip,
    health_check => true,
    transport_metrics => true,
    ssl_options => [
        {verify, verify_none},
        {server_name_indication, "example.com"}
    ]
},
{ok, TransportPid} = erlmcp_transport:start_transport(OwnerPid, Config).
```

### Transport Implementation Example

```erlang
-module(my_custom_transport).
-behaviour(erlmcp_transport).

%% Required callbacks
-export([connect/1, send/2, recv/2, close/1, get_info/1]).

%% State record
-record(state,
        {socket :: port() | undefined,
         config :: map(),
         stats :: map()}).

-spec connect(#state{}) -> {ok, #state{}} | {error, term()}.
connect(#state{config = Config} = State) ->
    case gen_tcp:connect(maps:get(host, Config), maps:get(port, Config), [binary, {active, false}], 5000) of
        {ok, Socket} -> {ok, State#state{socket = Socket}};
        {error, Reason} -> {error, {connection_failed, Reason}}
    end.

-spec send(#state{}, iodata()) -> {ok, #state{}} | {error, term()}.
send(#state{socket = Socket} = State, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok -> {ok, State};
        {error, Reason} -> {error, {send_failed, Reason}}
    end.

-spec recv(#state{}, timeout()) -> {ok, {iodata(), #state{}}} | {error, term()}.
recv(#state{socket = Socket} = State, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Data} -> {ok, {Data, State}};
        {error, Reason} -> {error, {receive_failed, Reason}}
    end.

-spec close(#state{}) -> ok.
close(#state{socket = Socket}) ->
    catch gen_tcp:close(Socket),
    ok.

-spec get_info(#state{}) -> {ok, map()} | {error, term()}.
get_info(#state{socket = Socket, config = Config, stats = Stats}) ->
    {ok, #{
        type => custom,
        connected => Socket =/= undefined,
        config => Config,
        stats => Stats
    }}.
```

## Integration with Existing Transport Implementations

The behavior module integrates with existing erlmcp transport implementations:

### TCP Transport Integration
```erlang
%% Uses erlmcp_transport_tcp internally
erlmcp_transport:start_link(tcp, #{host => "localhost", port => 8080})
```

### HTTP Transport Integration
```erlang
%% Uses erlmcp_transport_http internally
erlmcp_transport:start_link(http, #{url => "https://example.com/api"})
```

### WebSocket Transport Integration
```erlang
%% Uses erlmcp_transport_ws internally
erlmcp_transport:start_link(ws, #{host => "localhost", port => 8080, path => "/mcp"})
```

## Error Handling

### Error Types

The behavior defines several standard error types:

- `{connection_failed, Reason}`: Connection establishment failed
- `{send_failed, Reason}`: Data sending failed
- `{receive_failed, Reason}`: Data receiving failed
- `{protocol_error, Reason}`: Protocol-level error
- `{resource_error, Reason}`: Resource exhaustion
- `{invalid_config, Reason}`: Invalid configuration

### Error Handling Best Practices

```erlang
%% Example: Robust send implementation
send(#state{socket = Socket} = State, Data) ->
    try
        case gen_tcp:send(Socket, Data) of
            ok ->
                %% Update stats
                NewStats = maps:update(bytes_sent, State#state.stats + byte_size(Data), State#state.stats),
                {ok, State#state{stats = NewStats}};
            {error, timeout} ->
                %% Schedule retry if appropriate
                handle_send_timeout(State, Data);
            {error, Reason} ->
                {error, {send_failed, Reason}}
        end
    catch
        Class:Error:Stacktrace ->
            %% Log and convert to transport error
            logger:error("Send error: ~p:~p~nStack: ~p", [Class, Error, Stacktrace]),
            {error, {protocol_error, {Class, Error}}}
    end.
```

## Performance Considerations

### Memory Management

- **Buffer Management**: Implement proper buffer sizing to prevent memory leaks
- **Message Limits**: Enforce maximum message sizes to prevent resource exhaustion
- **Connection Pooling**: Use connection pooling for high-throughput scenarios

### Timeout Configuration

```erlang
%% Appropriate timeout settings
Config = #{
    connect_timeout => 5000,     % 5 seconds for connection
    send_timeout => 10000,       % 10 seconds for send operations
    recv_timeout => 5000,        % 5 seconds for receive operations
    max_message_size => 1048576  % 1MB max message size
}.
```

### Performance Monitoring

```erlang
%% Enable metrics collection
Config = #{
    type => tcp,
    transport_metrics => true,   % Enable metrics
    health_check => true,        % Enable health monitoring
    metrics_interval => 60000    % Collect metrics every 60 seconds
}.
```

## Testing

### Unit Testing

The behavior module supports comprehensive unit testing:

```erlang
-ifdef(TEST).
-module(my_transport_tests).
-include_lib("eunit/include/eunit.hrl").

connect_test() ->
    %% Test connection logic
    Config = #{
        type => test,
        host => "testhost",
        port => 8080
    },
    State = #state{config = Config},
    ?assertMatch({ok, _}, my_transport:connect(State)).

send_test() ->
    %% Test sending logic
    State = #state{socket = mock_socket()},
    ?assertMatch({ok, _}, my_transport:send(State, <<"test">>)).

-endif.
```

### Integration Testing

Integration with the transport registry:

```erlang
%% Test transport registration
Config = #{
    type => stdio,
    id => test_transport,
    owner => self()
},
{ok, TransportPid} = erlmcp_transport:start_link(Config),
{ok, Info} = erlmcp_transport:get_transport_info(TransportPid),
?assertEqual(test_transport, maps:get(id, Info)).
```

## Migration Guide

### From Existing Transport Implementations

1. **Add Behavior Declaration**: Add `-behaviour(erlmcp_transport).` to your module
2. **Implement Required Callbacks**: Implement all 5 required callbacks
3. **Update State Records**: Ensure state records include required fields
4. **Add Error Handling**: Implement proper error handling in all callbacks
5. **Register Transport**: Use the behavior's start/stop functions

### Example Migration

**Before:**
```erlang
-module(my_old_tcp_transport).
-behaviour(gen_server).

-export([start_link/1, send/2, close/1]).
```

**After:**
```erlang
-module(my_new_tcp_transport).
-behaviour(gen_server).
-behaviour(erlmcp_transport).

-export([behaviour_info/1]).
-export([connect/1, send/2, recv/2, close/1, get_info/1]).
```

## Troubleshooting

### Common Issues

1. **Callback Conflicts**: Avoid conflicts with gen_server callbacks
2. **Resource Leaks**: Ensure proper cleanup in `close/1`
3. **Timeout Handling**: Implement proper timeout handling
4. **Memory Management**: Monitor memory usage in long-running transports

### Debug Mode

```erlang
%% Enable debug logging
Config = #{
    type => tcp,
    log_level => debug,
    transport_id => debug_transport
}.
```

## API Reference

### Functions

- `behaviour_info(callbacks) -> [{atom(), arity()}]`
- `start_link(Type) -> {ok, pid()} | {error, term()}`
- `start_link(Type, Options) -> {ok, pid()} | {error, term()}`
- `start_transport(Config) -> {ok, pid()} | {error, term()}`
- `start_transport(Owner, Config) -> {ok, pid()} | {error, term()}`
- `validate_transport(Config) -> ok | {error, term()}`
- `get_transport_info(Pid) -> {ok, Info} | {error, term()}`
- `stop_transport(Pid) -> ok | {error, term()}`

### Callbacks

- `connect(State) -> {ok, NewState} | {error, Reason}`
- `send(State, Data) -> {ok, NewState} | {error, Reason}`
- `recv(State, Timeout) -> {ok, {Data, NewState}} | {error, Reason}`
- `close(State) -> ok`
- `get_info(State) -> {ok, Info} | {error, Reason}`

## Compliance Notes

- **MCP API Compatible**: Follows beamai_mcp transport interface patterns
- **OTP 26+ Ready**: Optimized for modern Erlang/OTP features
- **Enterprise Grade**: Built-in monitoring, metrics, and error handling
- **Chicago TDD**: No mocks, real processes only
- **Zero-Defect Quality**: Comprehensive error handling and resource management