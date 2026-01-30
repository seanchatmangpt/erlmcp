# Client API Reference

The erlmcp client API provides a complete interface for connecting to MCP servers. This document covers all client functions, types, and usage patterns.

## API Overview

### Client Types
```erlang
% Client reference
-type client_ref() :: pid() | atom().

% Client configuration record
-record(client_config, {
    transport,          % Transport module
    transport_state,    % Transport state
    capabilities,      % Supported capabilities
    request_id = 1,    % Request counter
    pending = #{},     % Pending requests map
    timeout = 5000,    % Default timeout (ms)
    reconnect = true, % Auto-reconnect flag
    max_retries = 3    % Maximum retry attempts
}).
```

## Core Functions

### `start/1` - Start Client

```erlang
-spec start(Config :: map()) -> {ok, Ref :: client_ref()} | {error, Reason :: term()}.
```

Start a new MCP client connection.

**Configuration Options:**
| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `transport` | atom() | tcp | Transport type (tcp, http, stdio) |
| `host` | string() | "localhost" | Server host |
| `port` | integer() | 8080 | Server port |
| `timeout` | integer() | 5000 | Request timeout (ms) |
| `reconnect` | boolean() | true | Auto-reconnect on disconnect |
| `max_retries` | integer() | 3 | Maximum retry attempts |
| `ssl` | boolean() | false | Enable SSL/TLS |
| `ssl_opts` | list() | [] | SSL options |

**Example:**
```erlang
% Basic TCP client
Config = #{
    transport => tcp,
    host => "localhost",
    port => 8080,
    timeout => 5000
},
{ok, Client} = erlmcp_client:start(Config).

% HTTP client with SSL
Config = #{
    transport => http,
    host => "localhost",
    port => 8081,
    ssl => true,
    ssl_opts => [
        {certfile, "/path/to/cert.pem"},
        {keyfile, "/path/to/key.pem"}
    ]
},
{ok, Client} = erlmcp_client:start(Config).

% stdio client
Config = #{
    transport => stdio,
    encoding => utf8
},
{ok, Client} = erlmcp_client:start(Config).
```

### `stop/1` - Stop Client

```erlang
-spec stop(Ref :: client_ref()) -> ok.
```

Stop and clean up the client connection.

**Example:**
```erlang
erlmcp_client:stop(Client).
```

### `call/2` - Send Request

```erlang
-spec call(Ref :: client_ref(), Request :: map()) ->
              {ok, Response :: map()} | {error, Reason :: term()}.
```

Send an MCP request and wait for response.

**Request Format:**
```erlang
Request = #{
    jsonrpc => "2.0",
    id => binary(),
    method => binary(),
    params => map() | list()
}.
```

**Example:**
```erlang
% List tools
Request = #{
    jsonrpc => "2.0",
    id => <<"list-001">>,
    method => <<"tools/list">>
},
{ok, Response} = erlmcp_client:call(Client, Request).

% Call a tool
Request = #{
    jsonrpc => "2.0",
    id => <<"calc-001">>,
    method => <<"tools/call">>,
    params => #{
        name => "calculator",
        arguments => #{expression => "2 + 2"}
    }
},
{ok, Response} = erlmcp_client:call(Client, Request).
```

### `call_async/3` - Send Async Request

```erlang
-spec call_async(Ref :: client_ref(), Request :: map(), Pid :: pid()) ->
                     ok | {error, Reason :: term()}.
```

Send a request asynchronously, results sent to `Pid`.

**Example:**
```erlang
self() ! {mcp_response, Ref, Response}.

Request = #{
    jsonrpc => "2.0",
    id => <<"async-001">>,
    method => <<"tools/call">>,
    params => #{
        name => "calculator",
        arguments => #{expression => "3 * 4"}
    }
},
erlmcp_client:call_async(Client, Request, self()).

% Handle response
receive
    {mcp_response, Ref, Response} ->
        io:format("Response: ~p~n", [Response])
end.
```

### `cast/2` - Send Notification

```erlang
-spec cast(Ref :: client_ref(), Request :: map()) -> ok.
```

Send a notification (no response expected).

**Example:**
```erlang
% Log a message
Request = #{
    jsonrpc => "2.0",
    method => <<"log/info">>,
    params => #{message => "System started"}
},
erlmcp_client:cast(Client, Request).
```

### `get_capabilities/1` - Get Server Capabilities

```erlang
-spec get_capabilities(Ref :: client_ref()) ->
                           {ok, Capabilities :: map()} | {error, Reason :: term()}.
```

Retrieve server capabilities.

**Example:**
```erlang
{ok, Capabilities} = erlmcp_client:get_capabilities(Client),
io:format("Capabilities: ~p~n", [Capabilities]).
```

### `subscribe/3` - Subscribe to Events

```erlang
-spec subscribe(Ref :: client_ref(), Topic :: binary(), Callback :: function()) ->
                     {ok, Subscription :: binary()} | {error, Reason :: term()}.
```

Subscribe to server events.

**Example:**
```erlang
Callback = fun(Event) -> io:format("Event: ~p~n", [Event]) end,
{ok, SubId} = erlmcp_client:subscribe(Client, <<"tool_updates">>, Callback).
```

### `unsubscribe/2` - Unsubscribe from Events

```erlang
-spec unsubscribe(Ref :: client_ref(), Subscription :: binary()) -> ok.
```

Unsubscribe from events.

**Example:**
```erlang
erlmcp_client:unsubscribe(Client, SubId).
```

## State Functions

### `state/1` - Get Client State

```erlang
-spec state(Ref :: client_ref()) -> {ok, State :: map()} | {error, Reason :: term()}.
```

Get current client state.

**Example:**
```erlang
{ok, State} = erlmcp_client:state(Client),
io:format("State: ~p~n", [State]).
```

### `update_config/2` - Update Configuration

```erlang
-spec update_config(Ref :: client_ref(), NewConfig :: map()) ->
                         {ok, Updated :: boolean()} | {error, Reason :: term()}.
```

Update client configuration.

**Example:**
```erlang
NewConfig = Client#client_config{timeout = 10000},
{ok, Updated} = erlmcp_client:update_config(Client, NewConfig).
```

## Utility Functions

### `ping/1` - Test Connection

```erlang
-spec ping(Ref :: client_ref()) -> ok | {error, Reason :: term()}.
```

Test connection to server.

**Example:**
```erlang
case erlmcp_client:ping(Client) of
    ok -> io:format("Connection alive~n");
    {error, Reason} -> io:format("Connection failed: ~p~n", [Reason])
end.
```

### `info/1` - Get Client Information

```erlang
-spec info(Ref :: client_ref()) -> {ok, Info :: map()} | {error, Reason :: term()}.
```

Get client connection information.

**Example:**
```erlang
{ok, Info} = erlmcp_client:info(Client),
io:format("Info: ~p~n", [Info]).
```

### `stats/1` - Get Statistics

```erlang
-spec stats(Ref :: client_ref()) -> {ok, Stats :: map()} | {error, Reason :: term()}.
```

Get client statistics.

**Example:**
```erlang
{ok, Stats} = erlmcp_client:stats(Client),
io:format("Stats: ~p~n", [Stats]).
```

## Batch Operations

### `batch_call/2` - Send Batch Request

```erlang
-spec batch_call(Ref :: client_ref(), Requests :: [map()]) ->
                     {ok, Responses :: [map()]} | {error, Reason :: term()}.
```

Send multiple requests in a single batch.

**Example:**
```erlang
Requests = [
    #{
        jsonrpc => "2.0",
        id => <<"batch-1">>,
        method => <<"tools/list">>
    },
    #{
        jsonrpc => "2.0",
        id => <<"batch-2">>,
        method => <<"tools/call">>,
        params => #{
            name => "calculator",
            arguments => #{expression => "2 + 2"}
        }
    }
],
{ok, Responses} = erlmcp_client:batch_call(Client, Requests).
```

### `batch_async/3` - Send Async Batch

```erlang
-spec batch_async(Ref :: client_ref(), Requests :: [map()], Pid :: pid()) ->
                        ok | {error, Reason :: term()}.
```

Send multiple requests asynchronously.

**Example:**
```erlang
Requests = [
    % ... multiple requests ...
],
erlmcp_client:batch_async(Client, Requests, self()).

receive
    {mcp_batch_response, Ref, Responses} ->
        handle_batch_responses(Responses)
end.
```

## Stream Operations

### `stream_call/3` - Stream Request

```erlang
-spec stream_call(Ref :: client_ref(), Request :: map(), Pid :: pid()) ->
                       ok | {error, Reason :: term()}.
```

Send a streaming request, results sent to `Pid`.

**Example:**
```erlang
Request = #{
    jsonrpc => "2.0",
    id => <<"stream-001">>,
    method => <<"tools/stream">>,
    params => #{
        name => "log_reader",
        arguments => #{path => "/var/log/app.log"}
    }
},
erlmcp_client:stream_call(Client, Request, self()).

receive
    {mcp_stream_data, Ref, Data} ->
        process_stream_data(Data);
    {mcp_stream_end, Ref} ->
        io:format("Stream ended~n")
end.
```

### `cancel_stream/2` - Cancel Stream

```erlang
-spec cancel_stream(Ref :: client_ref(), StreamId :: binary()) -> ok.
```

Cancel an ongoing stream.

**Example:**
```erlang
erlmcp_client:cancel_stream(Client, StreamId).
```

## Error Handling

### Error Types
```erlang
% Error reasons
-type error_reason() ::
    connection_failed |
    timeout |
    protocol_error |
    tool_not_found |
    permission_denied |
    server_error |
    internal_error |
    network_error.
```

### Error Handling Example
```erlang
case erlmcp_client:call(Client, Request) of
    {ok, Response} ->
        handle_success(Response);
    {error, connection_failed} ->
        reconnect_client(Client);
    {error, timeout} ->
        retry_with_backoff(Client, Request);
    {error, tool_not_found} ->
        log_error("Tool not found: ~p", [Request]);
    {error, Reason} ->
        handle_error(Reason)
end.
```

## Transport-Specific Functions

### TCP Transport Functions
```erlang
% TCP-specific options
tcp_client() ->
    #{
        transport => tcp,
        host => "localhost",
        port => 8080,
        socket_opts => [
            {backlog, 128},
            {nodelay, true},
            {reuseaddr, true}
        ]
    }.

% Connection pooling
pool_client() ->
    #{
        transport => tcp,
        host => "localhost",
        port => 8080,
        pool_size => 10,
        max_overflow => 20
    }.
```

### HTTP Transport Functions
```erlang
% HTTP-specific options
http_client() ->
    #{
        transport => http,
        host => "localhost",
        port => 8081,
        ssl => true,
        headers => [
            {"Content-Type", "application/json"},
            {"Authorization", "Bearer token"}
        ],
        timeout => 30000
    }.

% WebSocket client
websocket_client() ->
    #{
        transport => websocket,
        url => "ws://localhost:8081/mcp",
        headers => [
            {"Authorization", "Bearer token"}
        ]
    }.
```

## Callback Interface

### `handle_event/3` - Event Handling

```erlang
-spec handle_event(Event :: atom(), Data :: term(), State :: term()) ->
                         {ok, NewState :: term()} | {stop, Reason :: term()}.
```

Handle client events.

**Example:**
```erlang
handle_event(connect, Data, State) ->
    io:format("Connected: ~p~n", [Data]),
    {ok, State#state{connected = true}};

handle_event(disconnect, Reason, State) ->
    io:format("Disconnected: ~p~n", [Reason]),
    {ok, State#state{connected = false}};

handle_event(message, Message, State) ->
    handle_message(Message, State).
```

### `handle_error/3` - Error Handling

```erlang
-spec handle_error(Error :: term(), Data :: term(), State :: term()) ->
                         {ok, NewState :: term()} | {stop, Reason :: term()}.
```

Handle client errors.

**Example:**
```erlang
handle_error(connection_failed, Data, State) ->
    try_reconnect(State);

handle_error(protocol_error, Data, State) ->
    log_protocol_error(Data),
    {ok, State}.
```

## Examples

### Basic Usage Example
```erlang
% Start client
Config = #{
    transport => tcp,
    host => "localhost",
    port => 8080,
    timeout => 5000
},
{ok, Client} = erlmcp_client:start(Config).

% List tools
Request = #{
    jsonrpc => "2.0",
    id => <<"list-001">>,
    method => <<"tools/list">>
},
case erlmcp_client:call(Client, Request) of
    {ok, Response} ->
        io:format("Available tools: ~p~n", [Response]);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.

% Call a tool
Request = #{
    jsonrpc => "2.0",
    id => <<"calc-001">>,
    method => <<"tools/call">>,
    params => #{
        name => "calculator",
        arguments => #{expression => "2 + 2"}
    }
},
{ok, Response} = erlmcp_client:call(Client, Request),
io:format("Result: ~p~n", [Response]).

% Stop client
erlmcp_client:stop(Client).
```

### Advanced Usage Example
```erlang
% With error handling and retries
call_with_retry(Client, Request, MaxRetries) ->
    call_with_retry(Client, Request, MaxRetries, 0).

call_with_retry(_Client, _Request, 0, Retries) ->
    {error, {max_retries_exceeded, Retries}};

call_with_retry(Client, Request, MaxRetries, Retries) ->
    case erlmcp_client:call(Client, Request) of
        {ok, Response} -> {ok, Response};
        {error, timeout} ->
            timer:sleep(1000 * Retries),
            call_with_retry(Client, Request, MaxRetries, Retries + 1);
        {error, Reason} -> {error, Reason}
    end.

% Batch processing example
process_batch(Client, Operations) ->
    Requests = [
        begin
            {Op, Args} = Operation,
            #{
                jsonrpc => "2.0",
                id => list_to_binary("batch-" ++ integer_to_list(I)),
                method => <<"tools/call">>,
                params => #{
                    name => Op,
                    arguments => Args
                }
            }
        end
    || {I, Operation} <- lists:zip(lists:seq(1, length(Operations)), Operations)
    ],

    case erlmcp_client:batch_call(Client, Requests) of
        {ok, Responses} ->
            Results = lists:zip(lists:seq(1, length(Responses)), Responses),
            [process_result(Result) || Result <- Results];
        {error, Reason} ->
            {error, Reason}
    end.
```

## Performance Considerations

### Connection Management
- **Reuse connections** when possible
- **Use connection pooling** for high-throughput scenarios
- **Monitor connection health** with ping/1

### Request Optimization
- **Batch requests** when possible
- **Use async operations** for fire-and-forget scenarios
- **Set appropriate timeouts** for your use case

### Memory Management
- **Clean up pending requests** with timeouts
- **Use streaming** for large responses
- **Monitor memory usage** with stats/1

---

**Related Documentation:**
- [Server API](./server.md) - Server-side implementation
- [Transport API](./transport.md) - Transport layer interfaces
- [Protocol Documentation](../../explain/protocol.md) - MCP protocol details