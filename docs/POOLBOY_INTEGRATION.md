# Poolboy Connection Pooling Integration

## Overview

The erlmcp library now includes comprehensive connection pooling support via [poolboy](https://github.com/devinus/poolboy). This integration allows efficient management of multiple transport connections (TCP, HTTP, stdio) using worker pools.

## Features

- **Multiple Transport Support**: TCP, HTTP, and stdio connection pooling
- **Automatic Configuration Validation**: All transport configurations are validated before pool creation
- **Flexible Pool Sizing**: Configurable pool size and overflow limits
- **Safe Worker Management**: Automatic checkout/checkin with error handling
- **Pool Status Monitoring**: Real-time pool status and metrics
- **Type-Safe API**: Full type specifications for compile-time safety

## API Functions

### Pool Lifecycle Management

#### `setup_connection_pool/2`

Create a connection pool with default transport configuration.

```erlang
-spec setup_connection_pool(transport_type(), map()) -> {ok, pid()} | {error, term()}.

% Example
PoolConfig = #{
    pool_size => 10,
    max_overflow => 5
},
{ok, PoolPid} = erlmcp:setup_connection_pool(tcp, PoolConfig).
```

#### `setup_connection_pool/3`

Create a connection pool with custom transport configuration.

```erlang
-spec setup_connection_pool(transport_type(), map(), map()) -> {ok, pid()} | {error, term()}.

% Example
PoolConfig = #{
    pool_size => 10,
    max_overflow => 5
},
TransportConfig = #{
    host => <<"example.com">>,
    port => 8080,
    keepalive => true,
    connect_timeout => 5000
},
{ok, PoolPid} = erlmcp:setup_connection_pool(tcp, PoolConfig, TransportConfig).
```

#### `stop_connection_pool/1`

Stop a connection pool and release all resources.

```erlang
-spec stop_connection_pool(transport_type()) -> ok.

% Example
ok = erlmcp:stop_connection_pool(tcp).
```

### Worker Management

#### `with_pool_worker/2`

Execute a function with a checked-out worker (default 5 second timeout).

```erlang
-spec with_pool_worker(transport_type(), fun((pid()) -> term())) -> term() | {error, term()}.

% Example
Result = erlmcp:with_pool_worker(tcp, fun(Worker) ->
    gen_server:call(Worker, {send_message, <<"Hello">>})
end).
```

#### `with_pool_worker/3`

Execute a function with a checked-out worker and custom timeout.

```erlang
-spec with_pool_worker(transport_type(), fun((pid()) -> term()), timeout()) -> term() | {error, term()}.

% Example
Result = erlmcp:with_pool_worker(tcp, fun(Worker) ->
    gen_server:call(Worker, {send_large_file, FileData})
end, 30000).
```

#### `pool_transaction/2`

Execute a poolboy transaction (safer than with_pool_worker, default 5 second timeout).

```erlang
-spec pool_transaction(transport_type(), fun((pid()) -> term())) -> term().

% Example
Result = erlmcp:pool_transaction(http, fun(Worker) ->
    gen_server:call(Worker, {http_request, Url})
end).
```

#### `pool_transaction/3`

Execute a poolboy transaction with custom timeout.

```erlang
-spec pool_transaction(transport_type(), fun((pid()) -> term()), timeout()) -> term().

% Example
Result = erlmcp:pool_transaction(http, fun(Worker) ->
    gen_server:call(Worker, {http_post, Url, Body})
end, 10000).
```

### Pool Monitoring

#### `pool_status/1`

Get the current status of a connection pool.

```erlang
-spec pool_status(transport_type()) -> {ok, map()} | {error, term()}.

% Example
{ok, Status} = erlmcp:pool_status(tcp).
% Status: #{
%     pool_name => erlmcp_tcp_pool,
%     transport_type => tcp,
%     status => {ready, 10, 0, 0}  % {state, available, overflow, waiting}
% }
```

### Helper Functions

#### `pool_name/1`

Get the pool name for a transport type.

```erlang
-spec pool_name(transport_type()) -> atom().

% Examples
erlmcp_tcp_pool = erlmcp:pool_name(tcp).
erlmcp_http_pool = erlmcp:pool_name(http).
erlmcp_stdio_pool = erlmcp:pool_name(stdio).
```

#### `worker_module/1`

Get the worker module for a transport type.

```erlang
-spec worker_module(transport_type()) -> module().

% Examples
erlmcp_transport_tcp_worker = erlmcp:worker_module(tcp).
erlmcp_transport_http_worker = erlmcp:worker_module(http).
```

## Configuration

### Pool Configuration

Pool configuration controls the pool size and overflow behavior:

```erlang
PoolConfig = #{
    pool_size => 10,      % Number of permanent workers
    max_overflow => 5     % Additional temporary workers allowed
}
```

### Transport Configuration

#### TCP Configuration

```erlang
TcpConfig = #{
    type => tcp,                    % Required
    host => <<"localhost">>,        % Required
    port => 8080,                   % Required (1-65535)
    keepalive => true,              % Optional (default: true)
    connect_timeout => 5000,        % Optional (milliseconds)
    recv_timeout => 30000,          % Optional (milliseconds)
    send_timeout => 5000,           % Optional (milliseconds)
    nodelay => true,                % Optional (default: true)
    buffer_size => 4096,            % Optional (bytes)
    max_connections => 100          % Optional
}
```

#### HTTP Configuration

```erlang
HttpConfig = #{
    type => http,                           % Required
    url => <<"https://api.example.com">>,   % Required
    method => get,                          % Optional (get/post/put/delete/etc.)
    headers => #{},                         % Optional
    timeout => 30000,                       % Optional (milliseconds)
    max_redirects => 5,                     % Optional
    verify_ssl => true,                     % Optional
    compression => true                     % Optional
}
```

#### Stdio Configuration

```erlang
StdioConfig = #{
    type => stdio,              % Required
    buffer_size => 4096,        % Optional (bytes)
    read_timeout => 5000        % Optional (milliseconds)
}
```

## Usage Examples

### Example 1: TCP Connection Pool

```erlang
start_tcp_pool() ->
    % Create pool configuration
    PoolConfig = #{
        pool_size => 20,
        max_overflow => 10
    },

    % Create transport configuration
    TransportConfig = #{
        host => <<"mcp.example.com">>,
        port => 9090,
        keepalive => true,
        nodelay => true
    },

    % Start the pool
    case erlmcp:setup_connection_pool(tcp, PoolConfig, TransportConfig) of
        {ok, PoolPid} ->
            logger:info("TCP pool started: ~p", [PoolPid]),
            {ok, PoolPid};
        {error, Reason} ->
            logger:error("Failed to start TCP pool: ~p", [Reason]),
            {error, Reason}
    end.

send_message(Message) ->
    % Use the pool to send a message
    erlmcp:pool_transaction(tcp, fun(Worker) ->
        gen_server:call(Worker, {send, Message})
    end, 5000).

stop_tcp_pool() ->
    erlmcp:stop_connection_pool(tcp).
```

### Example 2: HTTP Connection Pool with Monitoring

```erlang
start_http_pool() ->
    PoolConfig = #{pool_size => 50, max_overflow => 20},
    HttpConfig = #{url => <<"https://api.anthropic.com/v1/messages">>},

    {ok, _} = erlmcp:setup_connection_pool(http, PoolConfig, HttpConfig),

    % Monitor pool status
    spawn(fun() -> monitor_pool_loop() end),
    ok.

monitor_pool_loop() ->
    timer:sleep(10000),  % Check every 10 seconds
    case erlmcp:pool_status(http) of
        {ok, #{status := Status}} ->
            logger:info("HTTP pool status: ~p", [Status]),
            monitor_pool_loop();
        {error, Reason} ->
            logger:error("Failed to get pool status: ~p", [Reason])
    end.
```

### Example 3: Multiple Pools with Error Handling

```erlang
start_all_pools() ->
    Pools = [
        {tcp, #{pool_size => 10, max_overflow => 5},
         #{host => <<"tcp.example.com">>, port => 8080}},

        {http, #{pool_size => 20, max_overflow => 10},
         #{url => <<"https://api.example.com">>}},

        {stdio, #{pool_size => 5, max_overflow => 2}, #{}}
    ],

    Results = lists:map(fun({Type, PoolCfg, TransportCfg}) ->
        case erlmcp:setup_connection_pool(Type, PoolCfg, TransportCfg) of
            {ok, Pid} ->
                {ok, Type, Pid};
            {error, Reason} ->
                {error, Type, Reason}
        end
    end, Pools),

    % Check for failures
    Errors = [E || {error, _, _} = E <- Results],
    case Errors of
        [] ->
            logger:info("All pools started successfully"),
            ok;
        _ ->
            logger:error("Some pools failed to start: ~p", [Errors]),
            {error, Errors}
    end.
```

## Configuration Validation

All transport configurations are automatically validated before pool creation:

```erlang
% This will fail validation - invalid port
BadConfig = #{
    host => <<"example.com">>,
    port => 99999  % Port must be 1-65535
},

{error, {validation_error, {invalid_port, 99999}}} =
    erlmcp:setup_connection_pool(tcp, #{pool_size => 10}, BadConfig).

% This will fail validation - missing required field
IncompleteConfig = #{
    host => <<"example.com">>
    % Missing 'port' field
},

{error, {validation_error, {missing_required_fields, [port]}}} =
    erlmcp:setup_connection_pool(tcp, #{pool_size => 10}, IncompleteConfig).
```

## Worker Modules

Worker modules must implement the poolboy_worker behavior:

```erlang
-module(erlmcp_transport_tcp_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

% Poolboy callbacks
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    % Initialize TCP connection using Args
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    % ... connection setup
    {ok, #state{host = Host, port = Port}}.

% ... other gen_server callbacks
```

## Best Practices

1. **Pool Sizing**: Size pools based on expected load
   - Low latency operations: smaller pools (5-10)
   - High latency operations: larger pools (20-50)
   - Set overflow to 50-100% of pool size

2. **Timeouts**: Use appropriate timeouts for operations
   - Quick operations: 1-5 seconds
   - Network operations: 10-30 seconds
   - File transfers: 60+ seconds

3. **Error Handling**: Always handle pool errors
   - Worker unavailable: implement retry logic
   - Pool not found: ensure pool is started before use
   - Validation errors: check configuration before startup

4. **Resource Cleanup**: Always stop pools when done
   ```erlang
   cleanup() ->
       lists:foreach(fun erlmcp:stop_connection_pool/1, [tcp, http, stdio]).
   ```

5. **Monitoring**: Regularly check pool status in production
   ```erlang
   health_check() ->
       Types = [tcp, http, stdio],
       lists:all(fun(Type) ->
           case erlmcp:pool_status(Type) of
               {ok, _} -> true;
               {error, _} -> false
           end
       end, Types).
   ```

## Performance Considerations

- **Connection Reuse**: Workers maintain persistent connections, reducing overhead
- **Concurrent Access**: Pools allow multiple concurrent requests without blocking
- **Overflow Workers**: Temporary workers handle load spikes without starving regular requests
- **Resource Limits**: Pool limits prevent resource exhaustion

## Dependencies

- **poolboy**: Version 1.5.2 or higher
- Already included in `rebar.config` dependencies

## Future Enhancements

The following worker modules will be implemented in future releases:

- `erlmcp_transport_stdio_worker` - stdio transport worker
- `erlmcp_transport_tcp_worker` - TCP transport worker
- `erlmcp_transport_http_worker` - HTTP transport worker
- `erlmcp_transport_worker` - Generic fallback worker

## References

- [Poolboy GitHub](https://github.com/devinus/poolboy)
- [Poolboy Documentation](https://hexdocs.pm/poolboy/)
- [erlmcp Documentation](../README.md)
