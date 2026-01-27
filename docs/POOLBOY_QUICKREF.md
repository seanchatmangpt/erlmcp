# Poolboy Integration - Quick Reference

## Setup

```erlang
% Add to your application
{applications, [..., poolboy]}
```

## Core Functions

### Start Pool

```erlang
% With default transport config
erlmcp:setup_connection_pool(Type, PoolConfig).

% With custom transport config
erlmcp:setup_connection_pool(Type, PoolConfig, TransportConfig).
```

### Use Pool

```erlang
% Transaction (recommended)
Result = erlmcp:pool_transaction(Type, fun(Worker) ->
    gen_server:call(Worker, Request)
end).

% With timeout
Result = erlmcp:pool_transaction(Type, Fun, 10000).

% Manual checkout (advanced)
Result = erlmcp:with_pool_worker(Type, fun(Worker) ->
    gen_server:call(Worker, Request)
end).
```

### Stop Pool

```erlang
erlmcp:stop_connection_pool(Type).
```

### Monitor Pool

```erlang
{ok, Status} = erlmcp:pool_status(Type).
```

## Transport Types

- `stdio` - Standard I/O
- `tcp` - TCP sockets
- `http` - HTTP/HTTPS

## Pool Configuration

```erlang
#{
    pool_size => 10,      % Permanent workers
    max_overflow => 5     % Temporary workers
}
```

## Transport Configurations

### TCP

```erlang
#{
    host => <<"localhost">>,
    port => 8080,
    keepalive => true,
    connect_timeout => 5000,
    nodelay => true
}
```

### HTTP

```erlang
#{
    url => <<"https://api.example.com">>,
    method => get,
    timeout => 30000,
    verify_ssl => true
}
```

### Stdio

```erlang
#{
    buffer_size => 4096,
    read_timeout => 5000
}
```

## Helper Functions

```erlang
% Get pool name
PoolName = erlmcp:pool_name(tcp).
% => erlmcp_tcp_pool

% Get worker module
Worker = erlmcp:worker_module(http).
% => erlmcp_transport_http_worker
```

## Quick Example

```erlang
% 1. Start pool
{ok, _} = erlmcp:setup_connection_pool(tcp,
    #{pool_size => 10, max_overflow => 5},
    #{host => <<"localhost">>, port => 8080}).

% 2. Use pool
Result = erlmcp:pool_transaction(tcp, fun(Worker) ->
    gen_server:call(Worker, {send, <<"Hello">>})
end).

% 3. Check status
{ok, Status} = erlmcp:pool_status(tcp).

% 4. Stop pool
ok = erlmcp:stop_connection_pool(tcp).
```

## Common Patterns

### Retry on Error

```erlang
retry_send(Msg, 0) -> {error, max_retries};
retry_send(Msg, N) ->
    case erlmcp:pool_transaction(tcp, fun(W) ->
        gen_server:call(W, {send, Msg})
    end) of
        {ok, Result} -> {ok, Result};
        {error, _} ->
            timer:sleep(1000),
            retry_send(Msg, N-1)
    end.
```

### Health Check

```erlang
pool_healthy(Type) ->
    case erlmcp:pool_status(Type) of
        {ok, _} -> true;
        {error, _} -> false
    end.
```

### Concurrent Requests

```erlang
Requests = lists:seq(1, 100),
lists:foreach(fun(N) ->
    spawn(fun() ->
        erlmcp:pool_transaction(tcp, fun(W) ->
            gen_server:call(W, {request, N})
        end)
    end)
end, Requests).
```

## Error Handling

```erlang
case erlmcp:setup_connection_pool(tcp, PoolCfg, TransportCfg) of
    {ok, PoolPid} ->
        % Success
        {ok, PoolPid};
    {error, {validation_error, Reason}} ->
        % Config validation failed
        logger:error("Invalid config: ~p", [Reason]);
    {error, Reason} ->
        % Other error
        logger:error("Pool start failed: ~p", [Reason])
end.
```

## Timeouts

```erlang
% Default: 5 seconds
erlmcp:pool_transaction(Type, Fun).

% Custom timeout
erlmcp:pool_transaction(Type, Fun, 30000).  % 30 seconds
erlmcp:with_pool_worker(Type, Fun, infinity).  % No timeout
```

## See Also

- Full documentation: `/Users/sac/erlmcp/docs/POOLBOY_INTEGRATION.md`
- Examples: `/Users/sac/erlmcp/examples/poolboy/poolboy_example.erl`
- Tests: `/Users/sac/erlmcp/tests/erlmcp_poolboy_tests.erl`
