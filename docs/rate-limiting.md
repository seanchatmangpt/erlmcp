# Rate Limiting & Throttling in erlmcp

## Overview

erlmcp implements sophisticated rate limiting and throttling to protect against DoS attacks and ensure fair resource allocation. The system provides multiple algorithms, priority-based access, and distributed coordination across clusters.

## Features

- **Multiple Algorithms**: Token bucket, sliding window, leaky bucket
- **Per-Client Limits**: Isolated rate limits for each client
- **Per-Method Limits**: Different limits for different MCP methods
- **Global Limits**: Cluster-wide rate limiting
- **Priority System**: High-priority clients bypass limits
- **DDoS Protection**: Automatic blocking after threshold violations
- **Distributed Coordination**: gproc-based cluster-wide limits
- **Middleware Integration**: Transparent rate limiting for all requests
- **<1% Overhead**: Minimal performance impact

## Architecture

```
Request Flow:
    Client Request
         |
         v
    erlmcp_rate_limit_middleware  <-- Intercepts all requests
         |
         v
    erlmcp_rate_limiter           <-- Token bucket / sliding window / leaky bucket
         |
         v
    gproc (distributed counters)  <-- Cluster-wide coordination
         |
         v
    {ok, Continue} | {error, rate_limited, RetryAfter}
```

## Configuration

### sys.config

```erlang
{erlmcp, [
    {rate_limiting, #{
        %% Per-client message rate (messages/second)
        max_messages_per_sec => 100,

        %% Per-client connection rate (connections/second)
        max_connections_per_sec => 10,

        %% Global message rate (messages/second)
        global_max_messages_per_sec => 10000,

        %% Tool execution rate per client (calls/second)
        max_tool_calls_per_sec => 50,

        %% Resource subscription rate per client (subscriptions/second)
        max_subscriptions_per_sec => 20,

        %% Token bucket refill interval (milliseconds)
        bucket_refill_interval_ms => 100,

        %% DDoS protection: violations per minute to trigger block
        ddos_violation_threshold => 100,

        %% DDoS protection: block duration (milliseconds)
        ddos_block_duration_ms => 300000,  % 5 minutes

        %% Enable/disable rate limiting
        enabled => true
    }},

    %% Method-specific rate limits
    {method_rate_limits, #{
        <<"tools/call">> => #{
            max_rate => 50,
            scope => per_tool
        },
        <<"resources/read">> => #{
            max_rate => 100,
            scope => per_client
        },
        <<"resources/subscribe">> => #{
            max_rate => 20,
            scope => per_client
        }
    }}
]}.
```

## API Usage

### Basic Rate Limiting

```erlang
%% Check message rate for a client
ClientId = <<"client_192.168.1.1">>,
TimeMs = erlang:system_time(millisecond),

case erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs) of
    {ok, TokensRemaining} ->
        %% Process request
        handle_request(Request);
    {error, rate_limited, RetryAfterMs} ->
        %% Return 429 error
        {error, ?MCP_ERROR_RATE_LIMITED, <<"Rate limit exceeded">>, #{
            <<"retry_after_ms">> => RetryAfterMs
        }}
end.
```

### Priority-Based Limiting

```erlang
%% Set high priority for VIP client
erlmcp_rate_limiter:set_client_priority(ClientId, high),

%% Check with priority (high priority bypasses limits)
case erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs, high) of
    {ok, _} ->
        %% High priority clients always pass (unless DDoS blocked)
        process_request(Request)
end.
```

### Middleware Integration

```erlang
%% Automatically check rate limits for all requests
case erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs) of
    {ok, _Remaining} ->
        %% Continue with request processing
        erlmcp_server:handle_request(Server, Method, Params);
    {error, rate_limited, RetryAfter} ->
        %% Inject Retry-After header
        ErrorResponse = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => RequestId,
            <<"error">> => #{
                <<"code">> => ?MCP_ERROR_RATE_LIMITED,
                <<"message">> => <<"Rate limit exceeded">>
            }
        },
        erlmcp_rate_limit_middleware:inject_retry_after(ErrorResponse, RetryAfter)
end.
```

### Distributed Limits (Cluster-Wide)

```erlang
%% Increment distributed counter (synchronized across cluster)
Scope = <<"global_api_calls">>,
{ok, NewCount} = erlmcp_rate_limiter:increment_distributed_limit(Scope, 1),

%% Check distributed limit from any node
{ok, CurrentCount} = erlmcp_rate_limiter:get_distributed_limit(Scope, node()),

%% Example: Global limit across all nodes
case CurrentCount > GlobalLimit of
    true ->
        {error, global_rate_limited, 1000};
    false ->
        {ok, continue}
end.
```

### DDoS Protection

```erlang
%% Check if client is under DDoS attack
case erlmcp_rate_limiter:is_ddos_attack(ClientId) of
    true ->
        %% Block client completely
        {error, blocked, <<"DDoS protection triggered">>};
    false ->
        %% Continue normal processing
        handle_request(Request)
end.

%% Check if client is currently rate limited
IsBlocked = erlmcp_rate_limiter:is_rate_limited(ClientId),
% true if blocked_until timestamp hasn't expired

%% Get detailed client info
{ok, ClientState} = erlmcp_rate_limiter:get_client_info(ClientId),
% #{message_bucket => ..., violations => ..., blocked_until => ...}

%% Reset client (remove all limits)
ok = erlmcp_rate_limiter:reset_client(ClientId).
```

### Monitoring & Statistics

```erlang
%% Get system statistics
Stats = erlmcp_rate_limiter:get_stats(),
% #{
%     clients => 150,                    % Active clients
%     violations => 5,                   % Clients with violations
%     blocked_clients => [<<"1.2.3.4">>], % Currently blocked
%     config => #{}                      % Current configuration
% }
```

## Algorithms

### Token Bucket (Default)

- **Best for**: Burst traffic with average rate limits
- **How it works**: Bucket fills with tokens at fixed rate. Each request consumes one token.
- **Advantages**: Allows bursts up to bucket capacity
- **Use case**: General message rate limiting

```erlang
%% Token bucket automatically used by default
Bucket = erlmcp_rate_limiter:create_token_bucket(100),
{ok, NewBucket, Remaining} = erlmcp_rate_limiter:consume_token(Bucket, 100).
```

### Sliding Window

- **Best for**: Accurate rate limiting without burst issues
- **How it works**: Tracks all requests in a time window, evicts old ones
- **Advantages**: More accurate than token bucket
- **Use case**: Strict rate limits (e.g., API billing)

```erlang
%% Create sliding window (60 second window)
Window = erlmcp_rate_limiter:create_sliding_window(60000),

%% Check rate (100 requests per window)
TimeMs = erlang:system_time(millisecond),
case erlmcp_rate_limiter:check_sliding_window(Window, 100, TimeMs) of
    {ok, NewWindow, Remaining} -> {ok, NewWindow};
    {error, exceeded} -> {error, rate_limited}
end.
```

### Leaky Bucket

- **Best for**: Smoothing bursty traffic
- **How it works**: Requests queue, leak out at constant rate
- **Advantages**: Smooths traffic spikes
- **Use case**: Protecting downstream services from bursts

```erlang
%% Create leaky bucket (100 request capacity)
Bucket = erlmcp_rate_limiter:create_leaky_bucket(100),

%% Check (leaks at capacity/second rate)
case erlmcp_rate_limiter:check_leaky_bucket(Bucket, 100) of
    {ok, NewBucket, Space} -> {ok, NewBucket};
    {error, exceeded} -> {error, queue_full}
end.
```

## Priority System

Three priority levels:

1. **high**: Bypasses rate limits (but not DDoS blocks)
2. **normal**: Standard rate limiting (default)
3. **low**: May be throttled more aggressively

```erlang
%% Set priority
erlmcp_rate_limiter:set_client_priority(ClientId, high),

%% Check with priority
erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs, high).
```

### Priority Use Cases

- **high**: Admin users, health checks, critical operations
- **normal**: Regular API users
- **low**: Background jobs, analytics, bulk operations

## Scope Levels

Rate limits can be scoped at different levels:

1. **per_client**: Each client has independent limit
2. **per_tool**: Each tool has independent limit
3. **global**: All clients share one limit

```erlang
%% Per-client: Each client gets 100 req/sec
max_messages_per_sec => 100

%% Per-tool: Each tool gets 50 calls/sec per client
max_tool_calls_per_sec => 50

%% Global: All clients share 10,000 req/sec total
global_max_messages_per_sec => 10000
```

## Performance

### Overhead Measurements

From `erlmcp_bench_rate_limit.erl`:

- **Token bucket**: <0.5 μs per check
- **Sliding window**: ~2 μs per check
- **Leaky bucket**: ~1 μs per check
- **Middleware**: <1 μs overhead
- **Total overhead**: <1% of request processing time

### Benchmarks

```bash
# Run all rate limit benchmarks
rebar3 shell
> erlmcp_bench_rate_limit:run_all().

# Run specific workload
> erlmcp_bench_rate_limit:run(<<"rate_limit_token_bucket_100k">>).
```

Expected throughput:
- Token bucket: ~2M ops/sec
- Per-client isolation: ~500K ops/sec
- Global limits: ~1M ops/sec
- Distributed limits: ~100K ops/sec

## DDoS Protection

### Automatic Blocking

When a client exceeds `ddos_violation_threshold` violations in a 1-minute window:

1. Client is blocked for `ddos_block_duration_ms` (default 5 minutes)
2. All requests return `{error, rate_limited, RetryAfter}`
3. High-priority clients also blocked (security over convenience)
4. Block expires automatically
5. Violations reset after block

### Manual Intervention

```erlang
%% Check if attack detected
case erlmcp_rate_limiter:is_ddos_attack(ClientId) of
    true ->
        %% Log security event
        logger:error("DDoS attack from ~p", [ClientId]),

        %% Optionally extend block
        %% (or reset to give second chance)
        erlmcp_rate_limiter:reset_client(ClientId)
end.
```

## Distributed Coordination

Using gproc for cluster-wide limits:

```erlang
%% On any node in cluster
erlmcp_rate_limiter:increment_distributed_limit(<<"api_calls">>, 1),

%% Check from any node (automatically synced)
{ok, ClusterTotal} = erlmcp_rate_limiter:get_distributed_limit(
    <<"api_calls">>,
    node()
).
```

### Network Partition Handling

- **Split-brain**: Each partition maintains independent counters
- **Rejoin**: Counters reconcile automatically
- **Safety**: Conservative approach (may over-limit during partition)

## Best Practices

1. **Start conservative**: Begin with strict limits, relax as needed
2. **Monitor violations**: Track `erlmcp_rate_limiter:get_stats()` regularly
3. **Use priorities wisely**: Reserve `high` for truly critical clients
4. **Tune per-method**: Different methods have different costs
5. **Enable DDoS protection**: Set reasonable `ddos_violation_threshold`
6. **Test under load**: Use benchmarks to validate configuration
7. **Coordinate with clients**: Communicate rate limits via headers
8. **Implement retry logic**: Clients should respect `Retry-After`
9. **Use distributed limits**: For cluster-wide fairness
10. **Log security events**: Track DDoS attacks for analysis

## Testing

```bash
# Unit tests
rebar3 eunit --module=erlmcp_rate_limiting_tests

# Middleware tests
rebar3 eunit --module=erlmcp_rate_limit_middleware_tests

# Benchmarks
rebar3 shell
> erlmcp_bench_rate_limit:run_all().
```

## Troubleshooting

### False Positives

If legitimate clients are blocked:

```erlang
%% Increase violation threshold
application:set_env(erlmcp, rate_limiting, #{
    ddos_violation_threshold => 200  % More lenient
}).

%% Or reduce block duration
application:set_env(erlmcp, rate_limiting, #{
    ddos_block_duration_ms => 60000  % 1 minute instead of 5
}).
```

### Performance Issues

If rate limiting causes slowdowns:

```erlang
%% Disable for debugging
application:set_env(erlmcp, rate_limiting, #{enabled => false}).

%% Or increase limits
application:set_env(erlmcp, rate_limiting, #{
    max_messages_per_sec => 1000  % 10x increase
}).
```

### Monitoring Bottlenecks

```erlang
%% Check ETS table sizes
Stats = erlmcp_rate_limiter:get_stats(),
io:format("Active clients: ~p~n", [maps:get(clients, Stats)]),

%% Profile if needed
eprof:start(),
eprof:profile([erlmcp_rate_limiter], fun() ->
    erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs)
end).
```

## See Also

- [Architecture Guide](architecture.md)
- [API Reference](api-reference.md)
- [Benchmarks](../bench/erlmcp_bench_rate_limit.erl)
- [MCP Error Codes](protocol.md#error-codes)
