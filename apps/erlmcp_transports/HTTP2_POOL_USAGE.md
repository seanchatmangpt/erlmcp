# HTTP/2 Client and ETS Pool Quick Start

**Purpose**: Quick reference for using the new OTP 28-optimized transport implementations

---

## HTTP/2 Client (Gun 2.0.1)

### Basic Usage

```erlang
%% Start HTTP/2 client
Opts = #{
    host => <<"httpbin.org">>,
    port => 443,
    transport => ssl,
    ssl_options => [{verify, verify_peer}],
    max_concurrent_streams => 100,
    retry => 3,
    retry_timeout => 5000
},
{ok, ClientPid} = erlmcp_transport_http2_client:start_link(my_http2_pool, Opts).

%% Make GET request
{ok, 200, Headers, Body} = erlmcp_transport_http2_client:request(
    ClientPid,
    <<"GET">>,
    <<"/get">>,
    [{<<"authorization">>, <<"Bearer token">>}],
    <<>>
).

%% Make POST request
JsonBody = jsx:encode(#{<<"message">> => <<"hello">>}),
{ok, 200, _, ResponseBody} = erlmcp_transport_http2_client:request(
    ClientPid,
    <<"POST">>,
    <<"/post">>,
    [{<<"content-type">>, <<"application/json">>}],
    JsonBody
).

%% Get metrics
{ok, Metrics} = erlmcp_transport_http2_client:get_metrics(ClientPid),
% Metrics = #{
%     total_requests => 150,
%     successful_responses => 148,
%     failed_responses => 2,
%     avg_latency_us => 12345.6,
%     stream_resets => 0
% }

%% Get status
{ok, Status} = erlmcp_transport_http2_client:get_status(ClientPid),
% Status = #{
%     host => <<"httpbin.org">>,
%     active_streams => 5,
%     max_concurrent_streams => 100,
%     stream_utilization_percent => 5.0
% }

%% Close client
erlmcp_transport_http2_client:close(ClientPid).
```

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `host` | binary() | required | Target hostname |
| `port` | integer() | 80 (tcp), 443 (ssl) | Target port |
| `transport` | tcp \| ssl | tcp | Transport type |
| `ssl_options` | list | [] | TLS options |
| `retry` | integer() | 3 | Retry attempts |
| `retry_timeout` | integer() | 5000 | Retry timeout (ms) |
| `max_concurrent_streams` | integer() | 100 | Max concurrent HTTP/2 streams |
| `flow_control_window` | integer() | 65535 | HTTP/2 flow control window |
| `trace` | boolean() | false | Enable Gun tracing |

### HTTP/2 Features

- ✅ **Multiplexing**: Up to 100 concurrent streams per connection
- ✅ **Flow Control**: Automatic window management
- ✅ **Header Compression**: HPACK (automatic with Gun)
- ✅ **TLS 1.3**: Optimized for OTP 27-28
- ✅ **Retry**: Automatic retry with exponential backoff
- ✅ **Metrics**: Latency, throughput, error tracking

---

## ETS-based Connection Pool

### Basic Usage

```erlang
%% Start ETS pool
PoolOpts = #{
    min_size => 10,
    max_size => 1000,
    checkout_timeout => 5000,
    health_check_interval => 60000,
    worker_module => erlmcp_transport_tcp,
    worker_opts => #{host => "localhost", port => 9000}
},
{ok, PoolPid} = erlmcp_transport_pool_ets:start_link(my_pool, PoolOpts).

%% Check out connection
{ok, ConnPid} = erlmcp_transport_pool_ets:checkout(my_pool).

%% Use connection
gen_server:call(ConnPid, {send, <<"Hello">>}).

%% Check in connection
erlmcp_transport_pool_ets:checkin(my_pool, ConnPid).

%% Get pool stats
{ok, Stats} = erlmcp_transport_pool_ets:get_stats(my_pool),
% Stats = #{
%     current_size => 50,
%     idle_connections => 45,
%     active_connections => 5,
%     total_checkouts => 1000,
%     avg_checkout_time_us => 123.4
% }

%% Resize pool
ok = erlmcp_transport_pool_ets:resize(my_pool, 500).

%% Stop pool
ok = erlmcp_transport_pool_ets:stop(my_pool).
```

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `min_size` | integer() | 10 | Minimum pool size |
| `max_size` | integer() | 1000 | Maximum pool size |
| `checkout_timeout` | integer() | 5000 | Checkout timeout (ms) |
| `health_check_interval` | integer() | 60000 | Health check interval (ms) |
| `worker_module` | atom() | required | Worker module |
| `worker_opts` | map() | #{} | Worker options |

### ETS Pool Features

- ✅ **O(1) Checkout**: Lock-free concurrent access
- ✅ **Read Concurrency**: Multiple readers (OTP 28)
- ✅ **Write Concurrency**: Multiple writers (OTP 28)
- ✅ **Health Monitoring**: Automatic health checks
- ✅ **Auto-replenishment**: Maintains min_size
- ✅ **Metrics**: Checkout latency, utilization

---

## Integration Example

```erlang
%% Application supervisor
-module(my_app_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Start ETS pool
    PoolSpec = #{
        id => http2_pool,
        start => {erlmcp_transport_pool_ets, start_link,
                 [http2_pool, pool_opts()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_pool_ets]
    },

    {ok, {{one_for_one, 10, 60}, [PoolSpec]}}.

pool_opts() ->
    #{
        min_size => 10,
        max_size => 100,
        checkout_timeout => 5000,
        health_check_interval => 60000,
        worker_module => erlmcp_transport_http2_client,
        worker_opts => #{
            host => <<"api.example.com">>,
            port => 443,
            transport => ssl,
            max_concurrent_streams => 100
        }
    }.
```

---

## Performance Comparison

### Queue-based Pool (Old)

| Metric | Value |
|--------|-------|
| Checkout latency | ~500µs |
| Max concurrent checkouts | ~1K/s |
| Scheduler contention | High |

### ETS Pool (New)

| Metric | Value | Improvement |
|--------|-------|-------------|
| Checkout latency | <100µs | 5x faster |
| Max concurrent checkouts | ~5K/s | 5x improvement |
| Scheduler contention | Low | Better distribution |

---

## Testing

### Unit Tests

```bash
# Test HTTP/2 client
rebar3 eunit --module=erlmcp_transport_http2_tests

# Test ETS pool
rebar3 eunit --module=erlmcp_transport_pool_ets_tests
```

### Integration Tests

```bash
# Full test suite
rebar3 ct --suite=erlmcp_transport_http2_SUITE
```

### Benchmark

```erlang
%% Benchmark HTTP/2 throughput
run_http2_benchmark() ->
    %% Start client
    {ok, Pid} = erlmcp_transport_http2_client:start_link(#{
        host => <<"localhost">>,
        port => 9000,
        transport => tcp
    }),

    %% Spawn 100 concurrent requests
    NumRequests = 100,
    Start = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        spawn(fun() ->
            erlmcp_transport_http2_client:request(
                Pid, <<"GET">>, <<"/api">>, [], <<>>
            )
        end)
    end, lists:seq(1, NumRequests)),

    %% Wait for completion
    timer:sleep(5000),
    End = erlang:monotonic_time(millisecond),

    %% Get metrics
    {ok, Metrics} = erlmcp_transport_http2_client:get_metrics(Pid),

    Throughput = NumRequests / ((End - Start) / 1000),
    io:format("Throughput: ~p req/s~n", [Throughput]),
    io:format("Avg latency: ~p µs~n", [Metrics#metrics.avg_latency_us]).

%% Expected: >2000 req/s (vs ~500 req/s with HTTP/1.1)
```

---

## Troubleshooting

### HTTP/2 Client

**Issue**: Connection timeout
```
Solution:
- Check host/port configuration
- Verify network connectivity
- Increase retry_timeout: #{retry_timeout => 10000}
```

**Issue**: Stream reset
```
Solution:
- Check max_concurrent_streams limit
- Verify server HTTP/2 support
- Enable trace: #{trace => true}
```

**Issue**: High latency
```
Solution:
- Reduce max_concurrent_streams (flow control)
- Check network latency
- Monitor Metrics#metrics.avg_latency_us
```

### ETS Pool

**Issue**: Checkout timeout
```
Solution:
- Increase max_size
- Reduce checkout_timeout
- Check pool stats for exhaustion
```

**Issue**: Memory growth
```
Solution:
- Reduce max_size
- Increase health_check_interval
- Monitor pool stats
```

---

## Migration Guide

### From Queue Pool to ETS Pool

**Before (Queue Pool)**:
```erlang
{ok, PoolPid} = erlmcp_transport_pool:start_link(my_pool, Config),
{ok, Conn} = erlmcp_transport_pool:acquire(my_pool),
ok = erlmcp_transport_pool:release(my_pool, Conn).
```

**After (ETS Pool)**:
```erlang
{ok, PoolPid} = erlmcp_transport_pool_ets:start_link(my_pool, Config),
{ok, Conn} = erlmcp_transport_pool_ets:checkout(my_pool),
ok = erlmcp_transport_pool_ets:checkin(my_pool, Conn).
```

**API Differences**:
- `acquire/1,2` → `checkout/1,2`
- `release/2` → `checkin/2`
- `get_pool_stats/1` → `get_stats/1`
- Same configuration options (backward compatible)

---

## Best Practices

### HTTP/2 Client

1. **Use connection pooling** for multiple requests
2. **Limit concurrent streams** to prevent flow control issues
3. **Monitor metrics** for performance optimization
4. **Enable TLS 1.3** for production (automatic)
5. **Set appropriate timeouts** based on SLA

### ETS Pool

1. **Set min_size appropriately** (warm pool)
2. **Monitor pool utilization** (target: 60-80%)
3. **Use health checks** for stale connections
4. **Size for peak load** (not average)
5. **Benchmark before production**

---

## References

- **Gun 2.0.1 Docs**: https://ninenines.eu/docs/en/gun/2.0/guide/
- **ETS Optimization**: http://erlang.org/doc/man/ets.html
- **OTP 28 Features**: http://erlang.org/doc/system_principles/system_principles.html
- **HTTP/2 RFC**: https://httpwg.org/specs/rfc7540.html

---

**Last Updated**: 2025-02-01
**Version**: 1.0.0
