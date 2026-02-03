# Database Scaling Implementation

## Overview

This document describes the database scaling implementation for erlmcp v3, including connection pooling, health checks, retry logic with exponential backoff, and circuit breakers for database failures.

## Components

### 1. erlmcp_db_pool (`apps/erlmcp_core/src/erlmcp_db_pool.erl`)

The main database connection pool manager providing:

- **Connection Pooling**: Uses poolboy for efficient connection management
- **Health Checks**: Periodic health checks for all pool connections
- **Exponential Backoff Retry**: Configurable retry with exponential backoff
- **Circuit Breaker**: Automatic circuit breaking on database failures
- **Transaction Support**: ACID transaction management
- **Telemetry**: Comprehensive metrics collection

#### API Functions

```erlang
%% Pool management
erlmcp_db_pool:start_link() -> {ok, pid()} | {error, term()}
erlmcp_db_pool:start_pool(PoolName, Config) -> {ok, pid()} | {error, term()}
erlmcp_db_pool:stop_pool(PoolName) -> ok

%% Query execution
erlmcp_db_pool:execute(PoolName, Query) -> {ok, Result} | {error, term()}
erlmcp_db_pool:execute(PoolName, Query, Params) -> {ok, Result} | {error, term()}

%% Transaction support
erlmcp_db_pool:transaction(PoolName, Fun) -> {ok, Result} | {error, term()}
erlmcp_db_pool:with_connection(PoolName, Fun) -> {ok, Result} | {error, term()}

%% Monitoring
erlmcp_db_pool:stats(PoolName) -> {ok, Stats} | {error, term()}
erlmcp_db_pool:health_check(PoolName) -> {ok, Health} | {error, term()}
erlmcp_db_pool:circuit_breaker_info(PoolName) -> {ok, Info} | {error, term()}
erlmcp_db_pool:reset_circuit_breaker(PoolName) -> ok | {error, term()}
```

#### Configuration

```erlang
{erlmcp_db_pool, [
    {pools, [
        {default, [
            {size, 10},                          % Min pool size
            {max_overflow, 20},                  % Max overflow
            {connection_mod, erlmcp_db_connection},
            {backend, postgres},                  % postgres | mysql | mock
            {host, "localhost"},
            {port, 5432},
            {database, "erlmcp"},
            {user, "postgres"},
            {password, ""},
            {health_check_interval, 30000},       % 30 seconds
            {circuit_breaker_threshold, 5},       % Failures before opening
            {circuit_breaker_timeout, 60000},     % Time before half-open (ms)
            {retry_max_attempts, 3},              % Max retry attempts
            {retry_base_delay, 100},              % Base delay (ms)
            {retry_max_delay, 10000}              % Max delay (ms)
        ]}
    ]}
]}.
```

### 2. erlmcp_db_connection (`apps/erlmcp_core/src/erlmcp_db_connection.erl`)

The database connection worker implementing poolboy_worker behaviour:

- **Backend Support**: PostgreSQL (epgsql), MySQL, Mock (for testing)
- **Query Execution**: Parameterized queries with proper error handling
- **Transaction Management**: begin, commit, rollback
- **Health Checks**: Per-connection health monitoring
- **Graceful Shutdown**: Proper connection cleanup

#### API Functions

```erlang
%% Connection lifecycle (used by poolboy)
erlmcp_db_connection:start_link(Args) -> {ok, pid()} | {error, term()}
erlmcp_db_connection:disconnect(Connection) -> ok

%% Query execution
erlmcp_db_connection:query(Connection, Query) -> {ok, Rows} | {error, term()}
erlmcp_db_connection:query(Connection, Query, Params) -> {ok, Rows} | {error, term()}

%% Transaction management
erlmcp_db_connection:begin_transaction(Connection) -> {ok, term()} | {error, term()}
erlmcp_db_connection:commit(Connection) -> {ok, term()} | {error, term()}
erlmcp_db_connection:rollback(Connection) -> ok

%% Health checks
erlmcp_db_connection:health_check(Connection) -> {ok, map()} | {error, term()}
```

### 3. erlmcp_db_pool_sup (`apps/erlmcp_core/src/erlmcp_db_pool_sup.erl`)

Top-level supervisor for database connection pools, managed by `erlmcp_resilience_sup`.

## Circuit Breaker State Machine

```
                    +-----------------+
                    |     CLOSED      |  <- Normal operation
                    |  (allowing)     |
                    +--------+--------+
                             |
                    v failures >= threshold
                    +-----------------+
                    |      OPEN       |  <- Blocking requests
                    |   (blocking)    |
                    +--------+--------+
                             |
                    v timeout elapsed
                    +-----------------+
                    |   HALF_OPEN     |  <- Testing recovery
                    |   (testing)     |
                    +--------+--------+
                             |
            +---------------+---------------+
            |                               |
    v success threshold             v any failure
    +-----------------+               +-----------------+
    |     CLOSED      |               |      OPEN       |
    +-----------------+               +-----------------+
```

### Circuit States

- **CLOSED**: Normal operation, requests pass through
- **OPEN**: Requests blocked after threshold failures
- **HALF_OPEN**: Testing if database has recovered (limited requests)

### Transitions

- CLOSED -> OPEN: When failure count reaches threshold
- OPEN -> HALF_OPEN: After circuit timeout expires
- HALF_OPEN -> CLOSED: After success threshold achieved
- HALF_OPEN -> OPEN: On any failure during testing

## Exponential Backoff

Retry delays follow exponential backoff with jitter:

```
delay(n) = min(base_delay * 2^n + jitter, max_delay)
```

Example with base_delay=100ms, max_delay=10000ms:

| Attempt | Delay (ms) | With Jitter |
|---------|------------|-------------|
| 0       | 100        | 100-125     |
| 1       | 200        | 200-250     |
| 2       | 400        | 400-500     |
| 3       | 800        | 800-1000    |
| 4       | 1600       | 1600-2000   |
| 5       | 3200       | 3200-4000   |
| 6+      | 10000      | 10000       |

## Health Checks

Health checks run periodically (default 30 seconds) on all pools:

```erlang
{ok, #{
    pool => PoolName,
    status => healthy | unhealthy,
    result => HealthResult
}}
```

For mock backend, health check always returns healthy.
For real backends, executes `SELECT 1` or equivalent.

## Metrics

Collected metrics per pool:

- `total_queries`: Total queries executed
- `successful_queries`: Successful query count
- `failed_queries`: Failed query count
- `circuit_opens`: Number of times circuit opened
- `total_latency_us`: Cumulative query latency

## Testing

### Test Modules

- `erlmcp_db_pool_tests.erl`: Pool manager tests
- `erlmcp_db_connection_tests.erl`: Connection worker tests

### Test Categories

1. **Lifecycle Tests**: Start/stop, connection management
2. **Query Tests**: Simple queries, parameterized queries
3. **Transaction Tests**: Success, failure, rollback
4. **Circuit Breaker Tests**: State transitions
5. **Health Check Tests**: Mock and real backends
6. **Performance Tests**: Concurrent query handling

### Running Tests

```bash
# Via Docker
docker compose run --rm erlmcp-ct

# Direct EUnit
rebar3 eunit --module=erlmcp_db_pool_tests
rebar3 eunit --module=erlmcp_db_connection_tests
```

## Integration with Resilience Supervisor

The database pool is integrated into the resilience infrastructure:

```
erlmcp_resilience_sup
├── erlmcp_connection_limiter
├── erlmcp_connection_monitor
├── erlmcp_memory_monitor
├── erlmcp_cpu_quota
├── erlmcp_cancellation
├── erlmcp_circuit_breaker
├── erlmcp_rate_limiter
└── erlmcp_db_pool_sup  <- Database pool supervisor
    └── erlmcp_db_pool  <- Pool manager (gen_server)
        └── poolboy pools (one per configured pool)
            └── erlmcp_db_connection workers
```

## Best Practices

1. **Pool Sizing**: Set pool size based on database connection limits
2. **Timeout Configuration**: Use appropriate timeouts for your workload
3. **Circuit Threshold**: Balance between sensitivity and resilience
4. **Monitoring**: Monitor pool stats and circuit state
5. **Connection Lifecycle**: Always use `with_connection` or transactions

## Example Usage

```erlang
%% Simple query
{ok, Users} = erlmcp_db_pool:execute(default, "SELECT * FROM users"),

%% Parameterized query
{ok, User} = erlmcp_db_pool:execute(
    default,
    "SELECT * FROM users WHERE id = $1",
    [123]
),

%% Transaction
{ok, Result} = erlmcp_db_pool:transaction(default, fun(Conn) ->
    {ok, _} = erlmcp_db_pool:query(Conn, "INSERT INTO accounts ..."),
    {ok, _} = erlmcp_db_pool:query(Conn, "UPDATE balance ..."),
    {ok, done}
end),

%% With connection
{ok, Result} = erlmcp_db_pool:with_connection(default, fun(Conn) ->
    % Multiple operations with auto checkin
    {ok, _} = erlmcp_db_pool:query(Conn, "SELECT ..."),
    {ok, _} = erlmcp_db_pool:query(Conn, "SELECT ...")
end),

%% Get pool status
{ok, Stats} = erlmcp_db_pool:stats(default),

%% Circuit breaker info
{ok, CircuitInfo} = erlmcp_db_pool:circuit_breaker_info(default),
```

## Files Modified

- `apps/erlmcp_core/src/erlmcp_db_pool.erl` (new)
- `apps/erlmcp_core/src/erlmcp_db_connection.erl` (new)
- `apps/erlmcp_core/src/erlmcp_db_pool_sup.erl` (new)
- `apps/erlmcp_core/test/erlmcp_db_pool_tests.erl` (new)
- `apps/erlmcp_core/test/erlmcp_db_connection_tests.erl` (new)
- `apps/erlmcp_core/src/erlmcp_core.app.src` (modified - added modules)
- `apps/erlmcp_core/src/erlmcp_resilience_sup.erl` (modified - added db pool sup)

## Verification

The implementation has been verified to compile successfully:

```bash
$ erlc -I apps/erlmcp_core/include apps/erlmcp_core/src/erlmcp_db_pool.erl
$ erlc -I apps/erlmcp_core/include apps/erlmcp_core/src/erlmcp_db_connection.erl
$ erlc -I apps/erlmcp_core/include apps/erlmcp_core/src/erlmcp_db_pool_sup.erl
# All compiled without errors
```

Docker build verification also passed compilation stage.
