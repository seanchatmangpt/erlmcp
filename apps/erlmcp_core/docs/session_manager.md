# erlmcp_session_manager - Session Lifecycle Management

## Overview

The `erlmcp_session_manager` is a gen_server that manages the lifecycle of MCP sessions with ETS-based storage, automatic expiration, and replication hooks for distributed failover.

## Architecture

### Storage Strategy
- **ETS Table**: `ordered_set` with public access and read concurrency
- **Key**: Session ID (32-character hex string)
- **Value**: Session data map containing id, timestamps, timeout, and metadata
- **Cleanup**: Automatic timer-based expiration of idle sessions

### Supervision
Supervised by `erlmcp_core_sup` as a permanent worker with 5-second shutdown timeout.

## Session Data Structure

```erlang
session_data() :: #{
    id := session_id(),              % 32-char hex string
    created_at := integer(),          % Millisecond timestamp
    last_accessed := integer(),       % Updated on get/update
    timeout_ms := pos_integer() | infinity,
    metadata := map(),                % User-defined data
    replication_ref => reference()    % Optional failover ref
}.
```

## API Functions

### Session Creation

```erlang
%% Create session with default timeout (1 hour)
{ok, SessionId} = erlmcp_session_manager:create_session(#{user => <<"alice">>}).

%% Create session with custom timeout
{ok, SessionId} = erlmcp_session_manager:create_session(#{project => <<"test">>}, 5000).

%% Create session with no expiration
{ok, SessionId} = erlmcp_session_manager:create_session(#{}, infinity).
```

### Session Retrieval

```erlang
%% Get session (updates last_accessed)
{ok, Session} = erlmcp_session_manager:get_session(SessionId).
% {ok, #{id => <<"...">>, created_at => ..., metadata => ...}}

%% Handle nonexistent session
{error, not_found} = erlmcp_session_manager:get_session(NonExistentId).
```

### Session Updates

```erlang
%% Update with function
UpdateFun = fun(Session) ->
    Meta = maps:get(metadata, Session),
    NewMeta = Meta#{counter => maps:get(counter, Meta, 0) + 1},
    Session#{metadata => NewMeta}
end,
ok = erlmcp_session_manager:update_session(SessionId, UpdateFun).

%% Update also updates last_accessed timestamp
```

### Session Deletion

```erlang
%% Delete session (idempotent)
ok = erlmcp_session_manager:delete_session(SessionId).

%% Deleting nonexistent session succeeds
ok = erlmcp_session_manager:delete_session(NonExistentId).
```

### Session Listing

```erlang
%% List all sessions
Sessions = erlmcp_session_manager:list_sessions().
% [#{id => ..., ...}, ...]

%% List with filter
AdminSessions = erlmcp_session_manager:list_sessions(
    fun(Session) ->
        Meta = maps:get(metadata, Session),
        maps:get(type, Meta, undefined) =:= admin
    end
).
```

### Session Maintenance

```erlang
%% Touch session (update last_accessed without get)
ok = erlmcp_session_manager:touch_session(SessionId).

%% Change session timeout
ok = erlmcp_session_manager:set_timeout(SessionId, 10000).

%% Manual cleanup of expired sessions
{ok, Count} = erlmcp_session_manager:cleanup_expired().
% {ok, 3}  % 3 sessions were expired and removed
```

## Automatic Cleanup

### Cleanup Timer
- **Interval**: 60 seconds (configurable)
- **Process**: Scans all sessions, removes expired ones
- **Logging**: Debug log when sessions are removed

### Expiration Logic
```erlang
%% Session expires if:
%% (current_time - last_accessed) > timeout_ms

%% Sessions with timeout_ms = infinity never expire
```

## Replication Hooks

The session manager notifies `erlmcp_session_replicator` (if running) of session events:

- `{session_created, SessionId, SessionData}`
- `{session_updated, SessionId, SessionData}`
- `{session_deleted, SessionId}`
- `{session_expired, SessionId}`

### Failover Integration

```erlang
%% erlmcp_session_failover uses these hooks to:
%% 1. Replicate sessions to standby nodes
%% 2. Detect session ownership on node failure
%% 3. Migrate sessions during graceful shutdown
```

## Configuration

### Application Environment

```erlang
%% config/sys.config
{erlmcp_core, [
    {session_manager, [
        {default_timeout_ms, 3600000},  % 1 hour
        {cleanup_interval_ms, 60000}    % 1 minute
    ]}
]}.
```

### Defaults
- **Default Timeout**: 3,600,000 ms (1 hour)
- **Cleanup Interval**: 60,000 ms (1 minute)

## Performance Characteristics

### ETS Optimizations
- **Read Concurrency**: Enabled for high-throughput reads
- **Public Access**: Optional direct reads without gen_server call
- **Ordered Set**: Efficient iteration for cleanup

### Benchmarks (from core_ops)
- **Session Create**: ~242,000 ops/sec
- **Session Get**: ~350,000 ops/sec (direct ETS read)
- **Session Update**: ~180,000 ops/sec
- **Cleanup Scan**: <10ms for 10,000 sessions

## Concurrency

### Thread Safety
- All mutations go through gen_server (serialized)
- Reads can bypass gen_server if using public ETS
- Update functions are atomic within gen_server

### Concurrent Access
```erlang
%% Safe: Multiple processes can create sessions concurrently
ParallelCreates = [spawn(fun() ->
    {ok, _Id} = erlmcp_session_manager:create_session(#{worker => N})
end) || N <- lists:seq(1, 100)].

%% Safe: Multiple processes can read sessions concurrently
ParallelReads = [spawn(fun() ->
    {ok, _Session} = erlmcp_session_manager:get_session(SessionId)
end) || _ <- lists:seq(1, 100)].
```

## Error Handling

### Error Types
- `{error, not_found}` - Session does not exist
- `{error, {update_failed, Reason}}` - Update function threw exception

### Let-It-Crash Philosophy
- Invalid session IDs: Return `{error, not_found}`
- Update failures: Catch and return error, don't crash gen_server
- Replication errors: Log and continue (replicator is optional)

## Testing

### Test Coverage: 100%

```bash
# Run tests
rebar3 eunit --module=erlmcp_session_manager_tests

# Tests include:
# - Session CRUD operations
# - Expiration and cleanup
# - Concurrency and uniqueness
# - Metadata operations
# - Replication hooks
# - Edge cases and error handling
```

### Key Test Scenarios
1. **Basic CRUD**: Create, read, update, delete sessions
2. **Expiration**: Short-lived sessions are cleaned up
3. **Concurrency**: 100 parallel session creations
4. **Metadata**: Various data types in session metadata
5. **Infinite Timeout**: Sessions that never expire
6. **Cleanup Timer**: Automatic periodic cleanup
7. **Replication**: Hook notifications (no-op if replicator not running)

## OTP Patterns

### gen_server Callbacks
```erlang
%% All 6 callbacks implemented:
init/1              % Create ETS table, start cleanup timer
handle_call/3       % Synchronous session operations
handle_cast/2       % Async operations (currently unused)
handle_info/2       % Cleanup timer messages
terminate/2         % Cancel timer, delete ETS table
code_change/3       % Hot code reload support
```

### State Record
```erlang
-record(state, {
    table :: ets:tid(),
    cleanup_timer :: reference() | undefined,
    cleanup_interval_ms = 60000 :: pos_integer(),
    default_timeout_ms = 3600000 :: pos_integer()
}).
```

## Integration Examples

### Client Integration
```erlang
%% Store client session
{ok, SessionId} = erlmcp_session_manager:create_session(#{
    client_id => ClientId,
    transport => tcp,
    capabilities => Capabilities
}),

%% Update on activity
erlmcp_session_manager:update_session(SessionId, fun(Session) ->
    Meta = maps:get(metadata, Session),
    Session#{metadata => Meta#{last_request => erlang:system_time(millisecond)}}
end).
```

### Server Integration
```erlang
%% Track active MCP servers
{ok, SessionId} = erlmcp_session_manager:create_session(#{
    server_id => ServerId,
    phase => initialized,
    resources => ResourceMap
}, infinity).  % Server sessions don't expire
```

## Monitoring

### Observability
```erlang
%% Get session count
SessionCount = length(erlmcp_session_manager:list_sessions()).

%% Get sessions by type
ActiveSessions = erlmcp_session_manager:list_sessions(
    fun(#{metadata := Meta}) ->
        maps:get(active, Meta, false) =:= true
    end
).

%% Check oldest session
Sessions = erlmcp_session_manager:list_sessions(),
OldestSession = lists:min([
    maps:get(created_at, S) || S <- Sessions
]).
```

### Metrics
- Session count (total, by type)
- Session age distribution
- Cleanup frequency and count
- Memory usage (ETS table size)

## Future Enhancements

### Planned Features (v2.2.0+)
1. **Mnesia Backend**: Optional persistent storage
2. **LRU Eviction**: Capacity-based limits
3. **Session Groups**: Bulk operations on session sets
4. **Distributed Sessions**: Cross-node session sharing
5. **Metrics Export**: Prometheus/OTEL integration

### Migration Path
Current ETS-based design allows zero-downtime migration to Mnesia:
1. Start dual-write (ETS + Mnesia)
2. Backfill existing sessions to Mnesia
3. Switch reads to Mnesia
4. Remove ETS writes

## References

- OTP Patterns: `docs/otp-patterns.md`
- Session Replication: `docs/SESSION_REPLICATION_SYSTEM.md`
- Failover Module: `erlmcp_session_failover.erl`
- Benchmark Results: `bench/results/core_ops_*.json`
