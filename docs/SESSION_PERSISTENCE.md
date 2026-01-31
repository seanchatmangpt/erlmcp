# Session Persistence - erlmcp

## Architecture Overview

erlmcp provides a flexible session persistence layer with pluggable backends. Sessions can be stored in-memory (ETS), on-disk (DETS), or in a distributed database (Mnesia) depending on your durability and scalability requirements.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    erlmcp_session_manager                    │
│                     (gen_server)                             │
└────────────────────────┬────────────────────────────────────┘
                         │
                         │ delegates to
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  erlmcp_session_backend                      │
│                  (behavior interface)                        │
└────┬─────────────┬─────────────┬────────────────────────────┘
     │             │             │
     │ implements  │ implements  │ implements
     ▼             ▼             ▼
┌──────────┐ ┌──────────┐ ┌──────────┐
│    ETS   │ │   DETS   │ │  Mnesia  │
│ (memory) │ │  (disk)  │ │(cluster) │
└──────────┘ └──────────┘ └──────────┘
```

### Session Data Model

```erlang
#{
    id => <<"session_abc123">>,              % Unique session ID
    created_at => 1640995200000,             % Creation timestamp (ms)
    last_accessed => 1640995300000,          % Last access timestamp (ms)
    timeout_ms => 300000,                    % Session timeout (5 minutes)
    metadata => #{
        user_id => <<"user_456">>,
        client_info => #{...}
    }
}
```

## Backend Comparison

| Backend | Type | Use Case | Performance | Durability | Scalability |
|---------|------|----------|-------------|------------|-------------|
| **ETS** | In-memory | Fast access, development | ⚡ Highest | ❌ Lost on restart | ❌ Single node |
| **DETS** | Disk storage | Simple persistence | 🚀 High | ✅ Survives restart | ❌ Single node |
| **Mnesia** | Distributed | Production clusters | ✓ Good | ✅ Configurable | ✅ Multi-node |

### Performance Characteristics

**ETS (In-Memory)**
- Read: ~1-5 µs
- Write: ~1-5 µs
- Capacity: Limited by RAM
- Concurrency: Excellent (lock-free reads)

**DETS (Disk)**
- Read: ~100-500 µs
- Write: ~1-5 ms (with auto_save)
- Capacity: ~2 GB per file
- Concurrency: Good (file-level locks)

**Mnesia (Distributed)**
- Read: ~50-200 µs (RAM), ~1-5 ms (disk)
- Write: ~1-10 ms (transactional)
- Capacity: Unlimited (clustered)
- Concurrency: Excellent (sharding)

## Configuration Examples

### ETS Backend (Default)

```erlang
%% config/sys.config
{erlmcp_session, [
    {backend, erlmcp_session_ets},
    {backend_opts, #{
        table_name => erlmcp_sessions_ets,
        cleanup_interval => 60000  % Clean expired sessions every 60s
    }}
]}.
```

**Programmatic:**
```erlang
{ok, BackendPid} = erlmcp_session_backend:start_link(#{
    backend => erlmcp_session_ets,
    table_name => erlmcp_sessions_ets,
    cleanup_interval => 60000
}).
```

### DETS Backend (Disk Persistence)

```erlang
%% config/sys.config
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{
        table_name => erlmcp_sessions_dets,
        file_path => "data/erlmcp_sessions.dets",
        auto_save => 60000,  % Flush to disk every 60s
        cleanup_interval => 60000
    }}
]}.
```

**Programmatic:**
```erlang
{ok, BackendPid} = erlmcp_session_backend:start_link(#{
    backend => erlmcp_session_dets,
    table_name => erlmcp_sessions_dets,
    file_path => "data/erlmcp_sessions.dets",
    auto_save => 60000,
    cleanup_interval => 60000
}).
```

### Mnesia Backend (Clustered)

```erlang
%% config/sys.config
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        table_name => erlmcp_session,
        nodes => [node1@host, node2@host, node3@host],
        disc_copies => true,  % true = disk+RAM, false = RAM only
        cleanup_interval => 60000
    }}
]}.
```

**Programmatic:**
```erlang
%% Ensure Mnesia is running on all nodes
mnesia:start(),
mnesia:change_config(extra_db_nodes, [node2@host, node3@host]),

%% Create distributed table
{ok, BackendPid} = erlmcp_session_backend:start_link(#{
    backend => erlmcp_session_mnesia,
    table_name => erlmcp_session,
    nodes => [node1@host, node2@host, node3@host],
    disc_copies => true,  % Disk + RAM for durability
    cleanup_interval => 60000
}).
```

## API Reference

### Backend Behavior

All session backends implement the `erlmcp_session_backend` behavior:

```erlang
-callback init(map()) -> {ok, State} | {error, term()}.
-callback store(session_id(), session(), State) ->
    {ok, NewState} | {error, term()}.
-callback fetch(session_id(), State) ->
    {ok, session(), NewState} | {error, not_found | term(), NewState}.
-callback delete(session_id(), State) ->
    {ok, NewState} | {error, term(), NewState}.
-callback list(State) ->
    {ok, [session_id()], NewState}.
-callback cleanup_expired(State) ->
    {ok, Count, NewState}.
```

### Backend API

```erlang
%% Start backend
-spec start_link(backend_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    %% Opts: #{backend => module(), cleanup_interval => pos_integer(), ...}

%% Store session
-spec store(session_id(), session()) -> ok | {error, term()}.
store(SessionId, Session) ->
    gen_server:call(BackendPid, {store, SessionId, Session}).

%% Fetch session
-spec fetch(session_id()) -> {ok, session()} | {error, not_found | term()}.
fetch(SessionId) ->
    gen_server:call(BackendPid, {fetch, SessionId}).

%% Delete session
-spec delete(session_id()) -> ok | {error, term()}.
delete(SessionId) ->
    gen_server:call(BackendPid, {delete, SessionId}).

%% List all sessions
-spec list() -> {ok, [session_id()]}.
list() ->
    gen_server:call(BackendPid, list).

%% Clean up expired sessions
-spec cleanup_expired() -> {ok, Count :: non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(BackendPid, cleanup_expired).
```

## Migration Guide

### ETS → DETS Migration

**Why migrate?**
- ETS sessions are lost on restart
- DETS provides persistent storage
- Minimal performance impact

**Steps:**

1. **Backup existing ETS data** (optional):
```erlang
%% Export ETS sessions to file
{ok, BackendPid} = erlmcp_session_backend:start_link(#{backend => erlmcp_session_ets}),
{ok, SessionIds} = erlmcp_session_backend:list(),
Sessions = [{Id, erlmcp_session_backend:fetch(Id)} || Id <- SessionIds],
file:write_file("sessions_backup", term_to_binary(Sessions)).
```

2. **Update configuration**:
```erlang
%% Change backend from ETS to DETS
{erlmcp_session, [
    {backend, erlmcp_session_dets},  % Changed from erlmcp_session_ets
    {backend_opts, #{
        file_path => "data/erlmcp_sessions.dets"  % New option
    }}
]}.
```

3. **Restart application**:
```bash
rebar3 shell
%% Sessions will now persist to disk
```

4. **Verify migration**:
```erlang
%% Check sessions are loaded
{ok, SessionIds} = erlmcp_session_backend:list(),
length(SessionIds).  % Should match pre-migration count
```

### DETS → Mnesia Migration

**Why migrate?**
- Need multi-node clustering
- Require ACID transactions
- Want horizontal scalability

**Steps:**

1. **Set up Mnesia cluster**:
```erlang
%% On all nodes
mnesia:create_schema([node1@host, node2@host, node3@host]),
mnesia:start().
```

2. **Export DETS sessions**:
```erlang
{ok, SessionIds} = erlmcp_session_backend:list(),
Sessions = [{Id, {ok, S}} || Id <- SessionIds, S <- [erlmcp_session_backend:fetch(Id)]].
```

3. **Update configuration**:
```erlang
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        nodes => [node1@host, node2@host, node3@host],
        disc_copies => true
    }}
]}.
```

4. **Import sessions to Mnesia**:
```erlang
%% Import exported sessions
lists:foreach(fun({SessionId, {ok, Session}}) ->
    erlmcp_session_backend:store(SessionId, Session)
end, Sessions).
```

5. **Verify replication**:
```erlang
%% On node2@host
mnesia:table_info(erlmcp_session, where_to_read).  % Should show all nodes
```

## Advanced Topics

### Session Expiration

Sessions expire based on `timeout_ms` and `last_accessed` timestamp:

```erlang
%% Check if session is expired
is_expired(Session) ->
    Now = erlang:system_time(millisecond),
    LastAccessed = maps:get(last_accessed, Session),
    TimeoutMs = maps:get(timeout_ms, Session),
    (Now - LastAccessed) > TimeoutMs.
```

The backend automatically runs `cleanup_expired/0` at the configured interval.

### Custom Backend Implementation

```erlang
-module(erlmcp_session_leveldb).
-behavior(erlmcp_session_backend).

%% Backend callbacks
-export([init/1, store/3, fetch/2, delete/2, list/1, cleanup_expired/1]).

-record(state, {db_ref :: term()}).

init(Opts) ->
    DbPath = maps:get(db_path, Opts, "data/sessions.leveldb"),
    {ok, DbRef} = eleveldb:open(DbPath, [{create_if_missing, true}]),
    {ok, #state{db_ref = DbRef}}.

store(SessionId, Session, State) ->
    Key = SessionId,
    Value = term_to_binary(Session),
    ok = eleveldb:put(State#state.db_ref, Key, Value),
    {ok, State}.

fetch(SessionId, State) ->
    case eleveldb:get(State#state.db_ref, SessionId) of
        {ok, Value} ->
            Session = binary_to_term(Value),
            {ok, Session, State};
        not_found ->
            {error, not_found, State}
    end.

delete(SessionId, State) ->
    ok = eleveldb:delete(State#state.db_ref, SessionId),
    {ok, State}.

list(State) ->
    Iter = eleveldb:iterator(State#state.db_ref),
    SessionIds = collect_keys(Iter, []),
    eleveldb:close_iterator(Iter),
    {ok, SessionIds, State}.

cleanup_expired(State) ->
    %% Implementation required
    {ok, 0, State}.
```

### Failover and Replication

Mnesia supports automatic failover:

```erlang
%% Configure disc_copies on multiple nodes
Opts = #{
    backend => erlmcp_session_mnesia,
    nodes => [node1@host, node2@host, node3@host],
    disc_copies => true  % Each node keeps disk copy
}.

%% If node1@host fails:
%% - Sessions still available on node2@host and node3@host
%% - Reads/writes continue transparently
%% - When node1@host recovers, it syncs from other nodes
```

## Troubleshooting

### DETS File Corruption

```erlang
%% Symptoms: {error, {corrupt, ...}}
%% Solution: Repair DETS file
dets:open_file(erlmcp_sessions_dets, [{file, "data/erlmcp_sessions.dets"}, {repair, true}]).
```

### Mnesia Table Loading Issues

```erlang
%% Symptoms: {aborted, {no_exists, ...}}
%% Solution: Wait for Mnesia tables to load
mnesia:wait_for_tables([erlmcp_session], 5000).
```

### Performance Tuning

```erlang
%% ETS: Increase read concurrency
ets:new(erlmcp_sessions_ets, [
    ordered_set,
    {read_concurrency, true},
    {write_concurrency, true}  % Enable write concurrency
]).

%% DETS: Adjust auto_save interval
{auto_save, 30000}  % Flush every 30s (more frequent = better durability, slower)

%% Mnesia: Use ram_copies for hot data
{ram_copies, [node()]}  % Faster but not durable
```

## Best Practices

1. **Development**: Use ETS for fastest iteration
2. **Single-node production**: Use DETS for persistence
3. **Multi-node production**: Use Mnesia with disc_copies
4. **High performance**: Use Mnesia with ram_copies + periodic snapshots
5. **Backup**: Regularly export sessions regardless of backend
6. **Monitoring**: Track session counts and expiration rates
7. **Cleanup**: Configure appropriate cleanup intervals (60s default)
8. **Timeouts**: Set reasonable session timeouts (5-30 minutes)

## References

- [Erlang ETS Documentation](https://erlang.org/doc/man/ets.html)
- [Erlang DETS Documentation](https://erlang.org/doc/man/dets.html)
- [Erlang Mnesia Documentation](https://erlang.org/doc/man/mnesia.html)
- `apps/erlmcp_core/src/erlmcp_session_backend.erl`
- `apps/erlmcp_core/src/erlmcp_session_ets.erl`
- `apps/erlmcp_core/src/erlmcp_session_dets.erl`
- `apps/erlmcp_core/src/erlmcp_session_mnesia.erl`
