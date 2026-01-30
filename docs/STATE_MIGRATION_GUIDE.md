# State Migration Guide for Hot Code Loading

## Overview

This guide describes the state versioning and migration system implemented in erlmcp for safe hot code loading during runtime upgrades.

## Architecture

### State Version Records

All gen_server modules now include a `version` field in their state records:

```erlang
-record(state, {
    version = v1 :: v1 | v2 | v3,  % State version for hot code loading
    % ... other fields
}).
```

### Migration Functions

Each module implements `code_change/3` with actual migration logic:

```erlang
code_change(OldVsn, State, Extra) ->
    try
        logger:info("~p: Code change from ~p", [?MODULE, OldVsn]),
        NewState = migrate_state(OldVsn, State, Extra),
        logger:info("~p: Code change completed", [?MODULE]),
        {ok, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("~p: Code change failed: ~p:~p", [?MODULE, Class, Reason]),
            error({code_change_failed, Class, Reason})
    end.
```

## Module Migration Paths

### erlmcp_rate_limiter

**Current Version:** v1

**State Record:**
```erlang
-record(state, {
    version = v1 :: v1 | v2,
    config :: #{atom() => any()},
    clients :: ets:table(),
    global_bucket :: token_bucket(),
    violations :: ets:table(),
    last_cleanup :: integer(),
    _v2_reserved = undefined :: term()
}).
```

**Migration Path:**
- `legacy (no version)` → `v1`: Add version field
- `v1` → `v2` (future): Add new fields, migrate ETS tables

**ETS Tables:**
- `rate_limit_clients` - Client rate limit state
- `rate_limit_violations` - DDoS violation tracking

**Upgrade Procedure:**
```erlang
%% 1. Load new code
l(erlmcp_rate_limiter).

%% 2. Trigger code change on running process
sys:suspend(erlmcp_rate_limiter).
sys:change_code(erlmcp_rate_limiter, OldVsn, Extra).
sys:resume(erlmcp_rate_limiter).
```

### erlmcp_session_manager

**Current Version:** v1

**State Record:**
```erlang
-record(state, {
    version = v1 :: v1 | v2,
    table :: ets:tid(),
    cleanup_timer :: reference() | undefined,
    cleanup_interval_ms = 60000 :: pos_integer(),
    default_timeout_ms = 3600000 :: pos_integer(),
    _v2_reserved = undefined :: term()
}).
```

**Migration Path:**
- `legacy (no version)` → `v1`: Add version field
- `v1` → `v2` (future): Add session replication fields

**ETS Tables:**
- `erlmcp_sessions` - Session data (ordered_set)

### erlmcp_cache

**Current Version:** v1

**State Record:**
```erlang
-record(state, {
    version = v1 :: v1 | v2,
    l1_table :: ets:tid(),
    l2_enabled = false :: boolean(),
    l3_enabled = false :: boolean(),
    l3_module :: module() | undefined,
    l3_conn :: term() | undefined,
    max_l1_size = 10000 :: pos_integer(),
    max_l2_size = 100000 :: pos_integer(),
    default_ttl_seconds = 300 :: pos_integer(),
    cleanup_interval_ms = 60000 :: pos_integer(),
    cleanup_timer :: reference() | undefined,
    stats = #{...} :: map(),
    _v2_reserved = undefined :: term()
}).
```

**Migration Path:**
- `legacy (no version)` → `v1`: Add version field
- `v1` → `v2` (future): Add cache coherence fields

**ETS Tables:**
- `erlmcp_cache_l1` - L1 cache (set, public)
- `erlmcp_cache_l2` - L2 cache (Mnesia)

## Migration Utilities

### erlmcp_state_migration Module

Provides utilities for safe state migration:

```erlang
%% Generic code_change implementation
code_change(Module, OldState, OldVsn, Extra, MigrationPath) ->
    {ok, NewState} | {error, Reason}

%% Migrate state through version path
migrate_state(State, TargetVersion, MigrationPath) -> NewState

%% Migrate ETS table schema
migrate_ets_table(Table, Module, FromVersion, ToVersion, TransformFun) -> ok

%% Backup ETS table
backup_ets_table(SourceTable, Module) -> BackupTable

%% Restore from backup
restore_ets_table(TargetTable, BackupTable, Module) -> ok

%% Validate migrated state
validate_migration(Module, State) -> ok | {error, Reason}
```

## ETS Table Migration

### Backup and Restore Pattern

```erlang
%% 1. Backup original table
BackupTable = erlmcp_state_migration:backup_ets_table(SourceTable, ?MODULE),

try
    %% 2. Transform data
    ets:foldl(fun({Key, Value}, _) ->
        NewValue = transform_value(Value),
        ets:insert(SourceTable, {Key, NewValue})
    end, ok, SourceTable),

    %% 3. Verify migration
    ok
catch
    error:Reason ->
        %% 4. Restore from backup on failure
        erlmcp_state_migration:restore_ets_table(SourceTable, BackupTable, ?MODULE),
        error({migration_failed, Reason})
after
    %% 5. Cleanup backup
    ets:delete(BackupTable)
end.
```

### Data Transformation

```erlang
%% Transform function for schema changes
TransformFun = fun({Key, OldValue}) ->
    case OldValue of
        {old_format, Data} ->
            {Key, {new_format, upgraded, Data}};
        _ ->
            {Key, OldValue}
    end
end,

erlmcp_state_migration:migrate_ets_table(
    Table, ?MODULE, v1, v2, TransformFun).
```

## Best Practices

### 1. Version Field Always First

```erlang
-record(state, {
    version = v1 :: v1 | v2,  % Always first for easy pattern matching
    field1 :: type1(),
    field2 :: type2()
}).
```

### 2. Reserved Fields for Future Versions

```erlang
-record(state, {
    version = v1,
    % ... current fields
    _v2_reserved = undefined :: term(),  % Reserved for v2
    _v3_reserved = undefined :: term()   % Reserved for v3
}).
```

### 3. Incremental Migrations

```erlang
migrate_state(OldVsn, State, Extra) ->
    case {OldVsn, State#state.version} of
        {_, v3} -> State;  % Already at target
        {v2, _} -> migrate_v2_to_v3(State);
        {v1, _} -> migrate_v1_to_v2(migrate_v1(State));
        {_, _}   -> migrate_legacy(State)
    end.
```

### 4. Comprehensive Logging

```erlang
code_change(OldVsn, State, Extra) ->
    logger:info("~p: Starting migration from ~p", [?MODULE, OldVsn]),
    try
        NewState = do_migration(OldVsn, State, Extra),
        logger:info("~p: Migration completed successfully", [?MODULE]),
        {ok, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("~p: Migration failed: ~p:~p~n~p",
                        [?MODULE, Class, Reason, Stack]),
            error({migration_failed, Class, Reason})
    end.
```

### 5. Data Validation

```erlang
validate_migration(Module, State) ->
    case State of
        #state{version = Ver} when is_atom(Ver) ->
            ok;
        _ ->
            {error, {invalid_state_format, State}}
    end.
```

## Testing

### Unit Tests

```erlang
%% Test legacy state upgrade
legacy_state_test() ->
    LegacyState = #{config => #{}, clients => ets:new(...)},
    {ok, NewState} = my_module:code_change([], LegacyState, []),
    ?assertEqual(v1, NewState#state.version).

%% Test current version unchanged
current_state_test() ->
    CurrentState = #state{version = v1},
    {ok, NewState} = my_module:code_change([], CurrentState, []),
    ?assertEqual(CurrentState, NewState).
```

### Integration Tests

```erlang
%% Test hot code reload cycle
hot_reload_test() ->
    Pid = start_server(),
    sys:suspend(Pid),
    sys:change_code(Pid, my_module, [], []),
    sys:resume(Pid),
    {ok, State} = sys:get_state(Pid),
    ?assertEqual(v1, State#state.version).
```

### Performance Tests

```erlang
%% Test migration performance with large tables
large_table_migration_test() ->
    Table = create_large_table(10000),
    StartTime = erlang:monotonic_time(microsecond),
    ok = migrate_table(Table),
    Duration = erlang:monotonic_time(microsecond) - StartTime,
    ?assert(Duration < 5_000_000).  % Less than 5 seconds
```

## Rollback Procedures

### Downgrade Migration

```erlang
code_change({down, FromVsn}, State, _Extra) ->
    logger:info("~p: Downgrading from ~p", [?MODULE, FromVsn]),
    case FromVsn of
        v2 -> downgrade_v2_to_v1(State);
        v1 -> State;  % Already at v1
        _  -> State
    end.
```

### Verification Steps

1. Verify all processes have correct version
2. Verify ETS table schemas match
3. Run integration tests
4. Monitor error logs
5. Check performance metrics

## Troubleshooting

### Common Issues

**Issue:** Migration fails with `{invalid_state_format, State}`
**Solution:** Check state record definition matches actual data

**Issue:** ETS table restore fails
**Solution:** Ensure backup table has same type and keypos

**Issue:** Performance degradation after upgrade
**Solution:** Check ETS table size and query patterns

### Debug Commands

```erlang
%% Check current state version
sys:get_state(MyProcess).

%% Check ETS table info
ets:info TableName.

%% Enable debug logging
logger:set_module_level(erlmcp_rate_limiter, debug).

%% Trace code_change calls
erlang:trace(Process, true, [call]),
erlang:trace_pattern({erlmcp_rate_limiter, code_change, 3}, [], [local]).
```

## Future Enhancements

1. **Automatic schema versioning** - ETS tables with version metadata
2. **Cluster-wide upgrades** - Coordinate upgrades across nodes
3. **Blue-green deployments** - Run old and new versions in parallel
4. **Rollback automation** - Automatic rollback on failure detection
5. **Migration planning** - Pre-validate migrations before deployment

## References

- [Erlang OTP Design Principles](http://erlang.org/doc/design_principles/code_loading.html)
- [gen_server Behavior](http://erlang.org/doc/man/gen_server.html#Module:code_change-3)
- [ETS Tables](http://erlang.org/doc/man/ets.html)
- [Sys Module](http://erlang.org/doc/man/sys.html)
