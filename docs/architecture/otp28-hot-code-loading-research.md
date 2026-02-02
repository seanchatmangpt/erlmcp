# OTP 26-28 Hot Code Loading Improvements - Research Report

**Date**: 2026-02-01
**Erlang/OTP Versions**: 26, 27, 28.3.1
**Research Focus**: Hot code loading improvements, module replacement features, reload mechanisms

## Executive Summary

OTP 26-28 introduce significant improvements to hot code loading, particularly:
1. **OTP 27**: `prepare_loading/1` and `finish_loading/1` for atomic reloads
2. **OTP 26**: Enhanced `on_load` directive handling during boot
3. **OTP 27**: Default "strict" code path mode (scheduled for removal in OTP 28)
4. **Native coverage**: JIT-based coverage instrumentation (OTP 27)

## Key Improvements by OTP Version

### OTP 26.0 - `on_load` Boot Improvements

**Source**: [OTP 26.0 Release Notes](https://www.erlang.org/patches/otp-26.0)

> "Handling of on_load modules during boot has been improved by adding an extra step in the boot order for embedded mode that runs all on_load..."

**Impact**:
- `-on_load/1` functions now execute in a dedicated boot phase
- Prevents race conditions during system initialization
- Better ordering of module loading in embedded mode

### OTP 27.0 - Atomic Code Loading

**New Functions**: `code:prepare_loading/1` and `code:finish_loading/1`

**Documentation**: [code module v10.5](https://www.erlang.org/doc/apps/kernel/code.html)

```erlang
% Atomic two-phase loading
{ok, Prepared} = code:prepare_loading(Modules),
% Put the application into an inactive state
ok = code:finish_loading(Prepared),
% Resume the application
```

**Key Benefits**:
- **Minimizes downtime**: Prepare phase validates all modules before loading
- **Atomic switch**: Either all modules load or none do
- **Better for production**: Reduces time window where system is partially upgraded

**Error Reasons**:
- `badfile` - Incorrect format or module name mismatch
- `nofile` - No object code exists
- `on_load_not_allowed` - Module contains `-on_load` function
- `duplicated` - Module included multiple times
- `not_purged` - Old code still exists
- `sticky_directory` - Module in sticky directory

### OTP 27.0 - Strict Code Path Mode (Default)

**Documentation**: [code module - code path choice](https://www.erlang.org/doc/apps/kernel/code.html)

**Change**: Default changed to `"strict"` in OTP 27, scheduled for removal in OTP 28

**Behavior**:
- **strict**: Code path is exactly as specified (no fallback to archives)
- **relaxed**: Code server chooses suitable directory based on file structure

**Migration Path**:
```erlang
% Old OTP 26 behavior (relaxed)
% Use explicit code paths in OTP 27+
% The -code_path_choice flag is deprecated in OTP 28
```

**Impact on erlmcp**:
- No action needed if using standard code paths
- Ensure all module paths are explicit in release configs

### OTP 27.0 - Native Coverage Support

**Source**: [Kernel code module - Native Coverage](https://www.erlang.org/doc/apps/kernel/code.html)

**New BIFs**:
- `code:coverage_support() -> boolean()`
- `code:set_coverage_mode(Mode)`
- `code:get_coverage(Level, Module)`

**Coverage Modes**:
- `none` - No coverage (default)
- `function` - Track which functions executed (negligible overhead)
- `function_counters` - Count executions per function
- `line` - Track which lines executed (requires `line_coverage` compile option)
- `line_counters` - Count executions per line

**Impact**:
- Lightweight alternative to `cover` tool
- Useful for production monitoring
- Can track coverage during runtime without module reload

## Module Versioning Improvements

### `-vsn()` Attribute

**Documentation**: [Modules - vsn attribute](https://www.erlang.org/doc/system/modules.html)

```erlang
-vsn("2.1.0").
% vsn defaults to MD5 checksum if not specified
```

**Best Practices**:
1. **Always specify `-vsn()`** for modules using `code_change/3`
2. Use semantic versioning for releases
3. Track version in `module_info(attributes)`

### Module Status Detection

**New in OTP 26+**: `code:module_status/1`

**Returns**:
- `not_loaded` - Module not in code server
- `loaded` - Module loaded, code matches disk
- `modified` - Module loaded, but disk has different MD5
- `removed` - Module loaded, but no file on disk

**Usage**:
```erlang
case code:module_status(Module) of
    modified -> % New version available on disk
        reload_module(Module);
    loaded -> ok;
    _ -> error
end
```

## Code Replacement Best Practices (OTP 28)

### Synchronized Code Replacement

**Documentation**: [Release Handling](https://www.erlang.org/doc/system/release_handling.html)

**When to use**:
- Changing internal state format (e.g., adding field to record)
- Requires process state transformation via `code_change/3`

**Instructions**:
```erlang
% .appup file
{"2",
 [{"1", [{update, Module, {advanced, Extra}}]}],
 [{"1", [{update, Module, {advanced, Extra}}]}]}
```

**Example**:
```erlang
% Upgrade: add counter to state
code_change(_Vsn, OldState, _Extra) ->
    {ok, {OldState, 0}}.

% Downgrade: remove counter
code_change({down, _Vsn}, {State, _Count}, _Extra) ->
    {ok, State}.
```

### Simple Code Replacement

**When to use**:
- Adding new functions
- Bug fixes
- No state changes

**Instruction**:
```erlang
% .appup file
{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]}
```

### Module Dependencies

**Critical**: Load order matters when modules depend on each other

**Example**:
```erlang
% m1 calls new function in ch3
% ch3 must be loaded FIRST

{"2",
 [{"1", [
   {load_module, ch3},
   {load_module, m1, [ch3]}  % DepMods=[ch3]
 ]}],
 [{"1", [
   {load_module, m1, [ch3]},
   {load_module, ch3}
 ]}]}
```

**Note**: `systools:make_relup/3` automatically reverses order for downgrades

## Current erlmcp Code Reloading Analysis

### Existing Implementation

**Module**: `apps/erlmcp_core/src/erlmcp_code_reload.erl`

**Current Features**:
- [x] Module reload with validation
- [x] Backup/restore mechanism
- [x] Rollback window (configurable)
- [x] Dependency-based reload ordering
- [x] State migration via `code_change/3`
- [x] Integration with `erlmcp_graceful_drain`

**Current Limitations**:
- [ ] Does NOT use `prepare_loading/1` / `finish_loading/1` (OTP 27+)
- [ ] No atomic multi-module reload
- [ ] Manual backup to `.backup` files (could use `get_object_code/1`)
- [ ] No coverage tracking during reload
- [ ] No module status detection (`code:module_status/1`)

### Current State Management

**Record**: `#state{version, reload_history, rollback_timers, draining}`

**Version**: `?STATE_VERSION = 1`

**Migration**:
```erlang
migrate_state_v0_to_v1(State) ->
    % v0: no version field
    % v1: added version field
    State#state{version = ?STATE_VERSION}
```

**Test Coverage**: 18 EUnit tests in `erlmcp_code_reload_migration_tests.erl`

## Recommendations for OTP 28 Optimization

### 1. Adopt Two-Phase Loading (High Priority)

**Before** (current):
```erlang
perform_code_reload(Module) ->
    code:soft_purge(Module),
    code:load_file(Module).
```

**After** (OTP 27+ optimized):
```erlang
reload_modules_atomic(Modules) ->
    % Phase 1: Validate all modules
    {ok, Prepared} = code:prepare_loading(Modules),

    % Phase 2: Drain connections
    drain_connections(Modules),

    % Phase 3: Atomic load
    case code:finish_loading(Prepared) of
        ok -> ok;
        {error, Reasons} ->
            logger:error("Atomic reload failed: ~p", [Reasons]),
            {error, Reasons}
    end.
```

**Benefits**:
- Zero partial upgrade window
- Validates all modules before any code changes
- Better error handling with detailed reasons

### 2. Add Module Status Detection

**Implementation**:
```erlang
detect_modified_modules() ->
    Modified = code:modified_modules(),
    lists:filter(fun(Mod) ->
        case code:module_status(Mod) of
            modified -> true;
            _ -> false
        end
    end, Modified).
```

**Use Case**: Automated reload of modified modules during development

### 3. Leverage Native Coverage for Reload Verification

**Current**: Smoke tests use user-provided functions
**Proposed**: Add native coverage checks

```erlang
verify_reload_coverage(Module) ->
    case code:get_coverage_mode() of
        none ->
            logger:warning("Coverage not enabled, skipping checks"),
            ok;
        Mode ->
            {ok, Coverage} = code:get_coverage(function, Module),
            % Verify critical functions executed
            verify_critical_functions_covered(Coverage)
    end.
```

**Benefits**:
- Detect if new code is actually being used
- Verify hot reload succeeded without manual tests
- Production-friendly monitoring

### 4. Enhanced Backup via `get_object_code/1`

**Current**: File-based `.backup` copies
**Proposed**: In-memory binary backups

```erlang
backup_module_binary(Module) ->
    case code:get_object_code(Module) of
        {Module, Binary, Filename} ->
            store_backup(Module, Binary, Filename);
        error ->
            {error, module_not_found}
    end.

restore_from_binary(Module, Binary, Filename) ->
    code:purge(Module),
    code:load_binary(Module, Filename, Binary).
```

**Benefits**:
- No disk I/O
- Faster rollback
- Works in read-only environments

### 5. Enhanced `code_change/3` Patterns

**Recommendation**: Use `Extra` parameter for version-specific migration

```erlang
% Pattern 1: State transformation
code_change({down, 1}, State1, _Extra) ->
    {ok, downgrade_state_v1_to_v0(State1)};
code_change(1, State0, _Extra) ->
    {ok, upgrade_state_v0_to_v1(State0)};

% Pattern 2: Using Extra for context
code_change(_Vsn, OldState, {add_field, FieldName, Default}) ->
    {ok, maps:put(FieldName, Default, OldState)};
code_change(_Vsn, OldState, {remove_field, FieldName}) ->
    {ok, maps:remove(FieldName, OldState)}.
```

## Breaking Changes and Migration

### OTP 27 Breaking Changes

1. **Code path choice default changed to "strict"**
   - **Impact**: Systems relying on archive fallback may fail
   - **Fix**: Use explicit code paths or set `-code_path_choice relaxed`

2. **`code:lib_dir/2` deprecated**
   - **Impact**: Archive-based code loading
   - **Fix**: Use `code:lib_dir/1` and `filename:join/2`

### OTP 28 Scheduled Removals

1. **`-code_path_choice` flag** (remove in OTP 28)
   - **Migration**: Ensure all code paths are explicit

2. **Archive loading behavior changes**
   - **Migration**: Use `escript:extract/2` for archive data files

## Performance Optimization Opportunities

### 1. Batch Reload with Dependency Graph

**Current**: Sequential reload
**Proposed**: Topological sort with atomic batch

```erlang
reload_batch(Modules) ->
    Graph = build_dependency_graph(Modules),
    {ok, Sorted} = topological_sort(Graph),
    {ok, Prepared} = code:prepare_loading(Sorted),
    code:finish_loading(Prepared).
```

### 2. Concurrent Smoke Tests

**Current**: Sequential test execution
**Proposed**: Parallel test execution with `async`/`await`

```erlang
run_smoke_tests_parallel(SmokeTests) ->
    Pids = [spawn_monitor(Test) || Test <- SmokeTests],
    await_results(Pids).
```

### 3. Lazy Rollback Timer Cleanup

**Current**: Timer per module
**Proposed**: Single timer with priority queue

```erlang
% More efficient for many modules
start_rollback_timer(ExpiryMap) ->
    erlang:send_after(next_expiry(ExpiryMap), self(), rollback_expiry_tick).
```

## Implementation Plan

### Phase 1: Adopt OTP 27+ APIs (Immediate)

1. Replace `code:load_file/1` with `prepare_loading/1` + `finish_loading/1`
2. Add `code:module_status/1` for detection
3. Implement in-memory binary backups

### Phase 2: Enhanced Verification (Short-term)

1. Add native coverage checks
2. Implement parallel smoke tests
3. Add module status monitoring

### Phase 3: Advanced Features (Long-term)

1. Supervision tree-aware reload
2. Distributed node coordination
3. Automated rollback based on health metrics

## Testing Strategy

### Unit Tests
- [x] State migration (existing 18 tests)
- [ ] Two-phase loading error cases
- [ ] Module status detection
- [ ] Binary backup/restore

### Integration Tests
- [ ] End-to-end upgrade/downgrade
- [ ] Concurrent reload stress test
- [ ] Rollback scenarios
- [ ] Coverage verification

### Property-Based Tests
- [ ] Reload idempotency
- [ ] Dependency graph properties
- [ ] State transformation roundtrip

## References

1. [Erlang/OTP 28 Highlights](https://www.erlang.org/blog/highlights-otp-28/)
2. [code module documentation](https://www.erlang.org/doc/apps/kernel/code.html)
3. [Release Handling](https://www.erlang.org/doc/system/release_handling.html)
4. [Appup Cookbook](https://www.erlang.org/doc/system/appup_cookbook.html)
5. [Modules documentation](https://www.erlang.org/doc/system/modules.html)
6. [OTP 26 Release Notes](https://www.erlang.org/downloads/26)
7. [OTP 27 Release Notes](https://www.erlang.org/downloads/27)
8. [OTP 28 Release Notes](https://www.erlang.org/downloads/28)

## Conclusion

OTP 26-28 provide significant improvements to hot code loading:

1. **Atomic loading** (`prepare_loading/1`, `finish_loading/1`) reduces downtime
2. **Native coverage** enables production monitoring
3. **Module status detection** simplifies reload automation
4. **Enhanced `on_load`** improves boot reliability

**Recommendation**: Prioritize adoption of two-phase loading for production deployments. This provides the most significant reliability improvement with minimal code changes.

---

**Document Version**: 1.0
**Author**: Claude (Erlang OTP Developer Agent)
**Status**: Research Complete
