# erlmcp OTP 28-Optimized Code Reload System Design

**Date**: 2026-02-01
**Target OTP Version**: 28.3.1
**Design Status**: Proposed
**Related**: [OTP 26-28 Hot Code Loading Research](./otp28-hot-code-loading-research.md)

## Overview

This document describes the optimization of erlmcp's code reloading system to leverage OTP 28 features, particularly atomic two-phase loading, native coverage, and enhanced module status detection.

## Design Goals

1. **Zero-Downtime Reload**: Minimize time window where system is partially upgraded
2. **Atomic Operations**: Either all modules load or none do
3. **Production-Ready**: Enable safe hot reload in production environments
4. **Backward Compatible**: Support OTP 26+ with graceful degradation
5. **Observable**: Provide metrics and tracing for reload operations

## Architecture

### High-Level Components

```
┌─────────────────────────────────────────────────────────────┐
│                  erlmcp_code_reload_sup                     │
│  ┌───────────────────────────────────────────────────────┐  │
│  │           erlmcp_code_reload_coordinator              │  │
│  │  - Orchestrates two-phase loading                     │  │
│  │  - Manages reload windows                             │  │
│  │  - Coordinates with graceful drain                     │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌──────────────────┐  ┌──────────────────────────────┐    │
│  │ reload_validator │  │    reload_metrics            │    │
│  │ - Pre-checks     │  │ - Timing                     │    │
│  │ - Dependencies   │  │ - Success/Failure rates      │    │
│  │ - Compatibility  │  │ - Module status tracking     │    │
│  └──────────────────┘  └──────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Module Specifications

### erlmcp_code_reload_coordinator

**Purpose**: Two-phase atomic reload orchestration

**Key Functions**:

```erlang
%% Two-phase atomic reload (OTP 27+)
-spec prepare_reload([module()], reload_opts()) ->
    {ok, prepared_code()} | {error, term()}.
prepare_reload(Modules, Opts) ->
    validate_reload_request(Modules, Opts),
    sort_by_dependencies(Modules),
    code:prepare_loading(Modules).

-spec execute_reload(prepared_code(), reload_opts()) ->
    ok | {error, term()}.
execute_reload(Prepared, Opts) ->
    % Phase 1: Drain connections
    drain_connections(Opts),
    % Phase 2: Atomic load
    code:finish_loading(Prepared),
    % Phase 3: Verify
    verify_reload_success(Modules, Opts).
```

**OTP Version Detection**:

```erlang
supports_atomic_reload() ->
    {ok, Vsn} = application:get_key(kernel, vsn),
    MajorVsn = list_to_integer(string:split(Vsn, ".") |> hd()),
    MajorVsn >= 27.

-spec reload_modules([module()], reload_opts()) -> reload_result().
reload_modules(Modules, Opts) ->
    case supports_atomic_reload() of
        true ->
            reload_modules_atomic(Modules, Opts);
        false ->
            % Fallback for OTP 26
            reload_modules_legacy(Modules, Opts)
    end.
```

### erlmcp_reload_validator

**Purpose**: Pre-flight validation before reload

**Checks**:

1. **Module Status**: Detect modified modules
   ```erlang
   validate_module_status(Module) ->
       case code:module_status(Module) of
           modified -> ok;
           loaded -> {error, not_modified};
           removed -> {error, module_removed};
           not_loaded -> {error, module_not_loaded}
       end.
   ```

2. **Dependency Graph**: Validate load order
   ```erlang
   validate_dependencies(Modules) ->
       Graph = build_dependency_graph(Modules),
       case detect_cycles(Graph) of
           true -> {error, circular_dependencies};
           false -> {ok, topological_sort(Graph)}
       end.
   ```

3. **Beam File Validation**: Check format
   ```erlang
   validate_beam_file(Module) ->
       BeamPath = code:which(Module),
       case beam_lib:chunks(BeamPath, [exports, attributes]) of
           {ok, {Module, _}} -> ok;
           {error, Reason} -> {error, {invalid_beam, Reason}}
       end.
   ```

4. **State Compatibility**: Check `code_change/3` exists
   ```erlang
   validate_code_change(Module) ->
       Exports = Module:module_info(exports),
       case lists:keymember(code_change, 1, Exports) of
           true -> ok;
           false -> {warning, no_code_change}
       end.
   ```

### erlmcp_reload_metrics

**Purpose**: Observability for reload operations

**Metrics**:

1. **Timing Metrics**
   ```erlang
   record_timing(reload_start, prepare_phase, execute_phase, verify_phase)
   ```

2. **Success/Failure Rates**
   ```erlang
   update_reload_counters(Module, Result)
   ```

3. **Module Status Tracking**
   ```erlang
   track_module_status(Modules) ->
       [{Mod, code:module_status(Mod)} || Mod <- Modules].
   ```

4. **OTEL Integration**
   ```erlang
   emit_reload_span(Module, Duration, Result)
   ```

## Enhanced State Management

### Versioned State Records

```erlang
% v2 state (proposed)
-record(state_v2, {
    version = 2 :: integer(),
    reload_history = [] :: [reload_entry()],
    rollback_timers = #{} :: #{module() => reference()},
    draining = false :: boolean(),
    % New fields
    backup_binaries = #{} :: #{module() => binary()},
    module_status_cache = #{} :: #{module() => module_status()},
    reload_metrics = #{} :: map()
}).
```

### Migration Strategy

```erlang
% V1 -> V2 migration
code_change(1, StateV1, _Extra) ->
    StateV2 = #state_v2{
        version = 2,
        reload_history = StateV1#state.reload_history,
        rollback_timers = StateV1#state.rollback_timers,
        draining = StateV1#state.draining,
        backup_binaries = #{},
        module_status_cache = #{},
        reload_metrics = #{}
    },
    {ok, StateV2}.
```

## Enhanced Backup Mechanism

### In-Memory Binary Backup

**Advantages**:
- No disk I/O
- Faster rollback
- Works in read-only environments

```erlang
%% Backup to in-memory binary
backup_module_binary(Module) ->
    case code:get_object_code(Module) of
        {Module, Binary, Filename} ->
            logger:info("Backed up ~p to memory (~p bytes)",
                       [Module, byte_size(Binary)]),
            {ok, Binary, Filename};
        error ->
            {error, module_not_found}
    end.

%% Restore from binary
restore_from_binary(Module, Binary, Filename) ->
    logger:info("Restoring ~p from in-memory backup", [Module]),
    code:purge(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} -> ok;
        {error, Reason} -> {error, {restore_failed, Reason}}
    end.
```

**Fallback**: Use file-based `.backup` for OTP < 26

## Native Coverage Integration

### Reload Verification

```erlang
%% Check if new code is actually being used
verify_reload_with_coverage(Modules) ->
    case code:coverage_support() of
        false ->
            logger:warning("Native coverage not supported, skipping verification"),
            ok;
        true ->
            verify_coverage(Modules)
    end.

verify_coverage(Modules) ->
    lists:foreach(fun(Module) ->
        case code:get_coverage_mode(Module) of
            none ->
                logger:info("Module ~p has no coverage enabled", [Module]);
            _ ->
                {ok, Coverage} = code:get_coverage(function, Module),
                verify_critical_functions_covered(Module, Coverage)
        end
    end, Modules).

verify_critical_functions_covered(Module, Coverage) ->
    CriticalExports = get_critical_exports(Module),
    Covered = [Func || {Func, _} <- Coverage, lists:member(Func, CriticalExports)],
    case length(Covered) =:= length(CriticalExports) of
        true ->
            logger:info("All critical functions in ~p are covered", [Module]);
        false ->
            logger:warning("Some critical functions in ~p not executed after reload",
                          [Module])
    end.
```

## Two-Phase Reload Flow

### Detailed Algorithm

```
┌─────────────────────────────────────────────────────────────┐
│  PHASE 1: PREPARE (Validation & Preparation)                │
│  1. Validate module status (modified?)                      │
│  2. Build dependency graph                                  │
│  3. Topological sort (load order)                           │
│  4. Validate beam files                                     │
│  5. Check code_change/3 callbacks                           │
│  6. code:prepare_loading(Modules)                           │
│     → Returns {ok, Prepared} | {error, Reasons}            │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  PHASE 2: DRAIN (Quiesce System)                           │
│  1. Signal erlmcp_graceful_drain                           │
│  2. Wait for in-flight requests to complete                │
│  3. Pause new requests                                     │
│     → Timeout: configurable (default 5s)                   │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  PHASE 3: EXECUTE (Atomic Load)                            │
│  1. Backup module binaries (in-memory)                     │
│  2. code:finish_loading(Prepared)                          │
│     → Either ALL modules load or NONE                      │
│  3. Trigger sys:change_code for gen_servers               │
│  4. Update module status cache                             │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  PHASE 4: VERIFY (Post-Reload Checks)                      │
│  1. Run smoke tests (parallel)                             │
│  2. Verify native coverage (if enabled)                    │
│  3. Check process health                                   │
│  4. Resume normal operations                               │
│     → On failure: rollback from backups                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  PHASE 5: ROLLBACK WINDOW (Optional)                       │
│  1. Start rollback timer (configurable)                    │
│  2. Monitor health metrics                                 │
│  3. Auto-rollback on failure                               │
│  4. Commit new code on success                             │
└─────────────────────────────────────────────────────────────┘
```

## Error Handling & Rollback

### Automatic Rollback Triggers

1. **Smoke Test Failure**
   ```erlang
   run_smoke_tests(SmokeTests) ->
       case run_tests_parallel(SmokeTests) of
           {ok, _} -> ok;
           {error, Reasons} ->
               logger:error("Smoke tests failed: ~p", [Reasons]),
               trigger_automatic_rollback()
       end.
   ```

2. **Coverage Verification Failure**
   ```erlang
   verify_coverage(Module) ->
       case code:get_coverage(function, Module) of
           {ok, []} ->
               logger:error("No coverage for ~p after reload", [Module]),
               trigger_automatic_rollback();
           {ok, Coverage} ->
               logger:info("Coverage OK for ~p: ~p functions", [Module, length(Coverage)])
       end.
   ```

3. **Process Health Degradation**
   ```erlang
   monitor_process_health(Modules) ->
       Pids = get_processes_using_modules(Modules),
       erlang:send_after(5000, self(), {health_check, Pids}),
       receive
           {health_check, Pids} ->
               case check_process_health(Pids) of
                   ok -> ok;
                   {error, HealthIssues} ->
                       logger:error("Process health degraded: ~p", [HealthIssues]),
                       trigger_automatic_rollback()
               end
       end.
   ```

### Rollback Mechanisms

```erlang
%% Fast rollback from in-memory backup
rollback_module(Module) ->
    case get_backup_binary(Module) of
        {ok, Binary, Filename} ->
            logger:warning("Rolling back ~p from memory", [Module]),
            restore_from_binary(Module, Binary, Filename);
        error ->
            logger:warning("No in-memory backup for ~p, trying disk", [Module]),
            rollback_from_disk(Module)
    end.

%% Rollback multiple modules atomically
rollback_modules(Modules) ->
    logger:warning("Rolling back ~p modules", [length(Modules)]),
    lists:foreach(fun rollback_module/1, Modules),
    ok.
```

## Configuration

### Reload Options

```erlang
-type reload_opts() :: #{
    % Validation options
    validate_syntax => boolean(),
    validate_dialyzer => boolean(),
    run_tests => boolean(),
    smoke_tests => [fun(() -> ok | {error, term()})],

    % Drain options
    drain_connections => boolean(),
    drain_timeout_ms => pos_integer(),

    % Rollback options
    rollback_window_s => pos_integer(),
    rollback_on_coverage_fail => boolean(),
    rollback_on_health_degrade => boolean(),

    % Verification options
    verify_coverage => boolean(),
    coverage_mode => function | line,
    verify_process_health => boolean(),

    % Performance options
    parallel_smoke_tests => boolean(),
    use_in_memory_backup => boolean(),

    % Logging options
    verbose => boolean(),
    log_module_status => boolean()
}.
```

### Default Configuration

```erlang
default_opts() ->
    #{
        validate_syntax => true,
        validate_dialyzer => false,
        run_tests => true,
        smoke_tests => [],

        drain_connections => true,
        drain_timeout_ms => 5000,

        rollback_window_s => 60,
        rollback_on_coverage_fail => true,
        rollback_on_health_degrade => true,

        verify_coverage => true,
        coverage_mode => function,
        verify_process_health => true,

        parallel_smoke_tests => true,
        use_in_memory_backup => true,

        verbose => false,
        log_module_status => true
    }.
```

## OTEL Integration

### Reload Spans

```erlang
start_reload_span(Module) ->
    otel_tracer:start_span("erlmcp.code_reload", #{
        "erlmcp.reload.module" => Module,
        "erlmcp.reload.timestamp" => erlang:system_time(millisecond)
    }).

end_reload_span(SpanCtx, Result, Duration) ->
    otel_tracer:span_event(SpanCtx, "reload_complete", #{
        "erlmcp.reload.result" => Result,
        "erlmcp.reload.duration_ms" => Duration
    }),
    otel_tracer:end_span(SpanCtx).
```

### Metrics

```erlang
record_reload_metric(Module, Result, Duration) ->
    otel_meter:histogram("erlmcp.reload.duration", Duration, #{
        "erlmcp.reload.module" => Module,
        "erlmcp.reload.result" => Result
    }).
```

## Testing Strategy

### Unit Tests

1. **Two-Phase Loading Tests**
   - Test `prepare_loading` success/failure
   - Test `finish_loading` atomicity
   - Test error handling

2. **Module Status Detection**
   - Test all 4 status values (loaded, modified, removed, not_loaded)

3. **Binary Backup/Restore**
   - Test in-memory backup
   - Test rollback from backup

4. **Dependency Graph**
   - Test topological sort
   - Test cycle detection

### Integration Tests

1. **End-to-End Reload Flow**
   - Test complete reload cycle
   - Test rollback scenarios

2. **Concurrent Reloads**
   - Test parallel module reloads
   - Test race conditions

3. **OTP Version Compatibility**
   - Test OTP 26 (legacy mode)
   - Test OTP 27+ (atomic mode)

### Property-Based Tests

```erlang
%% Reload idempotency
prop_reload_idempotent() ->
    ?FORALL(Module, module(),
        begin
            {ok, Vsn1} = reload_module(Module, Opts),
            {ok, Vsn2} = reload_module(Module, Opts),
            Vsn1 =:= Vsn2
        end).

%% State transformation roundtrip
prop_state_roundtrip() ->
    ?FORALL({OldState, Vsn, Extra}, {state(), version(), term()},
        begin
            {ok, NewState} = apply_code_change(OldState, Vsn, Extra),
            {ok, RestoredState} = apply_code_change(NewState, {down, Vsn}, Extra),
            OldState =:= RestoredState
        end).
```

## Migration Path

### Phase 1: Core Changes (Week 1)

1. Add `erlmcp_code_reload_coordinator`
2. Implement two-phase loading with OTP version detection
3. Add `erlmcp_reload_validator` module
4. Update `erlmcp_code_reload` to use new coordinator

### Phase 2: Enhanced Features (Week 2)

1. Implement in-memory binary backup
2. Add native coverage verification
3. Implement `erlmcp_reload_metrics` with OTEL
4. Add module status caching

### Phase 3: Testing & Documentation (Week 3)

1. Write comprehensive test suite
2. Update documentation
3. Create migration guide
4. Performance benchmarking

### Phase 4: Production Rollout (Week 4)

1. Deploy to staging
2. Monitor metrics
3. Tune configuration
4. Deploy to production

## Performance Considerations

### Expected Improvements

1. **Reduced Downtime**: Two-phase loading minimizes partial upgrade window
   - Current: ~100ms per module (sequential)
   - Optimized: ~50ms total (atomic batch)

2. **Faster Rollback**: In-memory backups
   - Current: ~200ms (disk I/O)
   - Optimized: ~10ms (memory copy)

3. **Better Throughput**: Parallel smoke tests
   - Current: 500ms sequential
   - Optimized: 100ms parallel (5 tests)

### Monitoring Metrics

```erlang
%% Key metrics to track
- erlmcp.reload.duration (histogram)
- erlmcp.reload.success (counter)
- erlmcp.rollback.count (counter)
- erlmcp.reload.errors (counter)
- erlmcp.module_status.cached (gauge)
```

## Backward Compatibility

### OTP Version Detection

```erlang
%% Graceful degradation for OTP < 27
reload_modules_atomic(Modules, Opts) ->
    case erlang:system_info(otp_release) >= "27" of
        true ->
            % Use two-phase loading
            {ok, Prepared} = code:prepare_loading(Modules),
            code:finish_loading(Prepared);
        false ->
            % Fall back to legacy sequential loading
            reload_modules_legacy(Modules, Opts)
    end.
```

### Feature Detection

```erlang
%% Check for native coverage support
supports_coverage() ->
    try code:coverage_support() of
        Supported -> Supported
    catch
        error:undef -> false
    end.

%% Check for module_status
supports_module_status() ->
    try code:module_status(undefined) of
        _ -> true
    catch
        error:undef -> false
    end.
```

## Security Considerations

1. **Code Signing**: Verify module signatures before reload
2. **Access Control**: Restrict reload operations to authorized users
3. **Audit Logging**: Log all reload operations with user context
4. **Rollback Safety**: Ensure rollback cannot be exploited

## Conclusion

This design provides a production-ready, zero-downtime code reload system optimized for OTP 28, with backward compatibility for OTP 26+. The key improvements are:

1. **Atomic two-phase loading** (OTP 27+) for zero partial upgrade window
2. **In-memory binary backups** for fast rollback
3. **Native coverage integration** for verification
4. **Comprehensive observability** with OTEL metrics
5. **Graceful degradation** for older OTP versions

**Next Steps**: Implement Phase 1 changes and begin testing.

---

**Document Version**: 1.0
**Status**: Proposed Design
**Review Date**: 2026-02-08
