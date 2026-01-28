# Hot Code Reload Operations Guide

## Overview

erlmcp provides production-safe hot code reloading capabilities using Erlang/OTP's built-in code loading mechanisms with additional safety checks and graceful degradation.

## Architecture

### Components

1. **erlmcp_code_reload** - Main reload coordinator
   - Validates new code before loading
   - Manages rollback windows
   - Tracks reload history
   - Orchestrates safety checks

2. **erlmcp_graceful_drain** - Connection draining service
   - Pauses new requests during reload
   - Waits for in-flight requests to complete
   - Ensures zero request loss

3. **erlmcp_reload_sup** - Reload subsystem supervisor
   - Supervises reload coordinator and drain service
   - Uses `one_for_all` strategy for consistency

### Supervision Tree

```
erlmcp_core_sup (one_for_one)
    └── erlmcp_reload_sup (one_for_all)
        ├── erlmcp_graceful_drain
        └── erlmcp_code_reload
```

## Safety Mechanisms

### 1. Pre-Reload Validation

Before any reload operation:

- **Syntax Check**: Verify BEAM file is valid
- **Dialyzer Check**: Optional type checking (if enabled)
- **Test Execution**: Run module-specific tests (if enabled)
- **State Compatibility**: Check record version compatibility

### 2. Connection Draining

Graceful request handling:

```erlang
% 1. Signal drain to pause new requests
erlmcp_graceful_drain:drain_module(Module, TimeoutMs)

% 2. Wait for in-flight requests to complete
wait_for_in_flight_requests(Module, TimeoutMs)

% 3. Proceed with reload once idle
```

### 3. Rollback Window

Automatic rollback on failure:

- Default: 60 seconds rollback window
- Old BEAM file backed up before reload
- Smoke tests run after reload
- Automatic rollback if smoke tests fail
- Manual rollback available: `erlmcp_code_reload:rollback_module(Module)`

### 4. Crash Spike Detection

Monitor for reload-induced crashes:

- Track process crash rate after reload
- Automatic rollback if crash spike detected
- Configurable threshold and window

## Usage

### Basic Module Reload

```erlang
% Simple reload with defaults
Module = my_module,
Opts = #{},
{ok, OldVsn, NewVsn} = erlmcp_code_reload:reload_module(Module, Opts).
```

### Production Reload (Recommended)

```erlang
% Full safety checks enabled
Module = my_module,
Opts = #{
    validate_syntax => true,
    validate_dialyzer => false,  % Expensive, use in staging
    run_tests => true,
    drain_connections => true,
    drain_timeout_ms => 5000,
    rollback_window_s => 60,
    smoke_tests => [
        fun() ->
            % Basic functionality test
            case my_module:health_check() of
                ok -> ok;
                Error -> {error, Error}
            end
        end
    ]
},
Result = erlmcp_code_reload:reload_module(Module, Opts).
```

### Multiple Module Reload

Reload modules in dependency order:

```erlang
Modules = [
    my_module_a,
    my_module_b,  % Depends on a
    my_module_c   % Depends on b
],
Opts = #{drain_connections => true},
Results = erlmcp_code_reload:reload_modules(Modules, Opts).

% Results: [{my_module_a, {ok, V1, V2}}, ...]
```

### Validation Only (Dry Run)

```erlang
% Validate without reloading
erlmcp_code_reload:validate_module(Module).
% Returns: ok | {error, Reason}
```

### Manual Rollback

```erlang
% Rollback to previous version
erlmcp_code_reload:rollback_module(Module).
```

### Check Reload History

```erlang
% Get all reload operations
History = erlmcp_code_reload:get_reload_history().

% Example entry:
% #{
%     module => my_module,
%     old_vsn => "1.0.0",
%     new_vsn => "1.0.1",
%     timestamp => {1640,995200,0},
%     result => ok
% }
```

## Reload Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `validate_syntax` | boolean | true | Verify BEAM file validity |
| `validate_dialyzer` | boolean | false | Run Dialyzer type checks |
| `run_tests` | boolean | false | Execute module tests |
| `drain_connections` | boolean | true | Pause new requests |
| `drain_timeout_ms` | pos_integer | 5000 | Max time to wait for drain |
| `rollback_window_s` | pos_integer | 60 | Auto-rollback window |
| `smoke_tests` | [fun()] | [] | Post-reload verification tests |

## Best Practices

### 1. State Record Compatibility

When changing gen_server state records, use versioned records:

```erlang
% Old version
-record(state, {
    field1 :: type1(),
    vsn = 1 :: integer()
}).

% New version with migration
-record(state, {
    field1 :: type1(),
    field2 :: type2(),  % New field
    vsn = 2 :: integer()
}).

code_change(1, State, _Extra) ->
    % Migrate from v1 to v2
    NewState = State#state{
        field2 = default_value(),
        vsn = 2
    },
    {ok, NewState}.
```

### 2. Incremental Rollout

Test reloads in staging before production:

```bash
# Staging: Full validation
Opts = #{
    validate_dialyzer => true,
    run_tests => true,
    smoke_tests => [...]
}

# Production: Fast validation
Opts = #{
    validate_syntax => true,
    drain_connections => true,
    smoke_tests => [basic_health_check()]
}
```

### 3. Monitoring

Track reload operations:

```erlang
% Monitor reload success rate
History = erlmcp_code_reload:get_reload_history(),
SuccessCount = length([E || E <- History, maps:get(result, E) =:= ok]),
TotalCount = length(History),
SuccessRate = (SuccessCount / TotalCount) * 100.
```

### 4. Smoke Tests

Design effective smoke tests:

```erlang
smoke_tests => [
    % 1. Health check
    fun() ->
        case my_module:ping() of
            pong -> ok;
            _ -> {error, ping_failed}
        end
    end,

    % 2. Basic operation
    fun() ->
        case my_module:simple_operation() of
            {ok, _} -> ok;
            Error -> {error, {operation_failed, Error}}
        end
    end,

    % 3. Critical path
    fun() ->
        my_module:critical_function(),
        ok
    end
]
```

## Operational Procedures

### Rolling Update Procedure

For multi-node clusters:

1. **Select target nodes**: Start with non-critical nodes
2. **Drain node**: `erlmcp_graceful_drain:drain_module(Module, 10000)`
3. **Reload module**: `erlmcp_code_reload:reload_module(Module, Opts)`
4. **Monitor for 5 minutes**: Watch crash rate and error logs
5. **Proceed to next node**: Repeat for remaining nodes
6. **Full rollback if needed**: Rollback all nodes if issues detected

### Emergency Rollback

If reload causes issues:

```erlang
% 1. Immediate rollback
erlmcp_code_reload:rollback_module(Module).

% 2. Verify rollback succeeded
{file, BeamPath} = code:is_loaded(Module),
{ok, {Module, Version}} = beam_lib:version(BeamPath).

% 3. Resume normal operations
erlmcp_graceful_drain:resume_module(Module).
```

### Troubleshooting

#### Issue: "Drain timeout"

```erlang
% Increase drain timeout
Opts = #{drain_timeout_ms => 30000}.  % 30 seconds
```

#### Issue: "In-flight requests timeout"

Indicates slow requests blocking reload:

```erlang
% Check in-flight count
erlmcp_code_reload:get_in_flight_count(Module).

% Consider longer timeout or force reload (risky)
```

#### Issue: "Rollback failed"

```erlang
% Check backup file exists
BeamPath = code:which(Module),
BackupPath = BeamPath ++ ".backup",
filelib:is_regular(BackupPath).  % Should be true

% Manual restore if needed
file:copy(BackupPath, BeamPath),
code:purge(Module),
code:load_file(Module).
```

## Performance Impact

### Reload Timing (Typical)

- Validation: 10-100ms
- Drain: 100-5000ms (depends on request rate)
- Code load: 1-10ms
- Smoke tests: 10-100ms
- **Total: ~150-5200ms**

### Zero-Downtime Guarantee

With proper configuration:

- New requests paused during reload (5s max)
- In-flight requests complete normally
- No connection drops
- No data loss

## Integration with CI/CD

### Example Deployment Script

```bash
#!/bin/bash
# deploy_hot_reload.sh

MODULE=$1
NODE=$2

# 1. Upload new BEAM file
scp _build/default/lib/*/ebin/${MODULE}.beam ${NODE}:/path/to/beam/

# 2. Trigger reload
erl -noshell -name deployer@localhost \
    -setcookie ${COOKIE} \
    -eval "
        rpc:call('${NODE}', erlmcp_code_reload, reload_module, [
            ${MODULE},
            #{
                validate_syntax => true,
                drain_connections => true,
                rollback_window_s => 300
            }
        ]),
        init:stop().
    "

# 3. Monitor for 5 minutes
echo "Monitoring for 5 minutes..."
sleep 300

# 4. Check status
erl -noshell -name checker@localhost \
    -setcookie ${COOKIE} \
    -eval "
        History = rpc:call('${NODE}', erlmcp_code_reload, get_reload_history, []),
        [Latest | _] = History,
        io:format(\"Result: ~p~n\", [maps:get(result, Latest)]),
        init:stop().
    "
```

## Security Considerations

1. **Authentication**: Only authorized operators should trigger reloads
2. **Validation**: Always validate BEAM files before loading
3. **Audit Trail**: All reloads logged in history
4. **Rollback**: Keep backups for emergency recovery
5. **Testing**: Test reloads in staging first

## References

- [Erlang Code Loading](https://www.erlang.org/doc/reference_manual/code_loading.html)
- [OTP Release Upgrades](https://www.erlang.org/doc/design_principles/release_handling.html)
- [erlmcp OTP Patterns](../otp-patterns.md)
