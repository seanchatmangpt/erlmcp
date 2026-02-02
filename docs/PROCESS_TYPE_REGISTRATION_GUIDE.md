# Process Type Registration Guide

## Overview

This guide shows how to add `$mcp_type` and `$mcp_context_id` markers to existing erlmcp processes for OTP 28 iterator-based introspection.

## Process Type Markers

### Required Keys

| Key | Type | Purpose | Example |
|-----|------|---------|---------|
| `$mcp_type` | atom() | Process type classification | `model_context`, `tool_process` |
| `$mcp_context_id` | binary() | Unique context identifier | `<<"session_123">>` |

## Integration Examples

### Example 1: erlmcp_server (Model Context)

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`

**Location**: `init/1` callback

**Before**:
```erlang
init([ServerId, Capabilities]) ->
    process_flag(trap_exit, true),

    %% ... existing init code ...

    {ok, #state{server_id = ServerId,
                capabilities = Capabilities,
                %% ... other fields ...
               }, {continue, initialize}}.
```

**After**:
```erlang
init([ServerId, Capabilities]) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', model_context),
    put('$mcp_context_id', ServerId),

    %% ... existing init code ...

    {ok, #state{server_id = ServerId,
                capabilities = Capabilities,
                %% ... other fields ...
               }, {continue, initialize}}.
```

### Example 2: erlmcp_client (Tool Process)

**File**: `apps/erlmcp_core/src/erlmcp_client.erl`

**Location**: `init/1` callback

**Before**:
```erlang
init([ServerId, Config]) ->
    process_flag(trap_exit, true),

    %% ... existing init code ...

    {ok, #state{server_id = ServerId,
                config = Config}}.
```

**After**:
```erlang
init([ServerId, Config]) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', tool_process),
    put('$mcp_context_id', ServerId),

    %% ... existing init code ...

    {ok, #state{server_id = ServerId,
                config = Config}}.
```

### Example 3: Transport Handlers

**File**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`

**Location**: `init/1` callback (both server and client modes)

**Server Mode**:
```erlang
init(#{mode := server,
       ranch_ref := RanchRef,
       protocol_opts := ProtocolOpts} = InitArgs) ->
    process_flag(trap_exit, true),

    ServerId = maps:get(server_id, ProtocolOpts, undefined),
    TransportId = maps:get(transport_id, ProtocolOpts, undefined),

    %% Add process type marker for introspection
    put('$mcp_type', transport_handler),
    put('$mcp_transport_id', TransportId),

    %% ... existing init code ...

    {ok, State}.
```

**Client Mode**:
```erlang
init(#{mode := client} = Opts) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', transport_handler),

    %% ... existing init code ...

    {ok, State}.
```

### Example 4: Session Backends

**File**: `apps/erlmcp_core/src/erlmcp_session_backend.erl`

**Location**: `init/1` callback

```erlang
init([BackendType, Opts]) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', session_backend),

    %% ... existing init code ...

    {ok, #state{backend_type = BackendType, opts = Opts}}.
```

### Example 5: Registry

**File**: `apps/erlmcp_core/src/erlmcp_registry.erl`

**Location**: `init/1` callback

```erlang
init([]) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', registry),

    %% ... existing init code ...

    {ok, #registry_state{}, {continue, ensure_dependencies}}.
```

### Example 6: Monitor Processes

**File**: `apps/erlmcp_observability/src/erlmcp_health_monitor.erl`

**Location**: `init/1` callback

```erlang
init([Opts]) ->
    process_flag(trap_exit, true),

    %% Add process type marker for introspection
    put('$mcp_type', monitor),

    %% ... existing init code ...

    {ok, #state{}}.
```

## Verification

### Test 1: Check Process Dictionary

```erlang
%% Get info for a process
{ok, Info} = erlmcp_inspector:get_process_info(Pid),

%% Verify type marker
Type = maps:get(type, Info),
?assertEqual(model_context, Type).

%% Check dictionary directly
Dict = maps:get(dictionary, Info),
?assertEqual(model_context, proplists:get_value('$mcp_type', Dict)).
```

### Test 2: List Processes by Type

```erlang
%% List all model contexts
Contexts = erlmcp_inspector:find_contexts_by_type(model_context),
?assert(length(Contexts) > 0).

%% List all tool processes
Tools = erlmcp_inspector:find_contexts_by_type(tool_process),
?assert(length(Tools) >= 0).
```

### Test 3: Find by Context ID

```erlang
%% Find specific context
ContextId = <<"my_session_123">>,
{ok, Pid} = erlmcp_inspector:find_context_by_id(ContextId),
?assert(is_pid(Pid)).
```

### Test 4: Admin API

```erlang
%% Inspect contexts
Contexts = erlmcp_admin:inspect_contexts(),
?assert(is_list(Contexts)).

%% Inspect stats
Stats = erlmcp_admin:inspect_stats(),
Total = maps:get(total, Stats),
?assert(Total >= 0).
```

## Process Type Taxonomy

### Type Classification Rules

**By Module Name Pattern**:
```
erlmcp_server*      -> model_context
erlmcp_client*      -> tool_process
erlmcp_transport_*  -> transport_handler
erlmcp_session_*    -> session_backend
erlmcp_registry*    -> registry
erlmcp_*monitor*    -> monitor
erlmcp_*profiler*   -> monitor
erlmcp_*debugger*   -> monitor
```

**By Registered Name**:
The inspector automatically classifies by registered name if `$mcp_type` is not set.

## Integration Checklist

For each module to integrate:

- [ ] Add `put('$mcp_type', Type)` in `init/1`
- [ ] Add `put('$mcp_context_id', Id)` if applicable
- [ ] Add `put('$mcp_transport_id', Id)` for transports
- [ ] Add unit test verifying type marker
- [ ] Add integration test with `erlmcp_inspector`
- [ ] Update module documentation

## Testing Integration

### Unit Test Template

```erlang
-module(my_module_tests).

-include_lib("eunit/include/eunit.hrl").

process_type_marker_test() ->
    %% Start process
    {ok, Pid} = my_module:start_link(),

    %% Get process info
    {ok, Info} = erlmcp_inspector:get_process_info(Pid),

    %% Verify type
    ?assertEqual(expected_type, maps:get(type, Info)),

    %% Cleanup
    gen_server:stop(Pid).
```

### Integration Test Template

```erlang
-module(my_module_integration_tests).

-include_lib("eunit/include/eunit.hrl").

inspect_by_type_test() ->
    %% Start process
    {ok, Pid} = my_module:start_link(),

    %% Find by type
    Processes = erlmcp_inspector:inspect_by_type(expected_type),

    %% Verify process appears in list
    ?assert(lists:keymember(Pid, pid, Processes)),

    %% Cleanup
    gen_server:stop(Pid).
```

## Troubleshooting

### Issue: Process not appearing in inspector

**Cause**: `$mcp_type` not set in process dictionary

**Solution**:
```erlang
%% Verify type is set
process_info(Pid, dictionary).
%% Should show: {dictionary, [{'$mcp_type', model_context}, ...]}
```

### Issue: Type classified as "unknown"

**Cause**: Module name doesn't match pattern, no type marker

**Solution**: Add explicit `put('$mcp_type', Type)` in init/1

### Issue: Context ID lookup fails

**Cause**: `$mcp_context_id` not set or incorrect ID

**Solution**:
```erlang
%% Verify context ID is set
{dictionary, Dict} = process_info(Pid, dictionary),
proplists:get_value('$mcp_context_id', Dict).
```

## Best Practices

1. **Set Early**: Always set type markers at the beginning of `init/1`
2. **Use Constants**: Define type atoms as module constants
3. **Document**: Add comments explaining type classification
4. **Test**: Verify with inspector after adding markers
5. **Consistent**: Use same type pattern across similar modules

## Module Constants Example

```erlang
%% Define process type as constant
-define(MCP_PROCESS_TYPE, model_context).

init([Args]) ->
    process_flag(trap_exit, true),

    %% Use constant
    put('$mcp_type', ?MCP_PROCESS_TYPE),

    %% ... rest of init ...
    {ok, #state{}}.
```

## References

- [OTP 28 Process Iteration](/Users/sac/erlmcp/docs/PROCESS_ITERATION_INTROSPECTION.md)
- [erlmcp_inspector Module](/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_inspector.erl)
- [erlmcp_admin Module](/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_admin.erl)
- [OTP Patterns](/Users/sac/erlmcp/docs/otp-patterns.md)
