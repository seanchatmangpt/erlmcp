# Graceful Shutdown Implementation - erlmcp_shutdown

## Overview

The `erlmcp_shutdown` module provides a comprehensive graceful shutdown mechanism for the erlmcp MCP SDK. It implements a 4-phase shutdown sequence that ensures:

1. **No new connections** are accepted during shutdown
2. **Existing requests** complete gracefully (with configurable timeout)
3. **Resources are cleaned up** in an orderly fashion
4. **State is saved** before termination

## Architecture

### Shutdown Coordinator

The shutdown coordinator is a `gen_server` that manages the shutdown process. It maintains:

- **Shutdown Status**: Current phase, connection counts, timestamps
- **Cleanup Handlers**: Registered callbacks for resource cleanup
- **Awaiters**: Processes waiting for shutdown completion

### 4-Phase Shutdown Sequence

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Graceful Shutdown                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Phase 1: Initiation (0-5s)                                         │
│  ├─ Set global shutdown flag (gproc)                               │
│  ├─ Notify all registered processes                                │
│  ├─ Stop accepting new connections                                 │
│  └─ Record connection counts                                       │
│                                                                     │
│  Phase 2: Drain (5-35s, configurable)                               │
│  ├─ Wait for existing requests to complete                         │
│  ├─ Monitor active connections                                     │
│  └─ Enforce timeout with fallback                                  │
│                                                                     │
│  Phase 3: Resource Cleanup (35-40s)                                 │
│  ├─ Run cleanup handlers (priority order)                          │
│  ├─ Close connection pools                                         │
│  ├─ Stop transports                                                │
│  └─ Stop servers                                                   │
│                                                                     │
│  Phase 4: State Persistence (40-45s)                                │
│  ├─ Save registry state                                            │
│  ├─ Persist metrics                                                │
│  └─ Flush telemetry                                                │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## API Reference

### Starting the Coordinator

```erlang
%% Start the shutdown coordinator (usually from supervisor)
{ok, Pid} = erlmcp_shutdown:start_link().
```

### Initiating Shutdown

```erlang
%% Shutdown with default 30s drain timeout
ok = erlmcp_shutdown:shutdown(normal).

%% Shutdown with custom 60s drain timeout
ok = erlmcp_shutdown:shutdown(normal, 60000).

%% Immediate shutdown (skips drain)
ok = erlmcp_shutdown:shutdown_now().
```

### Monitoring Shutdown

```erlang
%% Get current shutdown status
{ok, Status} = erlmcp_shutdown:get_status().
%% Status includes:
%%   - phase: Current shutdown phase
%%   - connections_active: Number of active connections
%%   - connections_total: Total connections
%%   - start_time: Shutdown start timestamp
%%   - estimated_completion: Estimated completion time

%% Block until shutdown completes
ok = erlmcp_shutdown:await_shutdown().

%% Block with timeout
ok = erlmcp_shutdown:await_shutdown(5000).
```

### Cleanup Handlers

Cleanup handlers allow custom code to run during the shutdown sequence. They're executed in priority order:

```erlang
%% Define a cleanup handler
Handler = #cleanup_handler{
    id = my_cleanup,
    module = my_module,
    function = cleanup_function,
    args = [arg1, arg2],
    timeout = 5000,           % Max execution time
    priority = high           % urgent | high | normal | low
},

%% Register handler
ok = erlmcp_shutdown:register_cleanup_handler(my_cleanup, Handler).

%% Unregister handler
ok = erlmcp_shutdown:unregister_cleanup_handler(my_cleanup).
```

**Priority Order**: `urgent > high > normal > low`

### Canceling Shutdown

Shutdown can only be canceled before the drain phase completes:

```erlang
ok = erlmcp_shutdown:cancel_shutdown().
```

## Integration with Existing Components

### erlmcp_app (Application Module)

The existing `prep_stop/1` callback in `erlmcp_app.erl` calls `erlmcp_registry:graceful_shutdown/0`. This can be extended to coordinate with the shutdown coordinator:

```erlang
%% In erlmcp_app.erl
prep_stop(State) ->
    logger:info("Preparing erlmcp_core for graceful shutdown"),

    %% Initiate coordinated shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Wait for drain phase (optional)
    timer:sleep(1000),

    State.
```

### erlmcp_registry

The registry already has `graceful_shutdown/0` which notifies registered processes:

```erlang
%% In erlmcp_registry.erl
graceful_shutdown() ->
    %% Notify servers and transports
    lists:foreach(fun({ServerId, {Pid, _Config}}) ->
        Pid ! {erlmcp_shutdown, ServerId, normal}
    end, list_servers()),
    ...
```

The shutdown coordinator calls this via `notify_processes/1`.

### erlmcp_transports_app

The transports application's `prep_stop/1` callback stops accepting new connections:

```erlang
%% In erlmcp_transports_app.erl
prep_stop(State) ->
    logger:info("Stopping accepting new connections"),
    erlmcp_transport_sup:stop_accepting(),
    erlmcp_connection_pool:drain(),
    State.
```

The shutdown coordinator calls these directly in the cleanup phase.

### erlmcp_observability_app

The observability application's `prep_stop/1` callback flushes telemetry:

```erlang
%% In erlmcp_observability_app.erl
prep_stop(State) ->
    logger:info("Flushing telemetry data"),
    erlmcp_otel:flush(),
    State.
```

The shutdown coordinator calls this in the state persistence phase.

## Usage Examples

### Example 1: Server Handling Shutdown Notification

```erlang
%% In your gen_server
handle_info({erlmcp_shutdown, ServerId, Reason}, State) ->
    logger:info("Server ~p shutting down: ~p", [ServerId, Reason]),

    %% Stop accepting new requests
    {noreply, State#server_state{accepting = false}};

handle_call({request, _Req}, _From, #server_state{accepting = false} = State) ->
    %% Reject new requests during shutdown
    {reply, {error, shutting_down}, State};
```

### Example 2: Transport Handling Shutdown

```erlang
%% In your transport process
handle_info({erlmcp_shutdown, TransportId, Reason}, State) ->
    logger:info("Transport ~p shutting down: ~p", [TransportId, Reason]),

    %% Close listen socket (stop accepting)
    catch gen_tcp:close(State#transport_state.listen_socket),

    %% Drain existing connections
    NewState = drain_connections(State),

    {noreply, NewState#transport_state{listening = false}}.
```

### Example 3: Custom Cleanup Handler

```erlang
%% Define cleanup module
-module(my_app_cleanup).

-export([save_state/1]).

save_state(Timeout) ->
    logger:info("Saving application state"),
    %% Save state to disk
    ok = file:write_file("/var/lib/my_app/state.json",
                         term_to_binary(get_state())),
    ok.

%% Register handler
Handler = #cleanup_handler{
    id = save_app_state,
    module = my_app_cleanup,
    function = save_state,
    args = [],
    timeout = 10000,
    priority = high
},
ok = erlmcp_shutdown:register_cleanup_handler(save_app_state, Handler).
```

## Configuration

### Drain Timeout

The default drain timeout is 30 seconds. This can be configured per shutdown:

```erlang
%% Shorter drain (10s)
ok = erlmcp_shutdown:shutdown(normal, 10000).

%% Longer drain (60s)
ok = erlmcp_shutdown:shutdown(normal, 60000).
```

**Limits**: 5s minimum, 5 minutes maximum

### Environment Variables

While the shutdown coordinator doesn't use environment variables directly, you can configure drain timeout via application environment:

```erlang
%% In sys.config
{erlmcp_core, [
    {shutdown_drain_timeout, 30000}  % 30 seconds
]}.
```

Then in your application code:

```erlang
DrainTimeout = application:get_env(erlmcp_core, shutdown_drain_timeout, 30000),
ok = erlmcp_shutdown:shutdown(normal, DrainTimeout).
```

## Testing

### Running Tests

```bash
# Run all shutdown tests
rebar3 eunit --module=erlmcp_shutdown_tests

# Run with coverage
rebar3 cover --verbose
```

### Validation Script

```bash
# Validate implementation
./scripts/validate_shutdown_implementation.sh
```

The validation script checks:
- Source file exists and is complete
- All required exports are present
- All gen_server callbacks are implemented
- Type specifications are complete
- Documentation is adequate
- Error handling is present
- Constants and records are defined
- Integration points exist
- Test coverage is adequate

## Best Practices

### 1. Register Critical Cleanup Handlers

Always register cleanup handlers for critical resources:

```erlang
%% Example: Flush database connection pool
Handler = #cleanup_handler{
    id = flush_db_pool,
    module = db_pool,
    function = flush,
    args = [],
    timeout = 5000,
    priority = urgent  % High priority for critical resources
},
ok = erlmcp_shutdown:register_cleanup_handler(flush_db_pool, Handler).
```

### 2. Handle Shutdown Messages in Your Servers

All gen_servers should handle `{erlmcp_shutdown, Id, Reason}`:

```erlang
handle_info({erlmcp_shutdown, Id, Reason}, State) ->
    logger:info("Shutting down ~p: ~p", [Id, Reason]),
    %% Perform cleanup
    {stop, normal, State}.
```

### 3. Use Appropriate Priorities

- **urgent**: Critical resources that must be saved (database, persistent state)
- **high**: Important cleanup (connection pools, caches)
- **normal**: Regular cleanup (metrics, logs)
- **low**: Nice-to-have cleanup (debug info, statistics)

### 4. Set Reasonable Timeouts

Each cleanup handler has a timeout. Set these based on expected completion time:

```erlang
Handler = #cleanup_handler{
    timeout = case Id of
        fast_cleanup -> 1000;      % 1s
        normal_cleanup -> 5000;    % 5s
        slow_cleanup -> 30000      % 30s
    end
}
```

### 5. Monitor Shutdown Progress

Use `get_status/0` to monitor shutdown:

```erlang
{ok, Status} = erlmcp_shutdown:get_status(),
logger:info("Shutdown phase: ~p, active connections: ~p",
           [Status#shutdown_status.phase,
            Status#shutdown_status.connections_active]).
```

## Troubleshooting

### Shutdown Hangs

If shutdown hangs in the drain phase:

1. Check connection counts: `get_status/0`
2. Reduce drain timeout and retry
3. Use `shutdown_now/0` for immediate termination

### Cleanup Handler Fails

If a cleanup handler fails:

1. Check logs for error messages
2. Verify handler module/function exists
3. Ensure timeout is sufficient
4. Fix the handler and retry

### Processes Don't Receive Shutdown Signal

Ensure processes are registered with `erlmcp_registry`:

```erlang
%% Register your server/transport
ok = erlmcp_registry:register_server(my_server, self(), Config).
```

## Performance Considerations

- **Fast Initiation**: Phase 1 completes in <5 seconds
- **Non-blocking**: The shutdown coordinator doesn't block the VM
- **Async Operations**: Most operations are asynchronous
- **Connection Monitoring**: Minimal overhead (polling every 5s)

## Security Considerations

- **No Secrets**: No hardcoded secrets in shutdown code
- **Audit Logging**: All shutdown actions are logged
- **State Sanitization**: Sensitive data is sanitized before logging
- **Access Control**: ETS tables have appropriate access controls

## References

- [OTP Design Principles - Supervisors](https://www.erlang.org/doc/design_principles/sup_princ.html)
- [gen_server Documentation](https://www.erlang.org/doc/man/gen_server.html)
- [Application Module - stop/1](https://www.erlang.org/doc/man/application.html#Module:stop-1)
- [Graceful Shutdown in Erlang](https://www.erlang-solutions.com/blog/graceful-shutdown-in-erlang.html)

## Receipt

Implementation details: `/Users/sac/erlmcp/receipts/P0-012-graceful-shutdown-implementation.json`

Key files:
- `apps/erlmcp_core/src/erlmcp_shutdown.erl` - Main implementation
- `apps/erlmcp_core/test/erlmcp_shutdown_tests.erl` - Test suite
- `scripts/validate_shutdown_implementation.sh` - Validation script
