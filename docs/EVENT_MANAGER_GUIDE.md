# erlmcp Event Manager Guide

## Overview

The `erlmcp_event_manager` provides a decoupled event handling system using Erlang's `gen_event` behavior. This allows multiple handlers to react to the same event without tight coupling between event producers and consumers.

## Architecture

### Components

1. **erlmcp_event_manager** - The gen_event manager process
2. **erlmcp_event_logger** - Logging handler (logs events to Erlang logger)
3. **erlmcp_event_metrics** - Metrics collection handler (telemetry integration)
4. **erlmcp_event_audit** - Audit trail handler (compliance logging)

### Benefits

- **Decoupling**: Event producers don't need to know about consumers
- **Multiple Handlers**: Many handlers can process the same event
- **Crash Isolation**: One handler crash doesn't affect others
- **Dynamic Configuration**: Add/remove handlers at runtime
- **No Message Copying**: gen_event is efficient for broadcast patterns

## Event Types

```erlang
-type event() ::
    {tool_executed, ToolName :: binary(), Duration :: non_neg_integer(), Result :: term()} |
    {resource_updated, Uri :: binary(), Metadata :: map()} |
    {connection_state, State :: connected | disconnected, Info :: map()} |
    {error, Category :: atom(), Reason :: term()} |
    {request_received, Method :: binary(), RequestId :: term()} |
    {response_sent, Method :: binary(), RequestId :: term(), Duration :: non_neg_integer()} |
    {notification_sent, Method :: binary(), Params :: map()} |
    {session_created, SessionId :: binary(), Metadata :: map()} |
    {session_terminated, SessionId :: binary(), Reason :: term()}.
```

## Usage

### Starting the Event Manager

The event manager is automatically started by `erlmcp_observability_sup`:

```erlang
%% Manually start if needed
{ok, Pid} = erlmcp_event_manager:start_link().
```

### Adding Handlers

```erlang
%% Add logger handler
ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}).

%% Add metrics handler
ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []).

%% Add audit handler with configuration
ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{
    enabled => true,
    log_all_events => false  % Only log critical events
}).
```

### Emitting Events

```erlang
%% Synchronous notification (blocks until all handlers process)
ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 1000000, ok}).

%% Asynchronous notification (returns immediately)
ok = erlmcp_event_manager:notify_async({resource_updated, <<"file://test.txt">>, #{}}).

%% Error notification
ok = erlmcp_event_manager:notify({error, validation, invalid_params}).

%% Connection state change
ok = erlmcp_event_manager:notify({connection_state, connected, #{transport => stdio}}).

%% Session lifecycle
ok = erlmcp_event_manager:notify({session_created, <<"sess_001">>, #{user => <<"alice">>}}).
ok = erlmcp_event_manager:notify({session_terminated, <<"sess_001">>, normal}).
```

### Managing Handlers

```erlang
%% List registered handlers
Handlers = erlmcp_event_manager:which_handlers().
%% => [erlmcp_event_logger, erlmcp_event_metrics, erlmcp_event_audit]

%% Remove a handler
ok = erlmcp_event_manager:delete_handler(erlmcp_event_logger, []).

%% Swap handlers (hot code upgrade)
ok = erlmcp_event_manager:swap_handler(
    erlmcp_event_logger,  % Old handler
    my_custom_logger,     % New handler
    #{}                   % New handler state
).
```

## Handler Details

### erlmcp_event_logger

Logs events to Erlang logger with appropriate log levels:

- **debug**: Request/response events
- **info**: Tool executions, resource updates, session events
- **warning**: Connection state changes
- **error**: Error events

**Configuration:**
```erlang
#{
    log_level => info  % debug | info | notice | warning | error
}
```

**Statistics:**
```erlang
Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_logger, get_stats).
%% => #{
%%     event_count => 42,
%%     uptime_ms => 12345,
%%     log_level => info
%% }
```

### erlmcp_event_metrics

Collects metrics and emits telemetry events:

**Telemetry Events:**
- `[erlmcp, event, tool_executed]` - Tool execution metrics
- `[erlmcp, event, resource_updated]` - Resource update metrics
- `[erlmcp, event, error]` - Error metrics
- `[erlmcp, event, request_received]` - Request metrics
- `[erlmcp, event, response_sent]` - Response metrics

**Metrics:**
```erlang
Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics).
%% => #{
%%     tool_executions => #{<<"echo">> => 10, <<"grep">> => 5},
%%     resource_updates => #{<<"file://test.txt">> => 3},
%%     error_counts => #{validation => 2, transport => 1},
%%     connection_events => 4,
%%     request_count => 20,
%%     response_count => 18,
%%     notification_count => 5,
%%     session_count => 2,
%%     uptime_ms => 60000
%% }

%% Reset metrics
ok = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, reset_metrics).
```

### erlmcp_event_audit

Creates audit trail for compliance (GDPR, SOC2, HIPAA):

**Configuration:**
```erlang
#{
    enabled => true,         % Enable/disable auditing
    log_all_events => false  % true: log all, false: only critical events
}
```

**Critical Events (always audited):**
- Tool executions
- Resource updates
- Connection state changes
- Errors
- Session creation/termination

**Optional Events (only if `log_all_events = true`):**
- Request received
- Response sent
- Notifications sent

**Statistics:**
```erlang
Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats).
%% => #{
%%     enabled => true,
%%     log_all_events => false,
%%     event_count => 42,
%%     uptime_ms => 60000
%% }

%% Enable/disable at runtime
ok = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, {set_enabled, false}).
ok = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, {set_log_all, true}).
```

**Sensitive Data Sanitization:**

The audit handler automatically sanitizes sensitive fields:
- `password` → `[REDACTED]`
- `secret` → `[REDACTED]`
- `token` → `[REDACTED]`
- `api_key` → `[REDACTED]`
- `credentials` → `[REDACTED]`

## Integration Examples

### Integration with erlmcp_server

```erlang
%% In erlmcp_server.erl, emit events when tools are executed
handle_call({call_tool, ToolName, Args}, From, State) ->
    StartTime = erlang:system_time(microsecond),

    Result = execute_tool(ToolName, Args, State),

    Duration = erlang:system_time(microsecond) - StartTime,

    %% Emit event
    erlmcp_event_manager:notify_async({tool_executed, ToolName, Duration, Result}),

    {reply, Result, State}.
```

### Integration with erlmcp_client

```erlang
%% In erlmcp_client.erl, track request/response
handle_call({send_request, Method, Params}, From, State) ->
    RequestId = generate_request_id(),

    %% Emit request received event
    erlmcp_event_manager:notify_async({request_received, Method, RequestId}),

    Result = send_jsonrpc_request(Method, Params, RequestId, State),

    {reply, Result, State}.

handle_info({response, Method, RequestId, Duration}, State) ->
    %% Emit response sent event
    erlmcp_event_manager:notify_async({response_sent, Method, RequestId, Duration}),

    {noreply, State}.
```

### Custom Event Handlers

Create your own handler:

```erlang
-module(my_custom_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    custom_data :: term()
}).

init(Args) ->
    {ok, #state{custom_data = maps:get(data, Args, undefined)}}.

handle_event({tool_executed, ToolName, Duration, Result}, State) ->
    %% Your custom logic here
    io:format("Tool ~s executed in ~p us~n", [ToolName, Duration]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_state, State) ->
    {ok, State, State};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

Register your custom handler:

```erlang
ok = erlmcp_event_manager:add_handler(my_custom_handler, #{data => some_value}).
```

## Testing

All handlers include comprehensive EUnit tests:

```bash
# Test event manager
rebar3 eunit --module=erlmcp_event_manager_tests

# Test logger handler
rebar3 eunit --module=erlmcp_event_logger_tests

# Test metrics handler
rebar3 eunit --module=erlmcp_event_metrics_tests

# Test audit handler
rebar3 eunit --module=erlmcp_event_audit_tests

# Test all event modules
rebar3 eunit --application=erlmcp_observability
```

## Performance Considerations

### When to Use Sync vs Async

- **Sync (`notify/1`)**: Use when you need to ensure all handlers have processed the event before continuing. Blocks until completion.

- **Async (`notify_async/1`)**: Use for non-critical events or when performance is important. Returns immediately.

### Handler Count

gen_event is efficient even with many handlers because:
- Events are not copied per handler (shared in memory)
- Handlers run sequentially but in the same process
- No message passing overhead

### Memory Usage

- Each event is a single term in memory (not copied per handler)
- Handlers maintain their own state (small overhead per handler)
- No process-per-handler overhead

## Joe Armstrong Philosophy

This implementation follows Joe Armstrong's principles:

1. **Isolation**: Handler crashes don't affect the manager or other handlers
2. **Simplicity**: gen_event is built-in OTP, no custom protocols
3. **Decoupling**: Event producers don't know about consumers
4. **Let-it-crash**: Handlers can crash and restart independently
5. **Process per responsibility**: Manager is one process, handlers are lightweight

## Migration from Existing Systems

If you're currently using direct calls or callbacks:

**Before:**
```erlang
%% Tightly coupled
lists:foreach(fun(Handler) -> Handler({tool_executed, Name, Duration}) end, Handlers).
```

**After:**
```erlang
%% Decoupled
erlmcp_event_manager:notify_async({tool_executed, Name, Duration, Result}).
```

## Troubleshooting

### Handler Not Receiving Events

Check if handler is registered:
```erlang
Handlers = erlmcp_event_manager:which_handlers().
```

### Event Manager Not Started

The manager is started by `erlmcp_observability_sup`. If manually managing:
```erlang
case whereis(erlmcp_event_manager) of
    undefined -> {ok, _} = erlmcp_event_manager:start_link();
    Pid -> {ok, Pid}
end.
```

### Handler Crashing

Check handler logs and fix the handler's `handle_event/2` callback. Other handlers will continue working.

## References

- Erlang gen_event documentation: https://www.erlang.org/doc/man/gen_event.html
- Joe Armstrong's thesis on fault tolerance: "Making reliable distributed systems in the presence of software errors"
- erlmcp OTP patterns: `docs/otp-patterns.md`
