# Progress Token Support

## Overview

Progress tokens enable servers to send incremental updates during long-running operations (tool calls, resource reads, etc.) per the MCP 2025-11-25 specification.

## Architecture

### Components

- **`erlmcp_progress`** - gen_server that tracks progress tokens and sends notifications
- **Progress Token** - Reference that uniquely identifies a progress stream
- **Progress Notification** - JSON-RPC notification sent to client with progress updates

### Flow

```
1. Client provides progressToken in _meta field of request
2. Server creates progress tracker (erlmcp_progress:create/2)
3. Server sends initial notification (progress: 0)
4. Server updates progress incrementally (erlmcp_progress:update/2)
5. Server marks progress complete (erlmcp_progress:complete/1)
6. Server sends final notification (progress: 100)
```

## API

### Starting the Progress Server

```erlang
{ok, Pid} = erlmcp_progress:start_link().
```

### Creating a Progress Token

```erlang
%% Create with client PID to receive notifications
Token = erlmcp_progress:create(ClientPid, <<"Starting operation">>).

%% Create without client (for tracking only)
Token = erlmcp_progress:create(undefined, <<"Tracking operation">>).
```

### Updating Progress

```erlang
%% Increment by 25
erlmcp_progress:update(Token, #{increment => 25}).

%% Set absolute current value
erlmcp_progress:update(Token, #{current => 50}).

%% Set total for percentage calculation
erlmcp_progress:update(Token, #{current => 50, total => 100}).

%% Update message
erlmcp_progress:update(Token, #{message => <<"Processing data">>}).

%% Multiple updates at once
erlmcp_progress:update(Token, #{
    current => 75,
    total => 100,
    message => <<"Almost done">>
}).
```

### Completing Progress

```erlang
%% Marks progress as 100% and removes tracking
erlmcp_progress:complete(Token).
```

### Canceling Progress

```erlang
%% Removes tracking without sending completion notification
erlmcp_progress:cancel(Token).
```

### Getting Progress State

```erlang
{ok, ProgressMap} = erlmcp_progress:get_progress(Token),
%% ProgressMap contains:
%%   - token: Reference
%%   - current: Current value
%%   - total: Total value (if set)
%%   - progress: Percentage (0-100) or undefined
%%   - message: Current status message
%%   - elapsed_ms: Time since creation
```

### Encoding Notifications

```erlang
%% For manual notification encoding
Notification = erlmcp_progress:encode_progress_notification(Token, 75.5, 100).
%% Returns:
%%   #{
%%     <<"jsonrpc">> => <<"2.0">>,
%%     <<"method">> => <<"notifications/progress">>,
%%     <<"params">> => #{
%%       <<"progressToken">> => Token,
%%       <<"progress">> => 75.5,
%%       <<"total">> => 100
%%     }
%%   }
```

## Integration with erlmcp_server

### Tool Execution with Progress

```erlang
%% In handle_tool_call
handle_tool_call(Id, Name, Arguments, TransportId, State) ->
    %% Generate progress token
    ProgressToken = erlmcp_progress:generate_token(),

    %% Track tool execution
    _ = erlmcp_progress:track_tool_call(ProgressToken, Name, ServerPid),

    %% Execute tool with progress updates
    execute_tool_with_progress(Handler, Arguments, ProgressToken),

    %% Cleanup
    erlmcp_progress:cleanup_completed(ProgressToken).
```

### Progress Updates During Execution

```erlang
execute_tool_with_progress(Handler, Arguments, ProgressToken) ->
    erlmcp_progress:update(ProgressToken, #{message => <<"Starting">>}),
    %% Do work...
    erlmcp_progress:update(ProgressToken, #{increment => 25, message => <<"Processing">>}),
    %% More work...
    erlmcp_progress:update(ProgressToken, #{increment => 25, message => <<"Finalizing">>}),
    %% Complete
    erlmcp_progress:complete(ProgressToken).
```

## Notification Format

### Progress Notification

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/progress",
  "params": {
    "progressToken": "<reference>",
    "progress": 50,
    "total": 100,
    "message": "Processing data"
  }
}
```

### Completion Notification

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/progress",
  "params": {
    "progressToken": "<reference>",
    "progress": 100,
    "total": 100,
    "message": "Operation - Complete"
  }
}
```

## Best Practices

### 1. Always Clean Up

```erlang
try
    execute_operation(ProgressToken)
catch
    Type:Reason:Stack ->
        logger:error("Operation failed: ~p:~p", [Type, Reason]),
        erlmcp_progress:cancel(ProgressToken)
after
    erlmcp_progress:cleanup_completed(ProgressToken)
end
```

### 2. Set Total When Known

```erlang
%% Good - percentage calculated automatically
erlmcp_progress:update(Token, #{current => 50, total => 100}).

%% Avoid - no percentage available
erlmcp_progress:update(Token, #{current => 50}).
```

### 3. Provide Meaningful Messages

```erlang
%% Good - informative
erlmcp_progress:update(Token, #{
    increment => 25,
    message => <<"Processed 25/100 records">>
}).

%% Avoid - vague
erlmcp_progress:update(Token, #{
    increment => 25,
    message => <<"Working">>
}).
```

### 4. Handle Undefined Client PIDs

```erlang
%% Safe - won't crash if client is undefined
Token = erlmcp_progress:create(ClientPid, Message),

%% Or create without notifications
Token = erlmcp_progress:create(undefined, Message).
```

## Error Handling

### Token Not Found

```erlang
case erlmcp_progress:get_progress(Token) of
    {ok, Progress} ->
        %% Use progress data
        {ok, maps:get(progress, Progress)};
    {error, not_found} ->
        %% Token doesn't exist (completed or cancelled)
        {error, invalid_token}
end
```

### Concurrent Updates

```erlang
%% Safe - updates are serialized by gen_server
spawn(fun() ->
    erlmcp_progress:update(Token, #{increment => 25})
end),
spawn(fun() ->
    erlmcp_progress:update(Token, #{increment => 25})
end).
```

## Testing

### Manual Test Script

```bash
./test_progress_manual.erl
```

### EUnit Tests

```bash
rebar3 eunit --module=erlmcp_progress_tests
```

## Performance Considerations

### Memory Usage

- Each active progress token consumes ~500 bytes
- Tokens are automatically removed on completion
- Cancel unused tokens to free memory

### Notification Frequency

```erlang
%% Good - reasonable update frequency
lists:foreach(fun(N) ->
    erlmcp_progress:update(Token, #{increment => 1}),
    timer:sleep(100)  % 100ms between updates
end, lists:seq(1, 100)).

%% Avoid - excessive updates
lists:foreach(fun(N) ->
    erlmcp_progress:update(Token, #{increment => 1})
    %% No delay - 10,000 updates/second!
end, lists:seq(1, 10000)).
```

## Compliance

### MCP 2025-11-25 Specification

- ✅ Progress token creation
- ✅ Progress notifications
- ✅ Meta field support
- ✅ Percentage calculation
- ✅ Completion handling
- ✅ Cancellation support
- ✅ Multiple concurrent streams

## Future Enhancements

1. **Progress Aggregation** - Aggregate multiple progress streams
2. **ETA Calculation** - Estimate time remaining
3. **Progress History** - Track progress over time
4. **Batch Notifications** - Reduce notification frequency
5. **Progress Sub-tasks** - Hierarchical progress tracking

## References

- [MCP Protocol Specification](https://modelcontextprotocol.io/docs)
- [erlmcp OTP Patterns](./otp-patterns.md)
- [erlmcp Server Implementation](../apps/erlmcp_core/src/erlmcp_server.erl)
