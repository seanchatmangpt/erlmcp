# Progress Token Integration Guide

## Overview

This document describes how to integrate progress token reporting into `erlmcp_server.erl` for tool execution.

## Implementation Summary

The progress token integration allows clients to receive incremental progress updates during long-running tool operations, following the MCP 2025-11-25 specification.

## Key Changes Required

### 1. Update `handle_request/4` for `tools/call`

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_request/4` (around line 595)

**Current code**:
```erlang
handle_request(Id, ?MCP_METHOD_TOOLS_CALL, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tools_call">>, ServerId),
    try
        Name = maps:get(?MCP_PARAM_NAME, Params, undefined),
        Args = maps:get(?MCP_PARAM_ARGUMENTS, Params, #{}),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TOOLS_CALL,
            <<"tool.name">> => Name,
            <<"arguments_count">> => maps:size(Args)
        }),

        case {Name, Args} of
            {undefined, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_tool_name, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_TOOL_NAME);
            {ToolName, Arguments} ->
                handle_tool_call(Id, ToolName, Arguments, TransportId, State)
        end,
        ...
```

**Updated code**:
```erlang
handle_request(Id, ?MCP_METHOD_TOOLS_CALL, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tools_call">>, ServerId),
    try
        Name = maps:get(?MCP_PARAM_NAME, Params, undefined),
        Args = maps:get(?MCP_PARAM_ARGUMENTS, Params, #{}),

        %% Extract progress token from _meta field if provided by client
        Meta = maps_get(<<"_meta">>, Params, #{}),
        ClientProgressToken = maps_get(?MCP_PARAM_PROGRESS_TOKEN, Meta, undefined),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TOOLS_CALL,
            <<"tool.name">> => Name,
            <<"arguments_count">> => maps:size(Args),
            <<"has_client_progress_token">> => ClientProgressToken =/= undefined
        }),

        case {Name, Args} of
            {undefined, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_tool_name, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_TOOL_NAME);
            {ToolName, Arguments} ->
                handle_tool_call(Id, ToolName, Arguments, ClientProgressToken, TransportId, State)
        end,
        ...
```

### 2. Update `handle_tool_call/5` signature and implementation

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_tool_call/5` (around line 1026)

**Current signature**:
```erlang
-spec handle_tool_call(json_rpc_id(), binary(), map(), atom(), state()) -> ok.
handle_tool_call(Id, Name, Arguments, TransportId, #state{server_id = ServerId} = State) ->
```

**Updated signature**:
```erlang
-spec handle_tool_call(json_rpc_id(), binary(), map(), term() | undefined, atom(), state()) -> ok.
handle_tool_call(Id, Name, Arguments, ClientProgressToken, TransportId, #state{server_id = ServerId} = State) ->
```

### 3. Update progress token handling in `handle_tool_call`

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_tool_call/6` (around line 1041-1050)

**Current code**:
```erlang
{_Tool, Handler, _Schema} ->
    % Gap #10: Generate unique progress token for this tool call
    ProgressToken = erlmcp_progress:generate_token(),
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"progress_token">> => ProgressToken
    }),

    % Track tool execution for progress reporting
    ServerPid = self(),
    _ = erlmcp_progress:track_tool_call(ProgressToken, Name, ServerPid),
```

**Updated code**:
```erlang
{_Tool, Handler, _Schema} ->
    %% Use client's progress token if provided, otherwise generate one
    ProgressToken = case ClientProgressToken of
        undefined ->
            %% Generate server-side token for internal tracking
            erlmcp_progress:generate_token();
        Token ->
            %% Use client's token for direct progress reporting
            Token
    end,

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"progress_token">> => ProgressToken,
        <<"client_progress_token">> => ClientProgressToken =/= undefined
    }),

    %% Track tool execution with progress token
    %% If client provided token, use it directly for reporting
    case ClientProgressToken of
        undefined ->
            %% No client token - create internal tracker
            ServerPid = self(),
            _ = erlmcp_progress:track_tool_call(ProgressToken, Name, ServerPid);
        _ ->
            %% Client token - will send notifications directly
            %% Send initial progress notification
            InitialMessage = <<"Tool execution started: ", Name/binary>>,
            erlmcp_progress:update(ProgressToken, #{
                message => InitialMessage,
                current => 0
            })
    end,
```

### 4. Add progress updates during execution

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_tool_call/6` (around line 1062-1072)

**Current code**:
```erlang
try
    % TASK #107: Execute with CPU protection (quota + timeout)
    % This prevents CPU-intensive DoS attacks
    TimeoutMs = case application:get_env(erlmcp, tool_timeout_ms) of
        {ok, Timeout} -> Timeout;
        undefined -> 5000  % Default 5 second timeout
    end,
```

**Updated code**:
```erlang
try
    %% Report execution starting (25% progress)
    case ClientProgressToken of
        undefined -> ok;
        _ -> erlmcp_progress:update(ProgressToken, #{message => <<"Initializing tool execution">>, current => 25, total => 100})
    end,

    % TASK #107: Execute with CPU protection (quota + timeout)
    % This prevents CPU-intensive DoS attacks
    TimeoutMs = case application:get_env(erlmcp, tool_timeout_ms) of
        {ok, Timeout} -> Timeout;
        undefined -> 5000  % Default 5 second timeout
    end,

    %% Report execution in progress (50% progress)
    case ClientProgressToken of
        undefined -> ok;
        _ -> erlmcp_progress:update(ProgressToken, #{message => <<"Executing tool handler">>, current => 50, total => 100})
    end,
```

### 5. Update success response handling

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_tool_call/6` (around line 1073-1099)

**Current code**:
```erlang
{ok, Result, CpuTime} ->
    % Tool executed successfully
    erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
        <<"cpu_time_ms">> => CpuTime
    }),

    ContentList = normalize_tool_result(Result),
    ...
    % Include progress token in response metadata
    Response = #{
        ?MCP_PARAM_CONTENT => ContentList,
        <<"_meta">> => #{
            ?MCP_PARAM_PROGRESS_TOKEN => ProgressToken
        }
    },
    send_response_safe(State, TransportId, Id, Response),
```

**Updated code**:
```erlang
{ok, Result, CpuTime} ->
    %% Tool executed successfully
    %% Report completion (100% progress)
    case ClientProgressToken of
        undefined -> ok;
        _ -> erlmcp_progress:update(ProgressToken, #{message => <<"Tool execution completed">>, current => 100, total => 100})
    end,

    erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
        <<"cpu_time_ms">> => CpuTime
    }),

    ContentList = normalize_tool_result(Result),
    ...

    %% Include progress token in response metadata only if client provided one
    Response = case ClientProgressToken of
        undefined ->
            %% No client token - don't include in response
            #{
                ?MCP_PARAM_CONTENT => ContentList
            };
        _ ->
            %% Client provided token - echo it back
            #{
                ?MCP_PARAM_CONTENT => ContentList,
                <<"_meta">> => #{
                    ?MCP_PARAM_PROGRESS_TOKEN => ProgressToken
                }
            }
    end,
    send_response_safe(State, TransportId, Id, Response),
```

### 6. Update cleanup logic

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Function**: `handle_tool_call/6` (around line 1124-1133)

**Current code**:
```erlang
end,

% Clean up progress token after completion
erlmcp_progress:cleanup_completed(ProgressToken)
catch
    ClassCatch:ReasonCatch:StackCatch ->
        erlmcp_tracing:record_exception(HandlerSpanCtx, ClassCatch, ReasonCatch, StackCatch),
        logger:error("Tool handler crashed: ~p:~p~n~p", [ClassCatch, ReasonCatch, StackCatch]),
        % Cleanup even on error
        erlmcp_progress:cleanup_completed(ProgressToken),
        send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
```

**Updated code**:
```erlang
end,

%% Clean up progress token after completion
case ClientProgressToken of
    undefined ->
        %% Only cleanup if we created the token
        erlmcp_progress:cleanup_completed(ProgressToken);
    _ ->
        %% Client owns the token, mark as complete but don't cleanup
        erlmcp_progress:complete(ProgressToken)
end
catch
    ClassCatch:ReasonCatch:StackCatch ->
        %% Report crash in progress
        case ClientProgressToken of
            undefined -> ok;
            _ -> erlmcp_progress:update(ProgressToken, #{message => <<"Tool handler crashed">>, current => 0})
        end,
        erlmcp_tracing:record_exception(HandlerSpanCtx, ClassCatch, ReasonCatch, StackCatch),
        logger:error("Tool handler crashed: ~p:~p~n~p", [ClassCatch, ReasonCatch, StackCatch]),
        %% Cleanup even on error
        case ClientProgressToken of
            undefined -> erlmcp_progress:cleanup_completed(ProgressToken);
            _ -> erlmcp_progress:complete(ProgressToken)
        end,
        send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
```

## Helper Function

Add this helper function at the end of the file (or use `maps:get/3` which is available in Erlang/OTP 26+):

```erlang
%% @private
%% Safe map get with default value
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
```

## Testing

### Manual Test

```erlang
%% Start a server with a long-running tool
{ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}).

%% Add a tool that takes time
ToolHandler = fun(Args) ->
    %% Simulate work
    timer:sleep(5000),
    <<"Result after 5 seconds">>
end,
erlmcp_server:add_tool(Server, <<"long_running_tool">>, ToolHandler).

%% Call the tool WITH progress token
%% In the request params, include:
%% #{
%%   <<"name">> => <<"long_running_tool">>,
%%   <<"arguments">> => #{},
%%   <<"_meta">> => #{
%%     <<"progressToken">> => 12345  %% Client's token
%%   }
%% }
```

### Expected Behavior

1. **With Client Progress Token**:
   - Server extracts token from `_meta.progressToken`
   - Server sends progress notifications at 0%, 25%, 50%, 100%
   - Response includes `_meta.progressToken` echoing the client's token
   - Token is marked complete but not cleaned up (client owns it)

2. **Without Client Progress Token**:
   - Server generates internal token
   - No progress notifications sent
   - Response does NOT include progress token
   - Token is cleaned up after execution

## Progress Notification Format

Notifications are sent via `erlmcp_progress:update/2`:

```erlang
%% Initial (0%)
erlmcp_progress:update(Token, #{
    message => <<"Tool execution started: tool_name">>,
    current => 0
})

%% Starting (25%)
erlmcp_progress:update(Token, #{
    message => <<"Initializing tool execution">>,
    current => 25,
    total => 100
})

%% In progress (50%)
erlmcp_progress:update(Token, #{
    message => <<"Executing tool handler">>,
    current => 50,
    total => 100
})

%% Complete (100%)
erlmcp_progress:update(Token, #{
    message => <<"Tool execution completed">>,
    current => 100,
    total => 100
})
```

The `erlmcp_progress` module sends JSON-RPC notifications in the format:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/progress",
  "params": {
    "progressToken": 12345,
    "progress": 50,
    "total": 100
  }
}
```

## Integration Checklist

- [ ] Update `handle_request/4` to extract client progress token
- [ ] Update `handle_tool_call/5` to `handle_tool_call/6` with `ClientProgressToken` parameter
- [ ] Add progress update calls during execution (25%, 50%, 100%)
- [ ] Update response metadata to only include token if client provided one
- [ ] Update cleanup logic to handle client vs server tokens
- [ ] Add `maps_get/3` helper function if needed
- [ ] Test with client progress token
- [ ] Test without client progress token
- [ ] Verify cleanup behavior for both cases

## Files Modified

1. `apps/erlmcp_core/src/erlmcp_server.erl` - Main implementation
2. `apps/erlmcp_core/src/erlmcp_progress.erl` - Already implemented, no changes needed

## Related Modules

- `erlmcp_progress` - Progress tracking and notification sending
- `erlmcp_json_rpc` - JSON-RPC encoding/decoding
- `erlmcp_tracing` - OpenTelemetry tracing
- `erlmcp_cpu_guard` - CPU quota and timeout protection

## MCP Spec Compliance

This implementation follows the MCP 2025-11-25 specification for progress tokens:

- Progress tokens are extracted from `_meta` field in request parameters
- Progress notifications are sent via `notifications/progress` method
- Progress values are percentage-based (0-100)
- Tokens can be integers, strings, or references
- Server echoes the client's token in response metadata if provided
- Proper cleanup of server-generated tokens
- Client tokens are marked complete but not removed by server
