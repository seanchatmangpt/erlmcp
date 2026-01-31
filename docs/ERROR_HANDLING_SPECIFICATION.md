# MCP Error Handling Specification

**Version**: 2.0
**Last Updated**: January 31, 2026
**Status**: Complete Reference Implementation

---

## Table of Contents

1. [Error Response Structure](#error-response-structure)
2. [Error Code Categories](#error-code-categories)
3. [Refusal Codes (1001-1089)](#refusal-codes-1001-1089)
4. [JSON-RPC 2.0 Standard Errors](#json-rpc-20-standard-errors)
5. [MCP-Specific Error Codes](#mcp-specific-error-codes)
6. [Error Messages and Descriptions](#error-messages-and-descriptions)
7. [Server-Side Error Handling Patterns](#server-side-error-handling-patterns)
8. [Client-Side Error Recovery](#client-side-error-recovery)
9. [Timeout Handling](#timeout-handling)
10. [Validation Error Responses](#validation-error-responses)

---

## Error Response Structure

### Standard JSON-RPC 2.0 Error Response Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "field": "limit",
      "reason": "must be between 1 and 1000"
    }
  }
}
```

### Response Structure Components

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `jsonrpc` | string | Yes | Must be "2.0" |
| `id` | number/string | Yes | Matches request ID (or null for notifications) |
| `error` | object | Yes | Error object with code, message, and optional data |
| `error.code` | integer | Yes | Error code (see categories below) |
| `error.message` | string | Yes | Human-readable error message |
| `error.data` | any | No | Additional context specific to the error |

### Error Data Field Guidelines

The `data` field provides structured context:

```erlang
%% Example: Invalid parameters with structured details
Data = #{
    <<"field">> => <<"query">>,
    <<"reason">> => <<"required">>,
    <<"received">> => null
},
encode_error_response(Id, -32602, <<"Invalid params">>, Data)
```

---

## Error Code Categories

### Category Classification

| Category | Code Range | Purpose |
|----------|-----------|---------|
| **JSON-RPC 2.0** | -32700 to -32600 | Protocol-level errors |
| **Core MCP** | -32001 to -32010 | Fundamental MCP operations |
| **Content** | -32011 to -32020 | Message size and encoding |
| **Resource** | -32021 to -32030 | Resource operations |
| **Tool** | -32031 to -32040 | Tool execution |
| **Prompt** | -32041 to -32050 | Prompt operations and sampling |
| **Auth** | -32051 to -32060 | Authentication/Authorization |
| **Protocol** | -32061 to -32070 | Protocol negotiation |
| **Pagination** | -32071 to -32080 | Cursor-based pagination |
| **Task** | -32081 to -32090 | Background task errors |
| **Progress** | -32091 to -32100 | Progress tracking |
| **Completion** | -32110 to -32113 | Completion requests |
| **Refusal** | 1001 to 1089 | Tool refusal with HTTP mapping |

### Error Severity Classification

```erlang
%% Critical errors - immediate action required
critical: -32700, -32600, -32603, 1079, 1089

%% Error level - operation failed
error: -32601, -32602, -32001..32010, -32011..32080

%% Warning level - recoverable/degraded
warning: -32009, -32032, -32047, 1005, 1069, 1076, 1078, 1086, 1087, 1088

%% Info level - non-critical
info: other codes
```

---

## Refusal Codes (1001-1089)

Refusal codes represent tool execution refusals with explicit HTTP status code mappings and remediation hints for client guidance.

### Complete Refusal Code Reference

#### Queue and Backpressure (1001-1005)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1001 | 429 | Queue capacity exceeded | Reduce message rate or increase queue_limits.max_messages in config | error |
| 1002 | 429 | Byte capacity exceeded | Message size too large or queue full; reduce message size or wait | error |
| 1003 | 429 | Tenant quota exceeded | Tenant aggregate limit reached; contact administrator | critical |
| 1004 | 503 | Buffer overflow | Internal buffer exhausted; reduce load or increase buffer_pool size | critical |
| 1005 | 503 | Backpressure active | Server under load; retry after delay or reduce request rate | warn |

#### Authentication & Authorization (1011-1016)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1011 | 401 | Authentication failed | Provide valid credentials or check token format | error |
| 1012 | 401 | Authentication expired | Refresh your authentication token | error |
| 1013 | 401 | Invalid credentials | Check username, password, or API key | error |
| 1014 | 403 | Authorization denied | You don't have permission for this resource; contact administrator | error |
| 1015 | 401 | Missing authentication | Provide credentials via Authorization header or session ID | error |
| 1016 | 401 | Invalid session ID | Provide valid MCP-Session-Id header (32+ hex chars) | error |

#### Parameter & Validation (1021-1029)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1021 | 400 | Invalid parameters | Check parameter names and types match API specification | error |
| 1022 | 400 | JSON schema validation failed | Review error details; structure must match schema | error |
| 1023 | 400 | Invalid URI format | Use valid absolute or relative URI | error |
| 1024 | 415 | Invalid Content-Type | Use application/json, text/plain, or application/octet-stream | error |
| 1025 | 400 | Required header missing or invalid | Include MCP-Protocol-Version and other required headers | error |
| 1026 | 400 | Session ID invalid | Session ID must be 32+ hexadecimal characters | error |
| 1027 | 400 | Protocol version not supported | Use protocol version 2025-11-25 or 2024-11-05 | error |
| 1028 | 400 | Required field missing | Check request includes all mandatory fields per API spec | error |
| 1029 | 400 | Field type mismatch | Field type doesn't match schema; check API documentation | error |

#### Path & URI Security (1036-1040)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1036 | 400 | Path traversal detected | Remove .. and % sequences; use canonical paths | critical |
| 1037 | 400 | Invalid path format | Path must start with / and contain only valid characters | error |
| 1038 | 400 | Symlink traversal detected | Symlinks not allowed; use direct file paths | critical |
| 1039 | 400 | URI out of bounds | URI must resolve within configured roots | error |
| 1040 | 400 | Canonical path violation | Path must be canonical (no . or .. components) | error |

#### Resource & Entity (1046-1052)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1046 | 404 | Resource not found | Check resource URI and ensure it exists | warn |
| 1047 | 409 | Resource already exists | Use different URI or update existing resource | warn |
| 1048 | 404 | Tool not found | Check tool name in tools/list or register new tool | warn |
| 1049 | 409 | Tool already registered | Tool exists; use different name or unregister first | warn |
| 1050 | 404 | Prompt not found | Check prompt name in prompts/list or create new prompt | warn |
| 1051 | 409 | Prompt already exists | Prompt exists; use different name or update existing | warn |
| 1052 | 409 | Entity already exists | Use different identifier or update via appropriate endpoint | warn |

#### Rate Limiting & Throttling (1056-1060)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1056 | 429 | Rate limit exceeded | Slow down request rate or implement exponential backoff | error |
| 1057 | 429 | Per-second rate limit exceeded | Max 100 requests/sec; wait before retrying | error |
| 1058 | 429 | Per-minute rate limit exceeded | Max 5000 requests/min; wait or increase plan | error |
| 1059 | 429 | Quota exceeded | Monthly quota used; upgrade plan or wait for reset | error |
| 1060 | 429 | Concurrent connection limit exceeded | Max 100 concurrent connections; close unused connections | error |

#### Protocol & Transport (1066-1070)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1066 | 400 | Protocol error | Review message format and protocol compliance | error |
| 1067 | 503 | Transport error | Network error detected; check connection and retry | error |
| 1068 | 413 | Message too large | Max 1MB per message; compress or split data | error |
| 1069 | 503 | Operation timeout | Increase timeout or reduce server load | warn |
| 1070 | 415 | Encoding not supported | Use UTF-8 or other supported encoding | error |

#### Server State (1076-1080)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1076 | 503 | Server not initialized | Call initialize before other RPC methods | error |
| 1077 | 503 | Server shutting down | Server is closing; reconnect after restart | warn |
| 1078 | 503 | Service unavailable | Server temporarily unavailable; retry later | warn |
| 1079 | 503 | Internal error | Unexpected error; check server logs and contact support | critical |
| 1080 | 503 | Dependency unavailable | External service down; wait or contact support | warn |

#### Circuit Breaker & Health (1086-1089)

| Code | HTTP | Message | Hint | Severity |
|------|------|---------|------|----------|
| 1086 | 503 | Circuit breaker open | Too many failures; wait before retrying | warn |
| 1087 | 503 | Health check failed | Server unhealthy; may be degraded or restarting | warn |
| 1088 | 503 | Service degraded | Some features unavailable; functionality reduced | warn |
| 1089 | 503 | Resource exhausted | Server out of resources (memory, connections, etc.) | critical |

### Refusal Code API Usage

```erlang
%% Get message for refusal code
{ok, Message} = erlmcp_refusal:get_message(1001).
%% => {ok, <<"Queue capacity exceeded">>}

%% Get full metadata
{ok, Code, HttpStatus, Message, Hint, Severity} =
    erlmcp_refusal:get_metadata(1001).

%% Format refusal as binary string
Formatted = erlmcp_refusal:format_refusal(1001).
%% => <<"Queue capacity exceeded. Reduce message rate...">>

%% Validate refusal code
true = erlmcp_refusal:is_valid_code(1001).
false = erlmcp_refusal:is_valid_code(9999).
```

---

## JSON-RPC 2.0 Standard Errors

Standard JSON-RPC 2.0 error codes as defined in RFC 7662.

| Code | Message | Meaning | Recovery |
|------|---------|---------|----------|
| -32700 | Parse error | Invalid JSON in request | Fix JSON syntax and retry |
| -32600 | Invalid Request | Malformed request object | Check request structure |
| -32601 | Method not found | Method does not exist | Verify method name |
| -32602 | Invalid params | Method parameters invalid | Check parameter types |
| -32603 | Internal error | Server error during execution | Retry with backoff |

### Usage Examples

```erlang
%% Encode parse error
erlmcp_json_rpc:error_parse(RequestId).
%% => Encoded JSON-RPC error response

%% Encode invalid params with context
erlmcp_json_rpc:error_invalid_params(RequestId,
    <<"limit must be between 1 and 1000">>).

%% Encode method not found
erlmcp_json_rpc:error_method_not_found(RequestId, <<"unknown_method">>).
```

---

## MCP-Specific Error Codes

### Core MCP Errors (-32001 to -32010)

| Code | Message | When to Use | Example |
|------|---------|------------|---------|
| -32001 | Resource not found | Resource URI does not exist | `GET /resources/doc://missing` |
| -32002 | Tool not found | Tool name not registered | `POST /tools/call {tool: "unknown"}` |
| -32003 | Prompt not found | Prompt template not found | `GET /prompts/get {name: "missing"}` |
| -32004 | Capability not supported | Server doesn't support capability | Request capability not in server capabilities |
| -32005 | Server not initialized | Server hasn't completed initialization | Request sent before `initialize` response |
| -32006 | Subscription failed | Cannot subscribe to resource | Resource not subscriptionable |
| -32007 | Validation failed | Input validation failure | Schema validation failed |
| -32008 | Transport error | I/O error on transport | Connection closed unexpectedly |
| -32009 | Request timeout | Operation exceeded timeout | Tool execution took > timeout_ms |
| -32010 | Rate limited | Rate limit exceeded | Too many requests in time window |

### Content Errors (-32011 to -32020)

| Code | Message | Trigger |
|------|---------|---------|
| -32011 | Tool description exceeds max length | Tool description > 10000 chars |
| -32012 | Message size exceeds maximum | Total message > 1MB |
| -32013 | Invalid content type | Unsupported MIME type |
| -32014 | Content too large | Content block exceeds limit |
| -32015 | Invalid encoding | Non-UTF-8 encoding |
| -32016 | Binary data too large | Base64 content > limit |
| -32017 | Text too long | Text field exceeds max |
| -32018 | Invalid MIME type | Malformed MIME type |
| -32019 | Unsupported media type | Media type not accepted |
| -32020 | Media type not acceptable | Client can't handle response type |

### Resource Errors (-32021 to -32030)

| Code | Message | Trigger |
|------|---------|---------|
| -32021 | Resource template not found | URI template not registered |
| -32022 | Invalid URI | URI format invalid |
| -32023 | URI syntax error | URI component parsing failed |
| -32024 | URI too long | URI exceeds max length |
| -32025 | Resource access denied | Permission check failed |
| -32026 | Resource already exists | Duplicate resource |
| -32027 | Resource locked | Resource under modification |
| -32028 | Resource version mismatch | Version conflict |
| -32029 | Template render failed | URI template expansion failed |
| -32030 | Invalid URI template | Template syntax invalid |

### Tool Errors (-32031 to -32040)

| Code | Message | Trigger |
|------|---------|---------|
| -32031 | Tool execution failed | Handler threw exception |
| -32032 | Tool execution timeout | Tool exceeded timeout_ms |
| -32033 | Tool execution cancelled | Request was cancelled |
| -32034 | Invalid tool arguments | Arguments don't match schema |
| -32035 | Tool is disabled | Tool marked as disabled |
| -32036 | Tool result too large | Result exceeds size limit |
| -32037 | Tool not allowed | Tool blocked by policy |
| -32038 | Maximum concurrent tools exceeded | Concurrent limit reached |
| -32039 | Tool dependency failed | Dependency tool failed |
| -32040 | Tool schema invalid | Schema definition invalid |

### Prompt Errors (-32041 to -32050)

| Code | Message | Trigger |
|------|---------|---------|
| -32042 | URL elicitation required | Interactive input needed |
| -32043 | Prompt argument missing | Required argument not provided |
| -32044 | Prompt render failed | Template expansion failed |
| -32045 | Invalid prompt arguments | Arguments type mismatch |
| -32046 | Sampling failed | LLM sampling error |
| -32047 | Sampling timeout | LLM request exceeded timeout |
| -32048 | Invalid model preferences | Model spec invalid |
| -32049 | Model not available | Model not accessible |
| -32050 | Sampling rate limited | Model API rate limit |

### Auth Errors (-32051 to -32060)

| Code | Message | Trigger |
|------|---------|---------|
| -32051 | Authentication failed | Invalid credentials |
| -32052 | Authorization failed | Permission denied |
| -32053 | Invalid credentials | Wrong password/token |
| -32054 | Token expired | Auth token no longer valid |
| -32055 | Insufficient permissions | Privilege level too low |
| -32056 | Access denied | Resource not accessible |
| -32057 | Session expired | Session no longer valid |
| -32058 | Session not found | Session ID doesn't exist |
| -32059 | Invalid token | Token format/signature invalid |
| -32060 | Unauthorized operation | Operation not allowed |

### Protocol Errors (-32061 to -32070)

| Code | Message | Trigger |
|------|---------|---------|
| -32061 | Unsupported protocol version | Version not implemented |
| -32062 | Protocol version mismatch | Version negotiation failed |
| -32063 | Capability negotiation failed | Can't agree on capabilities |
| -32064 | Incompatible capabilities | Capabilities conflict |
| -32065 | Method not supported | Method not in agreed capabilities |
| -32066 | Notification not supported | Notification type not supported |
| -32067 | Request ID invalid | ID format invalid |
| -32068 | Request ID conflict | ID already in use |
| -32069 | Batch request too large | Batch > max size |
| -32070 | Batch partial failure | Some batch items failed |

### Pagination Errors (-32071 to -32080)

| Code | Message | Trigger |
|------|---------|---------|
| -32071 | Invalid cursor | Cursor format invalid |
| -32072 | Cursor expired | Cursor no longer valid |
| -32073 | Pagination not supported | Endpoint doesn't support pagination |
| -32074 | Page size too large | Size > max_page_size |
| -32075 | Page size invalid | Size not positive integer |
| -32076 | Invalid offset | Offset out of range |
| -32077 | Cursor requires parameter | Missing required pagination param |
| -32078 | Cursor encoding failed | Can't decode cursor |
| -32079 | Pagination limit exceeded | Too many pagination requests |
| -32080 | Cursor position invalid | Position doesn't exist |

### Task Errors (-32081 to -32090)

| Code | Message | Trigger |
|------|---------|---------|
| -32081 | Task not found | Task ID doesn't exist |
| -32082 | Task already exists | Task ID in use |
| -32083 | Task failed | Task execution failed |
| -32084 | Task cancelled | Task was cancelled |
| -32085 | Task timeout | Task exceeded time limit |
| -32086 | Task state invalid | Illegal state transition |
| -32087 | Maximum concurrent tasks exceeded | Concurrent limit reached |
| -32088 | Task dependency failed | Dependency task failed |
| -32089 | Task result not ready | Result not yet available |
| -32090 | Task already completed | Task can't be modified |

### Progress Errors (-32091 to -32100)

| Code | Message | Trigger |
|------|---------|---------|
| -32091 | Invalid progress token | Token format invalid |
| -32092 | Progress token expired | Token no longer valid |
| -32093 | Progress update failed | Can't update progress |
| -32094 | Notification failed | Notification delivery failed |
| -32095 | Notification queue full | Queue at capacity |
| -32096 | Invalid notification type | Type not recognized |
| -32097 | Notification not delivered | Delivery timeout |
| -32098 | Progress value invalid | Value out of range |
| -32099 | Progress total invalid | Total <= 0 |

### Completion Errors (-32110 to -32113)

| Code | Message | Trigger |
|------|---------|---------|
| -32110 | Completion not found | Completion reference invalid |
| -32111 | Invalid completion reference | Reference format invalid |
| -32112 | Invalid completion argument | Argument doesn't match schema |
| -32113 | Completion failed | Completion execution failed |

### Custom Server Error (-32000)

| Code | Message | When to Use |
|------|---------|------------|
| -32000 | Server error | Implementation-specific server error not fitting other codes |

---

## Error Messages and Descriptions

### Error Message Format

Each error includes:

1. **Code**: Numeric identifier
2. **Message**: Brief human-readable description (50 chars max)
3. **Data**: Structured context (optional)

### Standard Error Message Constants

```erlang
%% JSON-RPC 2.0 messages
?JSONRPC_MSG_PARSE_ERROR       => <<"Parse error">>
?JSONRPC_MSG_INVALID_REQUEST   => <<"Invalid Request">>
?JSONRPC_MSG_METHOD_NOT_FOUND  => <<"Method not found">>
?JSONRPC_MSG_INVALID_PARAMS    => <<"Invalid params">>
?JSONRPC_MSG_INTERNAL_ERROR    => <<"Internal error">>

%% Core MCP messages
?MCP_MSG_RESOURCE_NOT_FOUND    => <<"Resource not found">>
?MCP_MSG_TOOL_NOT_FOUND        => <<"Tool not found">>
?MCP_MSG_NOT_INITIALIZED       => <<"Server not initialized">>
?MCP_MSG_TIMEOUT               => <<"Request timeout">>
?MCP_MSG_RATE_LIMITED          => <<"Rate limit exceeded">>

%% Content messages
?MCP_MSG_MESSAGE_TOO_LARGE     => <<"Message size exceeds maximum allowed">>
?MCP_MSG_INVALID_CONTENT_TYPE  => <<"Invalid content type">>
?MCP_MSG_INVALID_ENCODING      => <<"Invalid encoding">>

%% Tool messages
?MCP_MSG_TOOL_EXECUTION_FAILED => <<"Tool execution failed">>
?MCP_MSG_TOOL_TIMEOUT          => <<"Tool execution timeout">>
```

### Structured Error Data Examples

```erlang
%% Invalid parameters with field details
Data = #{
    <<"field">> => <<"limit">>,
    <<"reason">> => <<"must be between 1 and 1000">>,
    <<"received">> => 5000
},

%% Tool not found with context
Data = #{
    <<"tool">> => <<"undefined_tool">>,
    <<"available">> => [<<"tool_a">>, <<"tool_b">>]
},

%% Validation failed with errors list
Data = #{
    <<"errors">> => [
        #{<<"path">> => <<"params.query">>, <<"message">> => <<"required">>},
        #{<<"path">> => <<"params.limit">>, <<"message">> => <<"must be integer">>}
    ]
},

%% Message too large with limits
Data = #{
    <<"maxSize">> => 1048576,
    <<"actualSize">> => 2097152,
    <<"unit">> => <<"bytes">>
}
```

---

## Server-Side Error Handling Patterns

### Pattern 1: Throw Exceptions in Handlers

```erlang
%% Handler that validates then processes
handle_request(Params) ->
    case validate_args(Params) of
        ok ->
            process_request(Params);
        {error, Reason} ->
            throw({mcp_error, -32602, <<"Invalid params">>,
                   #{<<"reason">> => Reason}})
    end.

%% Server catches and formats
handle_call({Request}, From, State) ->
    try
        Result = handle_request(Request),
        {reply, {ok, Result}, State}
    catch
        throw:{mcp_error, Code, Message, Data} ->
            Response = erlmcp_json_rpc:encode_error_response(
                RequestId, Code, Message, Data),
            {reply, {error, Response}, State};
        error:Reason ->
            Response = erlmcp_json_rpc:error_internal(RequestId),
            {reply, {error, Response}, State}
    end.
```

### Pattern 2: Return Error Tuples

```erlang
%% Handler returns error tuples
process_tool_call(ToolName, Args) ->
    case erlmcp_server:get_tool(ToolName) of
        {ok, Tool} ->
            execute_tool(Tool, Args);
        {error, not_found} ->
            {error, -32002, <<"Tool not found">>,
             #{<<"tool">> => ToolName}}
    end.

%% Server handles error returns
case process_tool_call(Name, Args) of
    {ok, Result} ->
        Response = erlmcp_json_rpc:encode_response(Id, Result);
    {error, Code, Message, Data} ->
        Response = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data)
end.
```

### Pattern 3: Validation Error Accumulation

```erlang
%% Collect all validation errors
validate_request(Params) ->
    Errors = [],
    Errors1 = case validate_field_a(Params) of
        ok -> Errors;
        {error, E} -> [E | Errors]
    end,
    Errors2 = case validate_field_b(Params) of
        ok -> Errors1;
        {error, E} -> [E | Errors1]
    end,
    case Errors2 of
        [] -> ok;
        ErrorList ->
            {error, -32602, <<"Validation failed">>,
             #{<<"errors">> => ErrorList}}
    end.
```

### Pattern 4: Circuit Breaker Errors

```erlang
%% Check circuit breaker state
execute_with_circuit_breaker(ServiceKey, Fun) ->
    case erlmcp_circuit_breaker:status(ServiceKey) of
        closed ->
            case Fun() of
                {ok, Result} -> {ok, Result};
                {error, Reason} ->
                    erlmcp_circuit_breaker:record_failure(ServiceKey),
                    {error, -32031, <<"Tool execution failed">>,
                     #{<<"reason">> => Reason}}
            end;
        open ->
            {error, 1086, <<"Circuit breaker open">>,
             #{<<"service">> => ServiceKey}};
        half_open ->
            %% One request allowed
            Fun()
    end.
```

### Pattern 5: Rate Limiting

```erlang
%% Check rate limit before executing
handle_tool_call(ToolName, Args, ClientId) ->
    case erlmcp_rate_limiter:check(ClientId) of
        ok ->
            execute_tool(ToolName, Args);
        {exceeded, RemainingSeconds} ->
            {error, -32010, <<"Rate limit exceeded">>,
             #{<<"retryAfter">> => RemainingSeconds}}
    end.
```

### Pattern 6: Timeout Handling in Handlers

```erlang
%% Tool handler with timeout
call_tool_with_timeout(Tool, Args, TimeoutMs) ->
    case catch call_with_timeout(Tool, Args, TimeoutMs) of
        {ok, Result} ->
            {ok, Result};
        {error, timeout} ->
            {error, -32032, <<"Tool execution timeout">>,
             #{<<"timeoutMs">> => TimeoutMs}};
        {error, Reason} ->
            {error, -32031, <<"Tool execution failed">>,
             #{<<"reason">> => term_to_binary(Reason)}}
    end.

call_with_timeout(Tool, Args, TimeoutMs) ->
    Ref = make_ref(),
    Parent = self(),
    spawn_monitor(fun() ->
        Result = (Tool#mcp_tool.handler)(Args),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> Result
    after TimeoutMs ->
        {error, timeout}
    end.
```

### Server Initialization Errors

```erlang
%% Server must be initialized before accepting requests
handle_call({resources, list}, _From, State) when not State#state.initialized ->
    Response = erlmcp_json_rpc:error_not_initialized(RequestId),
    {reply, {error, Response}, State};

%% After successful initialization
handle_call({initialize, Params}, _From, State) ->
    case validate_protocol_version(Params) of
        ok ->
            NewState = State#state{initialized = true},
            {reply, {ok, InitResponse}, NewState};
        {error, Code, Message} ->
            Response = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
            {reply, {error, Response}, State}
    end.
```

---

## Client-Side Error Recovery

### Recovery Pattern 1: Retry with Exponential Backoff

```erlang
%% Retry mechanism for transient errors
retry_call(Method, Params, MaxAttempts) ->
    retry_call(Method, Params, 1, MaxAttempts, 1000).

retry_call(_Method, _Params, Attempt, MaxAttempts, _Delay)
  when Attempt > MaxAttempts ->
    {error, max_retries_exceeded};

retry_call(Method, Params, Attempt, MaxAttempts, Delay) ->
    case erlmcp_client:call_tool(Client, Method, Params) of
        {ok, Result} ->
            {ok, Result};
        {error, Code, Message, _Data} when is_retryable(Code) ->
            timer:sleep(Delay),
            NewDelay = min(Delay * 2, 30000),  % Max 30s delay
            retry_call(Method, Params, Attempt + 1, MaxAttempts, NewDelay);
        {error, Code, Message, Data} ->
            {error, Code, Message, Data}
    end.

%% Determine if error is retryable
is_retryable(-32009) -> true;   % Timeout
is_retryable(-32010) -> true;   % Rate limited
is_retryable(-32008) -> true;   % Transport error
is_retryable(Code) when Code >= 1056, Code =< 1060 -> true;  % Rate limits
is_retryable(Code) when Code >= 1005, Code =< 1005 -> true;  % Backpressure
is_retryable(_) -> false.
```

### Recovery Pattern 2: Fallback Resources

```erlang
%% Try primary resource, fallback to secondary
read_resource_with_fallback(Client, PrimaryUri, FallbackUri) ->
    case erlmcp_client:read_resource(Client, PrimaryUri) of
        {ok, Content} ->
            {ok, Content};
        {error, 1046, _, _} ->  % Resource not found
            erlmcp_client:read_resource(Client, FallbackUri);
        {error, Code, Message, Data} ->
            {error, Code, Message, Data}
    end.
```

### Recovery Pattern 3: Circuit Breaker on Client

```erlang
%% Client-side circuit breaker for service resilience
call_with_circuit_breaker(Client, Tool, Args) ->
    case erlmcp_circuit_breaker:status(Tool) of
        closed ->
            case erlmcp_client:call_tool(Client, Tool, Args) of
                {ok, Result} ->
                    erlmcp_circuit_breaker:record_success(Tool),
                    {ok, Result};
                {error, Code, Msg, Data} ->
                    erlmcp_circuit_breaker:record_failure(Tool),
                    {error, Code, Msg, Data}
            end;
        open ->
            {error, 1086, <<"Circuit breaker open">>,
             #{<<"tool">> => Tool}};
        half_open ->
            erlmcp_client:call_tool(Client, Tool, Args)
    end.
```

### Recovery Pattern 4: Auth Token Refresh

```erlang
%% Refresh token on 401 and retry
call_with_token_refresh(Client, Method, Params) ->
    case erlmcp_client:call(Client, Method, Params) of
        {ok, Result} ->
            {ok, Result};
        {error, -32051, _, _} ->  % Authentication failed
            case refresh_auth_token() of
                {ok, NewToken} ->
                    erlmcp_client:set_auth_token(Client, NewToken),
                    erlmcp_client:call(Client, Method, Params);
                {error, Reason} ->
                    {error, -32051, <<"Authentication failed">>,
                     #{<<"reason">> => Reason}}
            end;
        {error, Code, Msg, Data} ->
            {error, Code, Msg, Data}
    end.
```

### Recovery Pattern 5: Error Categorization for User Display

```erlang
%% Present user-friendly error messages based on code
user_friendly_message({error, Code, ServerMessage, Data}) ->
    case categorize_error(Code) of
        transient ->
            "Request failed temporarily. Please try again in a moment.";
        auth ->
            "Authentication required. Please log in again.";
        validation ->
            format_validation_errors(Data),
            "Please check your input and try again.";
        not_found ->
            "Resource not found. Please verify the name and try again.";
        rate_limited ->
            format_retry_after(Data),
            "Server is busy. Please wait before trying again.";
        server_error ->
            "Server error occurred. Please contact support if this persists.";
        unknown ->
            ServerMessage
    end.

categorize_error(Code) when Code >= -32009, Code =< -32008 -> transient;
categorize_error(Code) when Code >= -32051, Code =< -32060 -> auth;
categorize_error(Code) when Code >= -32602, Code =< -32602 -> validation;
categorize_error(Code) when Code =:= -32001; Code =:= -32002; Code =:= -32003 -> not_found;
categorize_error(Code) when Code >= -32010, Code =< -32010 -> rate_limited;
categorize_error(Code) when Code >= 1001, Code =< 1089 -> refusal;
categorize_error(_) -> unknown.
```

---

## Timeout Handling

### Timeout Configuration

Default timeout values in erlmcp:

```erlang
%% Client request timeout
-define(DEFAULT_CLIENT_TIMEOUT_MS, 5000).  % 5 seconds

%% Server initialization timeout
-define(MCP_DEFAULT_INIT_TIMEOUT_MS, 30000).  % 30 seconds

%% Tool execution timeout
-define(TOOL_EXECUTION_TIMEOUT_MS, 30000).  % 30 seconds

%% LLM sampling timeout
-define(SAMPLING_TIMEOUT_MS, 60000).  % 60 seconds

%% Connection establishment timeout
-define(CONNECT_TIMEOUT_MS, 10000).  % 10 seconds
```

### Timeout Error Codes

| Code | Message | Applies To |
|------|---------|-----------|
| -32009 | Request timeout | General RPC request |
| -32032 | Tool execution timeout | Tool call |
| -32047 | Sampling timeout | LLM sampling request |
| -32085 | Task timeout | Background task |
| 1069 | Operation timeout | Generic operation |
| 1091 | Elicitation timeout | Interactive input |
| 1098 | Task timeout | Experimental task |

### Timeout Response Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32009,
    "message": "Request timeout",
    "data": {
      "timeoutMs": 5000,
      "operation": "tools/call",
      "tool": "long_running_tool"
    }
  }
}
```

### Client-Side Timeout Handling

```erlang
%% Configure timeout per client
{ok, Client} = erlmcp_client:start_link(
    {stdio, []},
    #{timeout => 10000}  % 10 seconds
).

%% Timeout on specific request
case erlmcp_client:call_tool(Client, ToolName, Args) of
    {ok, Result} ->
        {ok, Result};
    {error, -32009, <<"Request timeout">>, Data} ->
        % Tool took too long
        Timeout = maps:get(<<"timeoutMs">>, Data),
        retry_with_longer_timeout(Client, ToolName, Args, Timeout * 2)
end.

%% Implement custom timeout for specific operations
call_with_custom_timeout(Client, Tool, Args, TimeoutMs) ->
    Ref = make_ref(),
    Parent = self(),
    Handler = spawn(fun() ->
        Result = erlmcp_client:call_tool(Client, Tool, Args),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, {ok, Result}} ->
            {ok, Result};
        {Ref, {error, Code, Msg, Data}} ->
            {error, Code, Msg, Data}
    after TimeoutMs ->
        exit(Handler, kill),
        {error, -32009, <<"Request timeout">>,
         #{<<"timeoutMs">> => TimeoutMs}}
    end.
```

### Server-Side Timeout Implementation

```erlang
%% Tool execution with timeout
execute_tool_with_timeout(Tool, Args, TimeoutMs) ->
    Ref = erlang:monitor(process, Tool),
    Tool ! {execute, Args},
    receive
        {Ref, result, Result} ->
            {ok, Result};
        {Ref, error, Reason} ->
            {error, -32031, <<"Tool execution failed">>,
             #{<<"reason">> => Reason}};
        {'DOWN', Ref, process, _, Reason} ->
            {error, -32031, <<"Tool execution failed">>,
             #{<<"reason">> => <<"Tool process crashed">>}}
    after TimeoutMs ->
        erlang:demonitor(Ref, [flush]),
        exit(Tool, kill),
        {error, -32032, <<"Tool execution timeout">>,
         #{<<"timeoutMs">> => TimeoutMs}}
    end.

%% Initialize with timeout (30s default)
handle_initialize(Params, State) ->
    TimeoutMs = maps:get(timeout, State#state.init_timeout_ms, 30000),
    Timer = erlang:send_after(TimeoutMs, self(), init_timeout),
    NewState = State#state{init_timeout_ref = Timer},

    case process_initialization(Params) of
        {ok, InitState} ->
            erlang:cancel_timer(Timer),
            {reply, {ok, InitState}, NewState#state{initialized = true}};
        {error, Reason} ->
            erlang:cancel_timer(Timer),
            {reply, {error, Reason}, State}
    end.

handle_info(init_timeout, State) ->
    Response = erlmcp_json_rpc:encode_error_response(
        1, -32005, ?MCP_MSG_NOT_INITIALIZED,
        #{<<"reason">> => <<"Initialization timeout">>}),
    {reply, {error, Response}, State}.
```

---

## Validation Error Responses

### Schema Validation Errors

```erlang
%% Validate against JSON Schema
validate_tool_args(ToolName, Schema, Args) ->
    case jesse:validate(Schema, Args) of
        {ok, ValidArgs} ->
            {ok, ValidArgs};
        {error, Errors} ->
            ErrorData = #{
                <<"errors">> => format_jesse_errors(Errors),
                <<"schema">> => ToolName
            },
            {error, -32602, <<"Invalid params">>, ErrorData}
    end.

format_jesse_errors(Errors) ->
    [#{
        <<"path">> => erlang:list_to_binary(Path),
        <<"message">> => format_error_message(Error)
    } || {Path, Error} <- Errors].
```

### Common Validation Error Scenarios

| Scenario | Code | Data |
|----------|------|------|
| Missing required field | -32602 | `{"field": "name", "reason": "required"}` |
| Type mismatch | -32602 | `{"field": "limit", "expected": "integer", "got": "string"}` |
| Out of range | -32602 | `{"field": "offset", "reason": "must be >= 0"}` |
| Invalid format | 1023 | `{"field": "uri", "reason": "invalid format"}` |
| Schema validation failure | 1022 | `{"errors": [...]}` |

### Multi-Field Validation Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "errors": [
        {
          "path": "params.query",
          "message": "required field missing"
        },
        {
          "path": "params.limit",
          "message": "must be between 1 and 1000, got 5000"
        },
        {
          "path": "params.sortOrder",
          "message": "must be 'asc' or 'desc'"
        }
      ]
    }
  }
}
```

### Custom Validation Error Helper

```erlang
validation_error(Id, ValidationErrors) when is_list(ValidationErrors) ->
    FormattedErrors = [format_validation_error(E) || E <- ValidationErrors],
    erlmcp_json_rpc:encode_error_response(
        Id, -32602, <<"Invalid params">>,
        #{<<"errors">> => FormattedErrors}).

format_validation_error({Field, Reason}) ->
    #{
        <<"field">> => Field,
        <<"reason">> => Reason
    };
format_validation_error({Field, Expected, Got}) ->
    #{
        <<"field">> => Field,
        <<"expected">> => Expected,
        <<"received">> => Got
    }.
```

### Path and URI Validation Errors

```erlang
%% Validate resource URI
validate_resource_uri(Uri) ->
    case erlmcp_path_canonicalizer:canonicalize(Uri) of
        {ok, CanonicalPath} ->
            {ok, CanonicalPath};
        {error, traversal_detected} ->
            {error, 1036, <<"Path traversal detected">>,
             #{<<"uri">> => Uri}};
        {error, symlink_detected} ->
            {error, 1038, <<"Symlink traversal detected">>,
             #{<<"uri">> => Uri}};
        {error, out_of_bounds} ->
            {error, 1039, <<"URI out of bounds">>,
             #{<<"uri">> => Uri}};
        {error, invalid_format} ->
            {error, 1037, <<"Invalid path format">>,
             #{<<"uri">> => Uri}}
    end.
```

---

## Error Handling Best Practices

### Do's

✅ **Always include error code** - Enables error categorization
✅ **Provide descriptive message** - Under 100 characters for brevity
✅ **Add data context** - Help clients understand remediation
✅ **Use standard codes** - Consistency across implementations
✅ **Log with full context** - Code, message, data, timestamp, request ID
✅ **Return appropriate HTTP status** - Match JSON-RPC error to HTTP status
✅ **Implement exponential backoff** - For transient errors
✅ **Set reasonable timeouts** - Default 5-30 seconds depending on operation

### Don'ts

❌ **Don't leak stack traces** - Expose internal implementation
❌ **Don't use unknown error codes** - Stick to standardized ranges
❌ **Don't swallow errors silently** - Always report failures
❌ **Don't set timeouts too short** - <1 second risks false timeouts
❌ **Don't retry infinitely** - Implement max retry limits
❌ **Don't include sensitive data** - No passwords, keys, or PII
❌ **Don't use vague messages** - "Error" doesn't help clients
❌ **Don't ignore validation errors** - Always validate before processing

---

## Error Handling Checklist

- [ ] Error response includes `code`, `message`, and optional `data`
- [ ] Error code is from standardized ranges
- [ ] Message is human-readable and < 100 characters
- [ ] Data field provides actionable context
- [ ] Validation errors list all failures (not first-fail-only)
- [ ] Timeout errors include configured timeout value
- [ ] Auth errors suggest remediation step
- [ ] Rate limit errors include retry-after if available
- [ ] Refusal codes use appropriate HTTP status
- [ ] Server logs error with full context
- [ ] Client implements exponential backoff for retries
- [ ] Client categorizes errors for user presentation
- [ ] Circuit breakers trigger on repeated failures
- [ ] No sensitive data in error responses
- [ ] Error codes documented for API consumers

---

## References

- **MCP Protocol Spec**: Version 2025-11-25
- **JSON-RPC 2.0**: RFC 7662
- **HTTP Status Codes**: RFC 7231
- **Erlang OTP Patterns**: gen_server error handling
- **Implementation**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- **Refusal Taxonomy**: `/home/user/erlmcp/apps/erlmcp_core/include/erlmcp_refusal.hrl`

