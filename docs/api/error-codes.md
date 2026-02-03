# erlmcp Error Code Reference

## Overview

erlmcp uses JSON-RPC 2.0 error codes and extends them with MCP-specific errors. All errors follow the JSON-RPC error response format:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32002,
    "message": "Tool not found",
    "data": {
      "tool": "nonexistent_tool"
    }
  }
}
```

## Error Response Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `code` | integer | Yes | Numeric error code |
| `message` | string | Yes | Human-readable error message |
| `data` | object/string | No | Additional error details |

## Error Categories

| Category | Code Range | Description |
|----------|------------|-------------|
| JSON-RPC Standard | -32700 to -32600 | Protocol-level errors |
| MCP Core | -32001 to -32010 | Core MCP errors |
| Content | -32011 to -32020 | Content and message errors |
| Resource | -32021 to -32030 | Resource errors |
| Tool | -32031 to -32040 | Tool execution errors |
| Prompt | -32041 to -32050 | Prompt and sampling errors |
| Auth | -32051 to -32060 | Authentication errors |
| Protocol | -32061 to -32070 | Protocol negotiation errors |
| Pagination | -32071 to -32080 | Cursor and pagination errors |
| Task | -32081 to -32090 | Asynchronous task errors |
| Progress | -32091 to -32100 | Progress notification errors |
| Completion | -32110 to -32113 | LLM completion errors |
| Experimental | 1090 to 1099 | Experimental feature errors |

---

## JSON-RPC 2.0 Standard Errors

### -32700: Parse Error

Invalid JSON was received by the server.

```json
{
  "error": {
    "code": -32700,
    "message": "Parse error",
    "data": "Unexpected token at position 15"
  }
}
```

**Causes:**
- Malformed JSON syntax
- Invalid UTF-8 encoding
- Truncated message

**Troubleshooting:**
1. Validate JSON with a linter
2. Check for UTF-8 encoding issues
3. Verify message completeness

### -32600: Invalid Request

The JSON sent is not a valid Request object.

```json
{
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": "Missing required field: method"
  }
}
```

**Causes:**
- Missing required fields (jsonrpc, method, id)
- Invalid field types
- Batch request with invalid items

**Troubleshooting:**
1. Verify all required fields are present
2. Check field types match specification
3. For batches, ensure each item is valid

### -32601: Method Not Found

The method does not exist or is not available.

```json
{
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": {
      "method": "nonexistent/method"
    }
  }
}
```

**Causes:**
- Typo in method name
- Method not supported by server
- Capability not declared

**Troubleshooting:**
1. Check method name spelling
2. Verify server capabilities
3. Ensure proper initialization

### -32602: Invalid Params

Invalid method parameter(s).

```json
{
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "details": "Missing required parameter: uri"
    }
  }
}
```

**Causes:**
- Missing required parameters
- Wrong parameter types
- Parameter validation failed

**Troubleshooting:**
1. Check method signature in documentation
2. Verify all required parameters are present
3. Validate parameter types

### -32603: Internal Error

Internal JSON-RPC error (unexpected condition).

```json
{
  "error": {
    "code": -32603,
    "message": "Internal error"
  }
}
```

**Causes:**
- Server-side bug
- Resource exhaustion
- Unexpected exception

**Troubleshooting:**
1. Check server logs for stack trace
2. Report bug with reproduction steps
3. Restart server if resource exhaustion

---

## MCP Core Errors (-32001 to -32010)

### -32001: Resource Not Found

Requested resource does not exist.

```json
{
  "error": {
    "code": -32001,
    "message": "Resource not found",
    "data": {
      "uri": "mcp://nonexistent"
    }
  }
}
```

**Causes:**
- URI typo
- Resource not registered
- Resource was deleted

**Troubleshooting:**
1. List available resources with `resources/list`
2. Verify URI is correct
3. Check resource registration

### -32002: Tool Not Found

Requested tool does not exist.

```json
{
  "error": {
    "code": -32002,
    "message": "Tool not found",
    "data": {
      "tool": "nonexistent_tool"
    }
  }
}
```

**Causes:**
- Tool name typo
- Tool not registered
- Tool was deleted

**Troubleshooting:**
1. List available tools with `tools/list`
2. Verify tool name spelling
3. Check tool registration

### -32003: Prompt Not Found

Requested prompt template does not exist.

```json
{
  "error": {
    "code": -32003,
    "message": "Prompt not found",
    "data": {
      "prompt": "nonexistent_prompt"
    }
  }
}
```

### -32004: Capability Not Supported

Requested operation requires a capability not supported by the server.

```json
{
  "error": {
    "code": -32004,
    "message": "Capability not supported",
    "data": {
      "capability": "completions"
    }
  }
}
```

**Causes:**
- Server doesn't support the feature
- Capability not declared during initialization

**Troubleshooting:**
1. Check server capabilities in initialize response
2. Use only declared capabilities
3. Upgrade server if needed

### -32005: Not Initialized

Operation called before initialization completed.

```json
{
  "error": {
    "code": -32005,
    "message": "Server not initialized",
    "data": {
      "phase": "pre_initialization"
    }
  }
}
```

**Causes:**
- Client sent request before `initialize`
- Waiting for `notifications/initialized`

**Troubleshooting:**
1. Send `initialize` request first
2. Wait for `notifications/initialized`
3. Then proceed with other requests

### -32006: Subscription Failed

Failed to subscribe to resource updates.

```json
{
  "error": {
    "code": -32006,
    "message": "Subscription failed",
    "data": {
      "uri": "mcp://config",
      "reason": "already_subscribed"
    }
  }
}
```

### -32007: Validation Failed

Request validation failed.

```json
{
  "error": {
    "code": -32007,
    "message": "Validation failed",
    "data": {
      "field": "uri",
      "constraint": "valid_uri"
    }
  }
}
```

### -32008: Transport Error

Transport layer communication error.

```json
{
  "error": {
    "code": -32008,
    "message": "Transport error",
    "data": {
      "reason": "connection_reset"
    }
  }
}
```

### -32009: Timeout

Request processing exceeded timeout.

```json
{
  "error": {
    "code": -32009,
    "message": "Request timeout",
    "data": {
      "timeout_ms": 30000,
      "elapsed_ms": 30001
    }
  }
}
```

**Causes:**
- Long-running operation
- Server overload
- Network latency

**Troubleshooting:**
1. Increase timeout if needed
2. Check server load
3. Use progress tokens for long operations

### -32010: Rate Limited

Request rate limit exceeded.

```json
{
  "error": {
    "code": -32010,
    "message": "Rate limit exceeded",
    "data": {
      "limit": 100,
      "window": 60,
      "retry_after": 30
    }
  }
}
```

**Troubleshooting:**
1. Implement exponential backoff
2. Check `retry_after` value
3. Reduce request frequency

---

## Content Errors (-32011 to -32020)

### -32011: Tool Description Too Long

Tool description exceeds maximum length (10,000 characters).

```json
{
  "error": {
    "code": -32011,
    "message": "Tool description exceeds maximum length",
    "data": {
      "actual_length": 15000,
      "max_length": 10000
    }
  }
}
```

### -32012: Message Too Large

Message size exceeds maximum allowed (16 MB default).

```json
{
  "error": {
    "code": -32012,
    "message": "Message size exceeds maximum allowed",
    "data": {
      "actual_size": 20971520,
      "max_size": 16777216,
      "unit": "bytes"
    }
  }
}
```

**Troubleshooting:**
1. Split large content into chunks
2. Use resource references instead of inline content
3. Configure larger limit if needed

### -32013: Invalid Content Type

Unsupported or invalid content type.

```json
{
  "error": {
    "code": -32013,
    "message": "Invalid content type",
    "data": {
      "content_type": "application/xml"
    }
  }
}
```

### -32014: Content Too Large

Individual content item exceeds size limit.

### -32015: Invalid Encoding

Content encoding is not valid UTF-8.

```json
{
  "error": {
    "code": -32015,
    "message": "Invalid encoding",
    "data": {
      "encoding": "utf-8",
      "position": 1234
    }
  }
}
```

---

## Resource Errors (-32021 to -32030)

### -32021: Resource Template Not Found

Requested resource template does not exist.

```json
{
  "error": {
    "code": -32021,
    "message": "Resource template not found",
    "data": {
      "uri_template": "logs://{date}"
    }
  }
}
```

### -32022: Invalid URI

URI format is invalid.

```json
{
  "error": {
    "code": -32022,
    "message": "Invalid URI",
    "data": {
      "uri": "not-a-valid-uri",
      "reason": "missing_scheme"
    }
  }
}
```

### -32023: URI Syntax Error

URI has syntax errors.

### -32025: Resource Access Denied

Client lacks permission to access resource.

```json
{
  "error": {
    "code": -32025,
    "message": "Resource access denied",
    "data": {
      "uri": "file:///etc/passwd",
      "required_permission": "read:protected"
    }
  }
}
```

**Troubleshooting:**
1. Check authentication status
2. Verify resource permissions
3. Contact administrator for access

### -32029: Template Render Failed

Failed to render resource template.

```json
{
  "error": {
    "code": -32029,
    "message": "Template render failed",
    "data": {
      "uri_template": "logs://{date}",
      "reason": "missing_variable: date"
    }
  }
}
```

---

## Tool Errors (-32031 to -32040)

### -32031: Tool Execution Failed

Tool handler threw an exception.

```json
{
  "error": {
    "code": -32031,
    "message": "Tool execution failed",
    "data": {
      "tool": "database_query",
      "reason": "connection_timeout"
    }
  }
}
```

### -32032: Tool Timeout

Tool execution exceeded timeout.

```json
{
  "error": {
    "code": -32032,
    "message": "Tool execution timeout",
    "data": {
      "tool": "long_operation",
      "timeout_ms": 30000
    }
  }
}
```

### -32033: Tool Cancelled

Tool execution was cancelled.

```json
{
  "error": {
    "code": -32033,
    "message": "Tool execution cancelled",
    "data": {
      "tool": "long_operation",
      "reason": "user_requested"
    }
  }
}
```

### -32034: Invalid Tool Arguments

Tool arguments don't match schema.

```json
{
  "error": {
    "code": -32034,
    "message": "Invalid tool arguments",
    "data": {
      "tool": "calculate",
      "details": "Missing required parameter: b"
    }
  }
}
```

---

## Prompt Errors (-32041 to -32050)

### -32043: Prompt Argument Missing

Required prompt argument not provided.

```json
{
  "error": {
    "code": -32043,
    "message": "Prompt argument missing",
    "data": {
      "prompt": "write_essay",
      "argument": "topic"
    }
  }
}
```

### -32044: Prompt Render Failed

Failed to render prompt template.

### -32046: Sampling Failed

LLM sampling operation failed.

```json
{
  "error": {
    "code": -32046,
    "message": "Sampling failed",
    "data": {
      "reason": "model_unavailable"
    }
  }
}
```

---

## Authentication Errors (-32051 to -32060)

### -32051: Authentication Failed

Authentication credentials invalid.

```json
{
  "error": {
    "code": -32051,
    "message": "Authentication failed",
    "data": {
      "reason": "invalid_token"
    }
  }
}
```

### -32052: Authorization Failed

Client lacks permission for operation.

```json
{
  "error": {
    "code": -32052,
    "message": "Authorization failed",
    "data": {
      "operation": "tools/call:admin_tool",
      "required_role": "admin"
    }
  }
}
```

### -32053: Invalid Credentials

Provided credentials are invalid.

### -32054: Token Expired

Authentication token has expired.

```json
{
  "error": {
    "code": -32054,
    "message": "Token expired",
    "data": {
      "expired_at": 1640995200
    }
  }
}
```

**Troubleshooting:**
1. Refresh authentication token
2. Re-authenticate with server
3. Check token expiration time

### -32056: Access Denied

General access denied error.

---

## Protocol Errors (-32061 to -32070)

### -32061: Unsupported Protocol Version

Protocol version not supported.

```json
{
  "error": {
    "code": -32061,
    "message": "Unsupported protocol version",
    "data": {
      "requested_version": "2020-01-01",
      "supported_versions": ["2025-11-25"]
    }
  }
}
```

### -32062: Protocol Version Mismatch

Client and server protocol versions incompatible.

### -32063: Capability Negotiation Failed

Failed to negotiate capabilities.

---

## Pagination Errors (-32071 to -32080)

### -32071: Invalid Cursor

Pagination cursor is invalid.

```json
{
  "error": {
    "code": -32071,
    "message": "Invalid cursor",
    "data": {
      "cursor": "invalid_cursor_string"
    }
  }
}
```

### -32072: Cursor Expired

Pagination cursor has expired.

```json
{
  "error": {
    "code": -32072,
    "message": "Cursor expired",
    "data": {
      "cursor": "expired_cursor",
      "ttl_seconds": 300
    }
  }
}
```

**Troubleshooting:**
1. Restart pagination from beginning
2. Reduce page size to process faster
3. Check cursor TTL configuration

---

## Task Errors (-32081 to -32090)

### -32081: Task Not Found

Asynchronous task does not exist.

```json
{
  "error": {
    "code": -32081,
    "message": "Task not found",
    "data": {
      "taskId": "task-123"
    }
  }
}
```

### -32083: Task Failed

Task execution failed.

```json
{
  "error": {
    "code": -32083,
    "message": "Task failed",
    "data": {
      "taskId": "task-123",
      "reason": "dependency_failed"
    }
  }
}
```

### -32084: Task Cancelled

Task was cancelled.

### -32085: Task Timeout

Task execution exceeded timeout.

---

## Progress Errors (-32091 to -32100)

### -32091: Invalid Progress Token

Progress token is invalid.

```json
{
  "error": {
    "code": -32091,
    "message": "Invalid progress token",
    "data": {
      "progressToken": "invalid_token"
    }
  }
}
```

### -32092: Progress Token Expired

Progress token has expired.

### -32094: Notification Failed

Failed to send notification.

```json
{
  "error": {
    "code": -32094,
    "message": "Notification failed",
    "data": {
      "notification_type": "resources/updated",
      "reason": "subscriber_not_found"
    }
  }
}
```

---

## Completion Errors (-32110 to -32113)

### -32110: Completion Not Found

Completion reference not found.

### -32111: Invalid Completion Reference

Completion reference format invalid.

### -32112: Invalid Completion Argument

Completion argument invalid.

### -32113: Completion Failed

Completion operation failed.

---

## Experimental Errors (1090-1099)

### 1090: Elicitation Failed

URL elicitation operation failed.

### 1091: Elicitation Timeout

Elicitation operation timed out.

### 1095: Task Not Found (Positive)

Alternative task not found code for experimental features.

---

## Error Handling Best Practices

### Client-Side

1. **Always check for error responses** before processing results
2. **Log error details** for debugging
3. **Implement retry logic** for transient errors
4. **Show user-friendly messages** based on error codes

```erlang
handle_response({ok, Response}) ->
    case maps:get(<<"error">>, Response) of
        undefined ->
            process_result(maps_get(<<"result">>, Response));
        Error ->
            handle_error(Error)
    end.

handle_error(#{<<"code">> := Code, <<"message">> := Message}) ->
    case Code of
        -32009 -> logger:warning("Request timeout, retrying...");
        -32010 -> timer:sleep(maps_get(<<"retry_after">>, Error#{
                                    <<"data">> => #{<<"retry_after">> => 30}
                                }, 30) * 1000),
                 retry();
        _ -> logger:error("Error ~p: ~s", [Code, Message])
    end.
```

### Server-Side

1. **Use specific error codes** for different failure modes
2. **Include helpful data** field with context
3. **Log errors** before sending response
4. **Sanitize sensitive data** from error messages

```erlang
send_error(RequestId, Error) when is_record(Error, mcp_error) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"error">> => #{
            <<"code">> => Error#mcp_error.code,
            <<"message">> => Error#mcp_error.message,
            <<"data">> => sanitize_data(Error#mcp_error.data)
        }
    },
    logger:error("Sending error: ~p", [Error]),
    send_response(Response).
```

---

## Severity Levels

| Severity | Error Codes | Action |
|----------|-------------|--------|
| **Critical** | -32700, -32600, -32603 | Report immediately, may indicate bugs |
| **Error** | -32601, -32602, -32001 to -32010 | Fix request and retry |
| **Warning** | -32011 to -32080 | May indicate configuration issues |
| **Info** | Others | Informational, log only |

---

## Testing Error Scenarios

### Using erlmcp_test_helpers

```erlang
%% Generate error responses for testing
{ok, ParseError} = erlmcp_json_rpc:create_error_response(
    1,
    ?JSONRPC_PARSE_ERROR,
    <<"Invalid JSON">>
).

%% Validate error codes
true = erlmcp_json_rpc:validate_error_code(-32002).
false = erlmcp_json_rpc:validate_error_code(-99999).

%% Check error categories
mcp_core = erlmcp_json_rpc:error_category(-32001).
tool = erlmcp_json_rpc:error_category(-32032).

%% Get error severity
critical = erlmcp_json_rpc:error_severity(-32700).
error = erlmcp_json_rpc:error_severity(-32002).
warning = erlmcp_json_rpc:error_severity(-32011).
```
