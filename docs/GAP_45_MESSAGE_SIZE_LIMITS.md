# Gap #45: Message Size Limits Implementation

**Status**: ✅ IMPLEMENTED (Phase 3)
**Specification**: MCP 2025-11-25 Compliance
**Priority**: MEDIUM
**Effort**: 1-2 hours
**Implementation Date**: 2026-01-27

## Overview

Gap #45 implements configurable message size limits for all MCP transports according to the MCP 2025-11-25 specification. This ensures:

- Protection against denial-of-service attacks via oversized messages
- Configurable limits per transport type
- Proper error responses (JSON-RPC and HTTP 413)
- Specification compliance

## Specification Requirements

From MCP 2025-11-25:

1. **Enforce maximum message size limits**
2. **Configurable per transport**
3. **Default limits: 16MB total message**
4. **Return error for oversized messages**
5. **Configuration via sys.config**

## Implementation Components

### 1. Header File (`include/erlmcp.hrl`)

Added constants for message size limits:

```erlang
-define(MCP_DEFAULT_MESSAGE_SIZE_LIMIT, 16777216).      %% 16 MB
-define(MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT, 16777216).    %% 16 MB
-define(MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT, 16777216).    %% 16 MB
-define(MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT, 16777216).   %% 16 MB
-define(MCP_MIN_MESSAGE_SIZE_LIMIT, 1024).              %% 1 KB minimum
-define(MCP_MAX_CONFIGURABLE_SIZE_LIMIT, 104857600).   %% 100 MB maximum
-define(MCP_ERROR_MESSAGE_TOO_LARGE, -32012).
-define(MCP_MSG_MESSAGE_TOO_LARGE, <<"Message size exceeds maximum allowed">>).
```

Added error code to valid error codes list:

```erlang
-define(VALID_ERROR_CODES, [
    ...
    -32012   % Message too large (Gap #45)
]).
```

### 2. Message Size Validation Module (`src/erlmcp_message_size.erl`)

New module provides comprehensive message size validation:

#### Public API

```erlang
%% Get configured limit for transport
get_limit(TransportType) -> non_neg_integer()

%% Validate message size
validate_message_size(TransportType, Message) -> ok | {error, Reason}
validate_message_size(Message, CustomLimit, TransportType) -> ok | {error, Reason}

%% Transport-specific validation
validate_http_body_size(Body) -> ok | {error, Reason}
validate_sse_event_size(Event) -> ok | {error, Reason}
validate_websocket_size(Message) -> ok | {error, Reason}
validate_tcp_size(Message) -> ok | {error, Reason}
validate_stdio_size(Message) -> ok | {error, Reason}

%% Error generation
get_max_size_error(MaxSize) -> binary()  %% JSON-RPC error response
get_http_413_error() -> {http, binary()}  %% HTTP 413 response

%% Configuration
get_size_limit_config() -> map()
```

#### Features

- Per-transport configuration (HTTP, SSE, WebSocket, TCP, Stdio)
- Human-readable size formatting (B, KB, MB, GB)
- Error responses with detailed metadata
- Fallback to defaults if configuration missing
- Type-safe implementation

### 3. JSON-RPC Integration (`src/erlmcp_json_rpc.erl`)

Enhanced JSON-RPC module with message size validation:

```erlang
%% New overloaded decode_message/2 with transport type
decode_message(Json, TransportType) -> {ok, Message} | {error, Reason}

%% New error helper function
error_message_too_large(Id, MaxSize) -> binary()
```

### 4. Configuration (`config/sys.config`)

Added configuration section for message size limits:

```erlang
{erlmcp, [
    {message_size_limits, #{
        default => 16777216,       % 16 MB
        http_body => 16777216,     % HTTP POST body
        sse_event => 16777216,     % SSE events
        websocket => 16777216,     % WebSocket messages
        tcp => 16777216,           % TCP messages
        stdio => 16777216          % Stdio messages
    }}
]}
```

All keys are optional - module uses defaults if missing.

## Validation Flow

```
Message Received
       ↓
[Check Message Size]
       ↓
Size > Limit?
   /        \
  NO        YES
  ↓          ↓
 OK      Return Error:
         - JSON-RPC: -32012 error
         - HTTP: 413 Payload Too Large
         - Details: maxSize, unit, readable format
```

## Error Responses

### JSON-RPC Error (All Transports except HTTP status)

```json
{
  "jsonrpc": "2.0",
  "id": request_id,
  "error": {
    "code": -32012,
    "message": "Message size exceeds maximum allowed",
    "data": {
      "maxSize": 16777216,
      "unit": "bytes",
      "maxSizeReadable": "16.00 MB"
    }
  }
}
```

### HTTP 413 Response

```
HTTP/1.1 413 Payload Too Large
Content-Type: text/plain

Payload Too Large - Message size exceeds maximum allowed
```

## Usage Examples

### 1. Basic Validation

```erlang
%% Validate default transport
case erlmcp_message_size:validate_message_size(default, Message) of
    ok -> handle_message(Message);
    {error, {message_too_large, ErrorResponse}} ->
        send_error(ErrorResponse)
end.

%% Validate HTTP body
case erlmcp_message_size:validate_http_body_size(HttpBody) of
    ok -> process_http_request(HttpBody);
    {error, _} -> respond_413()
end.
```

### 2. JSON-RPC Integration

```erlang
%% Decode with automatic size validation for HTTP transport
case erlmcp_json_rpc:decode_message(RequestData, http) of
    {ok, Message} -> handle_request(Message);
    {error, {message_too_large, ErrorMsg}} ->
        send_http_413(ErrorMsg);
    {error, _} -> send_parse_error()
end.
```

### 3. Custom Limits

```erlang
%% Validate with custom limit (e.g., 1MB for specific endpoint)
CustomLimit = 1024 * 1024,  % 1 MB
case erlmcp_message_size:validate_message_size(Message, CustomLimit, http) of
    ok -> process(Message);
    {error, _} -> reject()
end.
```

### 4. Getting Limits

```erlang
%% Get configured limit for transport
HttpLimit = erlmcp_message_size:get_limit(http),
WsLimit = erlmcp_message_size:get_limit(websocket),

%% Get full configuration
Config = erlmcp_message_size:get_size_limit_config(),
#{default := DefaultLimit, http_body := HttpBodyLimit} = Config.
```

## Configuration Guide

### Default Configuration

Default 16 MB limit per transport:

```erlang
{erlmcp, [
    %% message_size_limits not specified - uses defaults
]}
```

### Custom Limits

Reduce limits for constrained environments:

```erlang
{erlmcp, [
    {message_size_limits, #{
        default => 1048576,        % 1 MB
        http_body => 2097152,      % 2 MB
        sse_event => 524288,       % 512 KB
        websocket => 1048576,      % 1 MB
        tcp => 5242880,            % 5 MB
        stdio => 1048576           % 1 MB
    }}
]}
```

### Production Configuration

Increase limits for large payloads:

```erlang
{erlmcp, [
    {message_size_limits, #{
        default => 52428800,       % 50 MB
        http_body => 52428800,     % 50 MB
        sse_event => 52428800,     % 50 MB
        websocket => 52428800,     % 50 MB
        tcp => 52428800,           % 50 MB
        stdio => 52428800          % 50 MB
    }}
]}
```

## Test Coverage

Comprehensive test suite (`test/erlmcp_message_size_tests.erl`) with 20+ test cases:

### Basic Tests
- ✅ Default message size limit constant
- ✅ Message under limit accepted
- ✅ Message at limit accepted
- ✅ Message over limit rejected

### Transport-Specific Tests
- ✅ HTTP body size validation
- ✅ SSE event size validation
- ✅ WebSocket message size validation
- ✅ TCP message size validation
- ✅ Stdio message size validation

### Error Response Tests
- ✅ Error response format validation
- ✅ HTTP 413 error format
- ✅ Size formatting (KB, MB, GB)
- ✅ Error code in valid codes list

### Configuration Tests
- ✅ Get limits for all transport types
- ✅ Configuration validation
- ✅ Default fallback behavior
- ✅ Reasonable bounds validation

### JSON-RPC Integration Tests
- ✅ Decode message with size validation
- ✅ Error helper functions
- ✅ Custom message limits

### Edge Case Tests
- ✅ Zero-byte message
- ✅ Single byte message
- ✅ Exact limit boundary
- ✅ One byte over limit
- ✅ Very large message (20 MB)
- ✅ Configuration fallback

## Integration Points

### Transport Handlers

Should call validation before processing:

```erlang
%% In erlmcp_transport_sse.erl
handle_post_request(Data, State) ->
    case erlmcp_message_size:validate_http_body_size(Data) of
        ok -> process_request(Data, State);
        {error, {message_too_large, _}} ->
            cowboy_req:reply(413, #{}, <<"Payload Too Large">>, Req)
    end.
```

### HTTP Handlers

Integrate with HTTP transport layer:

```erlang
%% In HTTP request handler
validate_and_process(Body) ->
    case erlmcp_message_size:validate_http_body_size(Body) of
        ok -> erlmcp_json_rpc:decode_message(Body, http);
        {error, {message_too_large, ErrorMsg}} ->
            {error, {http_413, ErrorMsg}}
    end.
```

### WebSocket Handlers

Validate before processing frames:

```erlang
websocket_handle({text, Data}, State) ->
    case erlmcp_message_size:validate_websocket_size(Data) of
        ok -> process_message(Data, State);
        {error, _} ->
            {reply, {close, 1009, <<"Message too large">>}, State}
    end.
```

## Acceptance Criteria

- [x] Message size limits implemented
- [x] Configurable via sys.config
- [x] Default 16MB limit
- [x] Error for oversized messages
- [x] 10+ tests passing
- [x] JSON-RPC integration with decode_message/2
- [x] Transport-specific helpers
- [x] Human-readable error details
- [x] All edge cases handled

## Performance Impact

- **Memory**: Minimal - only stores size limits in memory
- **CPU**: O(1) - size check is a single comparison
- **Network**: Zero - validation before message processing

## Backward Compatibility

- Existing code using `decode_message/1` continues to work
- New `decode_message/2` is additive
- Configuration is optional - defaults are used if missing
- No breaking changes to existing API

## Security Benefits

1. **DoS Protection**: Prevents oversized message attacks
2. **Resource Protection**: Limits memory usage
3. **Clear Errors**: Clients receive explicit size limit information
4. **Configurable**: Operators can adjust for their needs

## Future Enhancements

1. **Per-client limits**: Different limits based on client info
2. **Streaming validation**: Check size during streaming
3. **Metrics**: Track oversized message attempts
4. **Alerts**: Log or alert on repeated oversized attempts
5. **Dynamic limits**: Change limits without restart

## Files Modified

1. `/Users/sac/erlmcp/include/erlmcp.hrl` - Added constants and error code
2. `/Users/sac/erlmcp/config/sys.config` - Added configuration section
3. `/Users/sac/erlmcp/src/erlmcp_json_rpc.erl` - Added decode_message/2 and error helpers
4. `/Users/sac/erlmcp/src/erlmcp_message_size.erl` - New module (created)
5. `/Users/sac/erlmcp/test/erlmcp_message_size_tests.erl` - Test suite (created)

## References

- MCP 2025-11-25 Specification
- RFC 7230 (HTTP/1.1 Message Semantics) - HTTP 413 status
- JSON-RPC 2.0 Specification (RFC 7159)
- erlmcp Architecture Documentation

## Author

Claude Code (claude.ai/code)
Generated: 2026-01-27
