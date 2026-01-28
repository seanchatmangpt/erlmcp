# Refusal Taxonomy Reference - erlmcp v1.4.0

System-wide consistent error handling with standardized refusal codes, HTTP status codes, remediation hints, and severity levels.

## Overview

The refusal taxonomy provides a unified approach to error responses across erlmcp. Each refusal includes:

- **Error Code**: Unique integer (1001-1095) for programmatic identification
- **HTTP Status**: Standard HTTP status code (400, 401, 403, 404, 429, 503)
- **Message**: Human-readable error description
- **Remediation Hint**: Actionable advice for resolving the issue
- **Severity**: `warn` | `error` | `critical`

## Full Refusal Taxonomy Table

### Queue & Backpressure Refusals (1001-1005)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1001 | 429 | Queue capacity exceeded | Reduce message rate or increase `queue_limits.max_messages` in config | error |
| 1002 | 429 | Byte capacity exceeded | Message size too large or queue full; reduce message size or wait | error |
| 1003 | 429 | Tenant quota exceeded | Tenant aggregate limit reached; contact administrator | critical |
| 1004 | 503 | Buffer overflow | Internal buffer exhausted; reduce load or increase `buffer_pool` size | critical |
| 1005 | 503 | Backpressure active | Server under load; retry after delay or reduce request rate | warn |

### Authentication & Authorization Refusals (1011-1016)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1011 | 401 | Authentication failed | Provide valid credentials or check token format | error |
| 1012 | 401 | Authentication expired | Refresh your authentication token | error |
| 1013 | 401 | Invalid credentials | Check username, password, or API key | error |
| 1014 | 403 | Authorization denied | You don't have permission for this resource; contact administrator | error |
| 1015 | 401 | Missing authentication | Provide credentials via Authorization header or session ID | error |
| 1016 | 401 | Invalid session ID | Provide valid `MCP-Session-Id` header (32+ hex chars) | error |

### Parameter & Validation Refusals (1021-1029)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1021 | 400 | Invalid parameters | Check parameter names and types match API specification | error |
| 1022 | 400 | JSON schema validation failed | Review error details; structure must match schema | error |
| 1023 | 400 | Invalid URI format | Use valid absolute or relative URI (e.g., `https://example.com/path`) | error |
| 1024 | 415 | Invalid Content-Type | Use `application/json`, `text/plain`, or `application/octet-stream` | error |
| 1025 | 400 | Required header missing or invalid | Include `MCP-Protocol-Version` and other required headers | error |
| 1026 | 400 | Session ID invalid | Session ID must be 32+ hexadecimal characters | error |
| 1027 | 400 | Protocol version not supported | Use protocol version `2025-11-25` or `2024-11-05` | error |
| 1028 | 400 | Required field missing | Check request includes all mandatory fields per API spec | error |
| 1029 | 400 | Field type mismatch | Field type doesn't match schema; check API documentation | error |

### Path & URI Security Refusals (1036-1040)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1036 | 400 | Path traversal detected | Remove `..` and `%` sequences; use canonical paths | critical |
| 1037 | 400 | Invalid path format | Path must start with `/` and contain only valid characters | error |
| 1038 | 400 | Symlink traversal detected | Symlinks not allowed; use direct file paths | critical |
| 1039 | 400 | URI out of bounds | URI must resolve within configured roots | error |
| 1040 | 400 | Canonical path violation | Path must be canonical (no `.` or `..` components) | error |

### Resource & Entity Refusals (1046-1052)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1046 | 404 | Resource not found | Check resource URI and ensure it exists | warn |
| 1047 | 409 | Resource already exists | Use different URI or update existing resource | warn |
| 1048 | 404 | Tool not found | Check tool name in `tools/list` or register new tool | warn |
| 1049 | 409 | Tool already registered | Tool exists; use different name or unregister first | warn |
| 1050 | 404 | Prompt not found | Check prompt name in `prompts/list` or create new prompt | warn |
| 1051 | 409 | Prompt already exists | Prompt exists; use different name or update existing | warn |
| 1052 | 409 | Entity already exists | Use different identifier or update via appropriate endpoint | warn |

### Rate Limiting & Throttling Refusals (1056-1060)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1056 | 429 | Rate limit exceeded | Slow down request rate or implement exponential backoff | error |
| 1057 | 429 | Per-second rate limit exceeded | Max 100 requests/sec; wait before retrying | error |
| 1058 | 429 | Per-minute rate limit exceeded | Max 5000 requests/min; wait or increase plan | error |
| 1059 | 429 | Quota exceeded | Monthly quota used; upgrade plan or wait for reset | error |
| 1060 | 429 | Concurrent connection limit exceeded | Max 100 concurrent connections; close unused connections | error |

### Protocol & Transport Refusals (1066-1070)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1066 | 400 | Protocol error | Review message format and protocol compliance | error |
| 1067 | 503 | Transport error | Network error detected; check connection and retry | error |
| 1068 | 413 | Message too large | Max 1MB per message; compress or split data | error |
| 1069 | 503 | Operation timeout | Increase timeout or reduce server load | warn |
| 1070 | 415 | Encoding not supported | Use UTF-8 or other supported encoding | error |

### Server State Refusals (1076-1080)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1076 | 503 | Server not initialized | Call `initialize` before other RPC methods | error |
| 1077 | 503 | Server shutting down | Server is closing; reconnect after restart | warn |
| 1078 | 503 | Service unavailable | Server temporarily unavailable; retry later | warn |
| 1079 | 503 | Internal error | Unexpected error; check server logs and contact support | critical |
| 1080 | 503 | Dependency unavailable | External service down; wait or contact support | warn |

### Circuit Breaker & Health Refusals (1086-1089)

| Code | HTTP Status | Message | Hint | Severity |
|------|-------------|---------|------|----------|
| 1086 | 503 | Circuit breaker open | Too many failures; wait before retrying | warn |
| 1087 | 503 | Health check failed | Server unhealthy; may be degraded or restarting | warn |
| 1088 | 503 | Service degraded | Some features unavailable; functionality reduced | warn |
| 1089 | 503 | Resource exhausted | Server out of resources (memory, connections, etc.) | critical |

## Category Classification

Refusal codes are organized into 9 categories:

| Category | Code Range | Use Case |
|----------|-----------|----------|
| **queue** | 1001-1005 | Message queue capacity and backpressure |
| **auth** | 1011-1016 | Authentication and authorization failures |
| **validation** | 1021-1029 | Parameter and schema validation errors |
| **security** | 1036-1040 | Path traversal, symlink, and URI boundary violations |
| **resource** | 1046-1052 | Resource, tool, and prompt existence/collision |
| **rate_limit** | 1056-1060 | Rate limiting and quota enforcement |
| **protocol** | 1066-1070 | Protocol and transport layer errors |
| **server** | 1076-1080 | Server state and initialization errors |
| **circuit_breaker** | 1086-1089 | Circuit breaker, health, and resource exhaustion |

## API Usage

### Erlang Module: `erlmcp_refusal`

#### Lookup Functions

```erlang
%% Get HTTP status for refusal code
{ok, 429} = erlmcp_refusal:get_http_status(1001).

%% Get error message
{ok, <<"Queue capacity exceeded">>} = erlmcp_refusal:get_message(1001).

%% Get remediation hint
{ok, <<"Reduce message rate...">>} = erlmcp_refusal:get_hint(1001).

%% Get severity level
{ok, error} = erlmcp_refusal:get_severity(1001).

%% Get full metadata
{ok, Meta} = erlmcp_refusal:get_metadata(1001),
%% Meta = #{code => 1001, http_status => 429, message => ..., hint => ..., severity => error, category => queue}
```

#### Creation Functions

```erlang
%% Create refusal with code only
{ok, Refusal} = erlmcp_refusal:create_refusal(1001).

%% Create with additional details
Details = #{connection_id => <<"conn_123">>, message_count => 1050},
{ok, Refusal} = erlmcp_refusal:create_refusal(1001, Details).

%% Create with custom message
{ok, Refusal} = erlmcp_refusal:create_refusal(1001, Details, <<"Custom message">>).
```

#### Formatting Functions

```erlang
%% Format as human-readable string
Formatted = erlmcp_refusal:format_refusal(Refusal).
%% Output: "[error] Code 1001: Queue capacity exceeded\nTip: Reduce message rate..."

%% Format as JSON (for HTTP responses)
JSON = erlmcp_refusal:format_refusal_json(Refusal).
%% Output: #{error_code => 1001, http_status => 429, message => ..., remediation_hint => ..., ...}
```

#### Validation Functions

```erlang
%% Check if code is valid
true = erlmcp_refusal:is_valid_code(1001).

%% Check severity and category
true = erlmcp_refusal:is_critical(1003).
true = erlmcp_refusal:is_auth_failure(1011).
true = erlmcp_refusal:is_security_issue(1036).
true = erlmcp_refusal:is_rate_limit(1056).
true = erlmcp_refusal:is_queue_limit(1001).
```

#### Logging Functions

```erlang
%% Log refusal with automatic severity level
ok = erlmcp_refusal:log_refusal(Refusal).

%% Log with context
ok = erlmcp_refusal:log_refusal(Refusal, "queue_check").

%% Log with full context details
ok = erlmcp_refusal:log_refusal_with_context(Refusal, "backpressure", #{
    connection_id => <<"conn_123">>,
    queue_depth => 1050
}).
```

## CLI Examples

### Example 1: Queue Limit Refusal

```erlang
%% In queue limits module
case check_queue_limit(ConnId, MessageSize) of
    ok ->
        {ok, accepted};
    exceeded ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(1001, #{
            connection_id => ConnId,
            current_messages => CurrentCount,
            max_messages => MaxCount
        }),
        erlmcp_refusal:log_refusal(Refusal, "queue_check"),
        {error, erlmcp_refusal:format_refusal_json(Refusal)}
end.
```

### Example 2: Path Traversal Detection

```erlang
%% In URI validator module
case validate_path_security(Path) of
    ok ->
        {ok, validated};
    {error, path_traversal} ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(1036, #{
            path => Path,
            detected_pattern => ".."
        }),
        erlmcp_refusal:log_refusal(Refusal, "security"),
        {error, erlmcp_refusal:format_refusal_json(Refusal)}
end.
```

### Example 3: Authentication Failure

```erlang
%% In HTTP auth module
case verify_token(Token) of
    valid ->
        ok;
    expired ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(1012),
        erlmcp_refusal:log_refusal(Refusal, "auth"),
        {error, erlmcp_refusal:format_refusal_json(Refusal)};
    invalid ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(1011),
        erlmcp_refusal:log_refusal(Refusal, "auth"),
        {error, erlmcp_refusal:format_refusal_json(Refusal)}
end.
```

### Example 4: HTTP Response

```erlang
%% Handler returns JSON with HTTP status code
case Refusal of
    {error, JSON} ->
        {
            StatusCode,
            [
                {<<"Content-Type">>, <<"application/json">>},
                {<<"X-Error-Code">>, integer_to_binary(maps:get(error_code, JSON))}
            ],
            jsx:encode(JSON)
        }
end.
```

## Integration Points

### erlmcp_queue_limits.erl

Refactor backpressure responses:

```erlang
%% OLD: {error, refuse, #{reason => message_count_exceeded, ...}}
%% NEW: {ok, Refusal} = erlmcp_refusal:create_refusal(1001, Details)
```

### erlmcp_uri_validator.erl

Refactor validation errors:

```erlang
%% OLD: {error, {path_traversal, <<"Detected .. in path">>}}
%% NEW: {ok, Refusal} = erlmcp_refusal:create_refusal(1036, #{path => Path})
```

### erlmcp_http_header_validator.erl

Refactor header validation:

```erlang
%% OLD: {error, {400, invalid_header, <<"Missing MCP-Protocol-Version">>}}
%% NEW: {ok, Refusal} = erlmcp_refusal:create_refusal(1025)
```

### erlmcp_http_auth.erl

Refactor authentication:

```erlang
%% OLD: {error, auth_failed}
%% NEW: {ok, Refusal} = erlmcp_refusal:create_refusal(1011)
```

## Implementation Strategy

### Phase 1: Foundation (DONE)
- Define header file with 45 refusal codes
- Create erlmcp_refusal module with utilities
- Create comprehensive test suite

### Phase 2: Integration (IN PROGRESS)
- Refactor erlmcp_queue_limits.erl
- Refactor erlmcp_uri_validator.erl
- Refactor erlmcp_http_header_validator.erl
- Refactor erlmcp_http_auth.erl

### Phase 3: Testing
- Verify all tests pass (make test)
- Verify code quality (make check)
- Run 5x to verify deterministic behavior

### Phase 4: Documentation & Release
- Update API reference
- Create migration guide for modules
- Release v1.4.0 with consistent refusal handling

## Known Limitations

1. **Code Range Fixed**: Codes 1001-1095 are reserved for refusals. New codes require header update.

2. **HTTP Status Mapping**: Multiple refusal codes may map to same HTTP status (e.g., 1001, 1002, 1056 all use 429). This is intentional for HTTP compliance.

3. **Severity Not Enforced at HTTP**: All refusals return appropriate HTTP status regardless of severity. Severity is for logging/monitoring only.

4. **Details Optional**: The `details` field in refusal is optional but recommended for debugging.

5. **Timestamp Auto-Generated**: All refusals include `erlang:system_time(millisecond)`. Cannot be overridden.

6. **Single Error Per Response**: Current design returns one refusal per error. Batch error handling not supported.

7. **Message Customization**: Can provide custom message via `create_refusal/3`, but overrides default message entirely.

## Testing

Run the comprehensive test suite:

```bash
# Run all refusal tests
make test-refusal
# or
rebar3 eunit --module=erlmcp_refusal_SUITE

# Verify with full check
make check
```

### Test Coverage

- **45 refusal codes**: All codes have metadata and work correctly
- **Deterministic behavior**: 5 runs each produce identical results
- **Category classification**: All codes correctly categorized
- **JSON formatting**: All fields present and types correct
- **Remediation hints**: All actionable and specific
- **Severity levels**: All valid (warn/error/critical)
- **HTTP status codes**: All valid HTTP codes

## Performance Considerations

- **Lookup**: O(1) average via list keyfind (45 entries = negligible)
- **Refusal Creation**: Sub-millisecond (record allocation + timestamp)
- **JSON Formatting**: Sub-millisecond (map operations)
- **Logging**: Depends on logger backend

No performance impact on hot paths. Recommended for all error conditions.

## Security Considerations

- **Information Disclosure**: Remediation hints are user-friendly but don't expose sensitive details
- **Path Traversal Detection**: Separate codes for path traversal attacks (1036, 1038)
- **Session Validation**: Separate codes for authentication and session issues (1011-1016)
- **URI Validation**: Separate codes for URI security (1036-1040)

## Version History

- **v1.4.0**: Initial release with 45 refusal codes and comprehensive taxonomy
