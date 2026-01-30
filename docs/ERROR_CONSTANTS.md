# Error Constants in erlmcp.hrl

This document describes all error constants available in `include/erlmcp.hrl` for use in tests and application code.

## JSON-RPC 2.0 Standard Error Codes

```erlang
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).
```

**Convenience Macros (return {Code, Message} tuples):**
```erlang
-define(ERROR_PARSE, {?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR}).
-define(ERROR_INVALID_REQUEST, {?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST}).
-define(ERROR_METHOD_NOT_FOUND, {?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND}).
-define(ERROR_INVALID_PARAMS, {?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS}).
-define(ERROR_INTERNAL, {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}).
```

## MCP-Specific Error Codes (-32000 to -32104)

### Core MCP Errors (-32001 to -32010)
```erlang
-define(MCP_ERROR_RESOURCE_NOT_FOUND, -32001).
-define(MCP_ERROR_TOOL_NOT_FOUND, -32002).
-define(MCP_ERROR_PROMPT_NOT_FOUND, -32003).
-define(MCP_ERROR_CAPABILITY_NOT_SUPPORTED, -32004).
-define(MCP_ERROR_NOT_INITIALIZED, -32005).
-define(MCP_ERROR_SUBSCRIPTION_FAILED, -32006).
-define(MCP_ERROR_VALIDATION_FAILED, -32007).
-define(MCP_ERROR_TRANSPORT_ERROR, -32008).
-define(MCP_ERROR_TIMEOUT, -32009).
-define(MCP_ERROR_RATE_LIMITED, -32010).
```

### Content and Message Errors (-32011 to -32020)
```erlang
-define(MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG, -32011).
-define(MCP_ERROR_MESSAGE_TOO_LARGE, -32012).
-define(MCP_ERROR_INVALID_CONTENT_TYPE, -32013).
-define(MCP_ERROR_CONTENT_TOO_LARGE, -32014).
-define(MCP_ERROR_INVALID_ENCODING, -32015).
-define(MCP_ERROR_BINARY_DATA_TOO_LARGE, -32016).
-define(MCP_ERROR_TEXT_TOO_LONG, -32017).
-define(MCP_ERROR_INVALID_MIME_TYPE, -32018).
-define(MCP_ERROR_UNSUPPORTED_MEDIA_TYPE, -32019).
-define(MCP_ERROR_MEDIA_TYPE_NOT_ACCEPTABLE, -32020).
```

### Resource and Template Errors (-32021 to -32030)
```erlang
-define(MCP_ERROR_RESOURCE_TEMPLATE_NOT_FOUND, -32021).
-define(MCP_ERROR_INVALID_URI, -32022).
-define(MCP_ERROR_URI_SYNTAX_ERROR, -32023).
-define(MCP_ERROR_URI_TOO_LONG, -32024).
-define(MCP_ERROR_RESOURCE_ACCESS_DENIED, -32025).
-define(MCP_ERROR_RESOURCE_ALREADY_EXISTS, -32026).
-define(MCP_ERROR_RESOURCE_LOCKED, -32027).
-define(MCP_ERROR_RESOURCE_VERSION_MISMATCH, -32028).
-define(MCP_ERROR_TEMPLATE_RENDER_FAILED, -32029).
-define(MCP_ERROR_INVALID_URI_TEMPLATE, -32030).
```

### Tool and Execution Errors (-32031 to -32040)
```erlang
-define(MCP_ERROR_TOOL_EXECUTION_FAILED, -32031).
-define(MCP_ERROR_TOOL_TIMEOUT, -32032).
-define(MCP_ERROR_TOOL_CANCELLED, -32033).
-define(MCP_ERROR_INVALID_TOOL_ARGUMENTS, -32034).
-define(MCP_ERROR_TOOL_DISABLED, -32035).
-define(MCP_ERROR_TOOL_RESULT_TOO_LARGE, -32036).
-define(MCP_ERROR_TOOL_NOT_ALLOWED, -32037).
-define(MCP_ERROR_MAX_CONCURRENT_TOOLS, -32038).
-define(MCP_ERROR_TOOL_DEPENDENCY_FAILED, -32039).
-define(MCP_ERROR_TOOL_SCHEMA_INVALID, -32040).
```

### Prompt and Sampling Errors (-32041 to -32050)
```erlang
-define(MCP_ERROR_URL_ELICITATION_REQUIRED, -32042).
-define(MCP_ERROR_PROMPT_ARGUMENT_MISSING, -32043).
-define(MCP_ERROR_PROMPT_RENDER_FAILED, -32044).
-define(MCP_ERROR_INVALID_PROMPT_ARGUMENTS, -32045).
-define(MCP_ERROR_SAMPLING_FAILED, -32046).
-define(MCP_ERROR_SAMPLING_TIMEOUT, -32047).
-define(MCP_ERROR_INVALID_MODEL_PREFERENCES, -32048).
-define(MCP_ERROR_MODEL_NOT_AVAILABLE, -32049).
-define(MCP_ERROR_SAMPLING_RATE_LIMITED, -32050).
```

### Authentication and Authorization Errors (-32051 to -32060)
```erlang
-define(MCP_ERROR_AUTHENTICATION_FAILED, -32051).
-define(MCP_ERROR_AUTHORIZATION_FAILED, -32052).
-define(MCP_ERROR_INVALID_CREDENTIALS, -32053).
-define(MCP_ERROR_TOKEN_EXPIRED, -32054).
-define(MCP_ERROR_INSUFFICIENT_PERMISSIONS, -32055).
-define(MCP_ERROR_ACCESS_DENIED, -32056).
-define(MCP_ERROR_SESSION_EXPIRED, -32057).
-define(MCP_ERROR_SESSION_NOT_FOUND, -32058).
-define(MCP_ERROR_INVALID_TOKEN, -32059).
-define(MCP_ERROR_UNAUTHORIZED_OPERATION, -32060).
```

### Protocol and Negotiation Errors (-32061 to -32070)
```erlang
-define(MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, -32061).
-define(MCP_ERROR_PROTOCOL_VERSION_MISMATCH, -32062).
-define(MCP_ERROR_CAPABILITY_NEGOTIATION_FAILED, -32063).
-define(MCP_ERROR_INCOMPATIBLE_CAPABILITIES, -32064).
-define(MCP_ERROR_METHOD_NOT_SUPPORTED, -32065).
-define(MCP_ERROR_NOTIFICATION_NOT_SUPPORTED, -32066).
-define(MCP_ERROR_REQUEST_ID_INVALID, -32067).
-define(MCP_ERROR_REQUEST_ID_CONFLICT, -32068).
-define(MCP_ERROR_BATCH_REQUEST_TOO_LARGE, -32069).
-define(MCP_ERROR_BATCH_PARTIAL_FAILURE, -32070).
```

### Pagination and Cursor Errors (-32071 to -32080)
```erlang
-define(MCP_ERROR_INVALID_CURSOR, -32071).
-define(MCP_ERROR_CURSOR_EXPIRED, -32072).
-define(MCP_ERROR_PAGINATION_NOT_SUPPORTED, -32073).
-define(MCP_ERROR_PAGE_SIZE_TOO_LARGE, -32074).
-define(MCP_ERROR_PAGE_SIZE_INVALID, -32075).
-define(MCP_ERROR_INVALID_OFFSET, -32076).
-define(MCP_ERROR_CURSOR_REQUIRES_PARAMETER, -32077).
-define(MCP_ERROR_CURSOR_ENCODING_FAILED, -32078).
-define(MCP_ERROR_PAGINATION_LIMIT_EXCEEDED, -32079).
-define(MCP_ERROR_CURSOR_POSITION_INVALID, -32080).
```

### Task and Job Errors (-32081 to -32090)
```erlang
-define(MCP_ERROR_TASK_NOT_FOUND, -32081).
-define(MCP_ERROR_TASK_ALREADY_EXISTS, -32082).
-define(MCP_ERROR_TASK_FAILED, -32083).
-define(MCP_ERROR_TASK_CANCELLED, -32084).
-define(MCP_ERROR_TASK_TIMEOUT, -32085).
-define(MCP_ERROR_TASK_STATE_INVALID, -32086).
-define(MCP_ERROR_MAX_CONCURRENT_TASKS, -32087).
-define(MCP_ERROR_TASK_DEPENDENCY_FAILED, -32088).
-define(MCP_ERROR_TASK_RESULT_NOT_READY, -32089).
-define(MCP_ERROR_TASK_ALREADY_COMPLETED, -32090).
```

### Progress and Notification Errors (-32091 to -32099)
```erlang
-define(MCP_ERROR_INVALID_PROGRESS_TOKEN, -32091).
-define(MCP_ERROR_PROGRESS_TOKEN_EXPIRED, -32092).
-define(MCP_ERROR_PROGRESS_UPDATE_FAILED, -32093).
-define(MCP_ERROR_NOTIFICATION_FAILED, -32094).
-define(MCP_ERROR_NOTIFICATION_QUEUE_FULL, -32095).
-define(MCP_ERROR_INVALID_NOTIFICATION_TYPE, -32096).
-define(MCP_ERROR_NOTIFICATION_NOT_DELIVERED, -32097).
-define(MCP_ERROR_PROGRESS_VALUE_INVALID, -32098).
-define(MCP_ERROR_PROGRESS_TOTAL_INVALID, -32099).
-define(MCP_ERROR_CUSTOM_SERVER_ERROR, -32000).
```

## Security Error Codes (-32101 to -32104)

```erlang
-define(MCP_ERROR_PROMPT_TOO_LARGE, -32101).
-define(MCP_ERROR_DANGEROUS_PATTERN_DETECTED, -32102).
-define(MCP_ERROR_INVALID_VARIABLE_NAME, -32103).
-define(MCP_ERROR_PROMPT_RATE_LIMITED, -32104).
```

## JSON Schema Validation Error Atoms (Jesse-compatible)

These are used for pattern matching with jesse validation errors:

```erlang
% Error atoms
-define(JESSE_ERR_MISSING_REQUIRED_PROPERTY, missing_required_property).
-define(JESSE_ERR_WRONG_TYPE, wrong_type).
-define(JESSE_ERR_NOT_IN_ENUM, not_in_enum).
-define(JESSE_ERR_NOT_UNIQUE, not_unique).
-define(JESSE_ERR_WRONG_LENGTH, wrong_length).
-define(JESSE_ERR_WRONG_SIZE, wrong_size).
-define(JESSE_ERR_MISSING_DEPENDENCY, missing_dependency).
-define(JESSE_ERR_NO_MATCH, no_match).
-define(JESSE_ERR_NO_EXTRA_PROPERTIES, no_extra_properties).
-define(JESSE_ERR_NO_EXTRA_ITEMS, no_extra_items).
-define(JESSE_ERR_NOT_IN_RANGE, not_in_range).
-define(JESSE_ERR_TOO_MANY_PROPERTIES, too_many_properties).
-define(JESSE_ERR_TOO_FEW_PROPERTIES, too_few_properties).
-define(JESSE_ERR_ALL_SCHEMAS_NOT_VALID, all_schemas_not_valid).
-define(JESSE_ERR_NOT_MULTIPLE_OF, not_multiple_of).
-define(JESSE_ERR_NOT_ONE_SCHEMA_VALID, not_one_schema_valid).
-define(JESSE_ERR_MORE_THAN_ONE_SCHEMA_VALID, more_than_one_schema_valid).
-define(JESSE_ERR_DATA_INVALID, data_invalid).
-define(JESSE_ERR_SCHEMA_INVALID, schema_invalid).
-define(JESSE_ERR_DATA_ERROR, data_error).
-define(JESSE_ERR_SCHEMA_ERROR, schema_error).
-define(JESSE_ERR_PARSE_ERROR, parse_error).
```

## Common Error Reason Atoms

These are used for `{error, Reason}` tuple returns:

```erlang
% Generic errors
-define(ERR_NOT_FOUND, not_found).
-define(ERR_ALREADY_EXISTS, already_exists).
-define(ERR_TIMEOUT, timeout).
-define(ERR_INVALID_STATE, invalid_state).
-define(ERR_INVALID_ARGUMENT, invalid_argument).
-define(ERR_INVALID_REQUEST_ATOM, invalid_request).

% Circuit breaker errors
-define(ERR_CIRCUIT_BREAKER_OPEN, circuit_breaker_open).
-define(ERR_BREAKER_NOT_FOUND, breaker_not_found).
-define(ERR_BREAKER_ALREADY_EXISTS, breaker_already_exists).

% Rate limiter errors
-define(ERR_RATE_LIMITED, rate_limited).
-define(ERR_RATE_LIMITER_NOT_FOUND, rate_limiter_not_found).

% Cache errors
-define(ERR_CACHE_MISS, cache_miss).
-define(ERR_CACHE_DISABLED, cache_disabled).

% Batch errors
-define(ERR_BATCH_NOT_FOUND, batch_not_found).
-define(ERR_BATCH_FULL, batch_full).
-define(ERR_BATCH_CLOSED, batch_closed).

% Module/Process errors
-define(ERR_MODULE_NOT_LOADED, module_not_loaded).
-define(ERR_PROCESS_NOT_FOUND, process_not_found).
-define(ERR_PROCESS_DEAD, process_dead).

% Session errors
-define(ERR_SESSION_NOT_FOUND, session_not_found).
-define(ERR_SESSION_EXPIRED, session_expired).
-define(ERR_SESSION_CLOSED, session_closed).

% Transport errors
-define(ERR_TRANSPORT_NOT_FOUND, transport_not_found).
-define(ERR_TRANSPORT_CLOSED, transport_closed).
-define(ERR_TRANSPORT_ERROR_ATOM, transport_error).

% Resource/Tool/Prompt errors
-define(ERR_RESOURCE_NOT_FOUND_ATOM, resource_not_found).
-define(ERR_TOOL_NOT_FOUND_ATOM, tool_not_found).
-define(ERR_PROMPT_NOT_FOUND_ATOM, prompt_not_found).

% Validation errors
-define(ERR_VALIDATION_FAILED, validation_failed).
-define(ERR_SCHEMA_INVALID, schema_invalid).

% Authentication/Authorization errors
-define(ERR_AUTHENTICATION_FAILED, authentication_failed).
-define(ERR_AUTHORIZATION_FAILED, authorization_failed).
-define(ERR_ACCESS_DENIED, access_denied).
```

**Convenience Error Tuple Macros:**
```erlang
-define(ERR_NOT_FOUND_T, {error, ?ERR_NOT_FOUND}).
-define(ERR_ALREADY_EXISTS_T, {error, ?ERR_ALREADY_EXISTS}).
-define(ERR_TIMEOUT_T, {error, ?ERR_TIMEOUT}).
-define(ERR_INVALID_STATE_T, {error, ?ERR_INVALID_STATE}).
-define(ERR_INVALID_ARGUMENT_T, {error, ?ERR_INVALID_ARGUMENT}).
-define(ERR_CIRCUIT_BREAKER_OPEN_T, {error, ?ERR_CIRCUIT_BREAKER_OPEN}).
-define(ERR_RATE_LIMITED_T, {error, ?ERR_RATE_LIMITED}).
```

## Validation Error Record

```erlang
-record(jesse_validation_error, {
    path = [] :: list(),
    message :: binary(),
    expected :: term(),
    actual :: term()
}).
```

## Usage Examples

### Using JSON-RPC Error Codes in Tests

```erlang
% Check error code in response
?assertEqual(-32602, maps:get(<<"code">>, ErrorObject)).
?assertEqual(-32002, maps:get(<<"code">>, ErrorObject)).
```

### Using Error Reason Atoms

```erlang
% Return error from function
{error, ?ERR_NOT_FOUND}
{error, ?ERR_TIMEOUT}
{error, ?ERR_CIRCUIT_BREAKER_OPEN}

% Pattern match in tests
?assertEqual({error, not_found}, Result).
?assertEqual({error, timeout}, Result).
```

### Using Validation Error Atoms

```erlang
% Pattern match jesse errors
case jesse:validate(Schema, Data) of
    {ok, _} -> ok;
    {error, [?JESSE_MISSING_REQUIRED(<<"name">> | _]} -> {error, missing_name};
    {error, [?JESSE_WRONG_TYPE(Expected) | _]} -> {error, {wrong_type, Expected}}
end.
```

### Using Error Constants in Code

```erlang
% Build error response
Error = #mcp_error{
    code = ?MCP_ERROR_TOOL_NOT_FOUND,
    message = ?MCP_MSG_TOOL_NOT_FOUND,
    data = #{<<"tool">> => ToolName}
}
```

## Type Specification

The error_reason() type specification includes all common error atoms:

```erlang
-type error_reason() :: not_found | already_exists | timeout | invalid_state
                       | invalid_argument | invalid_request | circuit_breaker_open
                       | rate_limited | validation_failed | access_denied
                       | authentication_failed | authorization_failed
                       | transport_closed | process_dead
                       | breaker_not_found | batch_not_found
                       | session_not_found | session_expired
                       | resource_not_found | tool_not_found
                       | prompt_not_found | term().
```

This allows for precise type specifications in function returns:

```erlang
-spec my_function() -> {ok, term()} | {error, error_reason()}.
```
