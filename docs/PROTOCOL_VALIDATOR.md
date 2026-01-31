# Protocol Validator - JSON-RPC 2.0 and MCP Protocol Validation

## Purpose and Scope

The `erlmcp_protocol_validator` module provides comprehensive validation of JSON-RPC 2.0 and MCP (Model Context Protocol) messages against the official MCP 2025-11-25 specification. It ensures:

- JSON-RPC 2.0 structural compliance
- Valid MCP method and notification names
- Correct error code ranges
- Proper field types and required fields
- Message format validation

## Architecture

```
erlmcp_protocol_validator
├── Message Type Detection          - Request/Response/Notification/Error
├── JSON-RPC Validation            - Version field, ID validation
├── MCP Method Validation          - Method name checking
├── MCP Notification Validation    - Notification name checking
├── Error Code Validation          - Range and category validation
├── Field Type Validation          - Type checking for fields
└── Required Fields Validation     - Presence checking
```

## API Reference

### Core Validation Functions

#### validate_jsonrpc/1

Validate JSON-RPC 2.0 message structure.

```erlang
-spec validate_jsonrpc(map()) -> validation_result().
```

**Parameters:**
- `Message` - A map representing a JSON-RPC message

**Returns:**
- `ok` - Message is valid
- `{error, validation_error()}` - Message is invalid with details

**Checks performed:**
1. Message is a map
2. `jsonrpc` field exists and equals `"2.0"`
3. Message type detection (request/response/notification)
4. Type-specific validation

**Example:**
```erlang
% Valid request
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"ping">>,
    <<"id">> => 1,
    <<"params">> => #{}
},
Result = erlmcp_protocol_validator:validate_jsonrpc(Request),
% Returns: ok

% Missing jsonrpc field
Invalid = #{<<"method">> => <<"ping">>, <<"id">> => 1},
Result = erlmcp_protocol_validator:validate_jsonrpc(Invalid),
% Returns: {error, #{reason => invalid_jsonrpc_version, details => ...}}
```

#### validate_request/2

Validate MCP request structure.

```erlang
-spec validate_request(binary(), json_rpc_params()) -> validation_result().
```

**Parameters:**
- `Method` - Binary method name (e.g., `<<"tools/list">>`)
- `Params` - Parameters (map, array, or undefined)

**Returns:**
- `ok` - Request is valid
- `{error, validation_error()}` - Request is invalid

**Example:**
```erlang
% Valid tool list request
Result = erlmcp_protocol_validator:validate_request(
    <<"tools/list">>,
    #{<<"cursor">> => <<"next-page">>}
),
% Returns: ok

% Unknown method
Result = erlmcp_protocol_validator:validate_request(
    <<"unknown/method">>,
    #{}
),
% Returns: {error, #{reason => unknown_method, details => ...}}
```

#### validate_response/2

Validate MCP response structure.

```erlang
-spec validate_response(json_rpc_id(), term()) -> validation_result().
```

**Parameters:**
- `Id` - Request ID (null, binary, or integer)
- `ResultOrError` - Result map or error object

**Returns:**
- `ok` - Response is valid
- `{error, validation_error()}` - Response is invalid

**Example:**
```erlang
% Valid response with result
Result = erlmcp_protocol_validator:validate_response(
    1,
    #{<<"tools">> => []}
),
% Returns: ok

% Valid response with error
Result = erlmcp_protocol_validator:validate_response(
    1,
    #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}
),
% Returns: ok
```

#### validate_notification/2

Validate MCP notification structure.

```erlang
-spec validate_notification(binary(), json_rpc_params()) -> validation_result().
```

**Parameters:**
- `NotificationName` - Binary notification name
- `Params` - Notification parameters

**Returns:**
- `ok` - Notification is valid
- `{error, validation_error()}` - Notification is invalid

**Example:**
```erlang
% Valid initialized notification
Result = erlmcp_protocol_validator:validate_notification(
    <<"notifications/initialized">>,
    #{<<"protocolVersion">> => <<"2025-11-25">>}
),
% Returns: ok
```

#### validate_error_code/1

Validate error code is in valid range.

```erlang
-spec validate_error_code(integer()) -> validation_result().
```

**Valid Ranges:**
- JSON-RPC 2.0 standard: `-32700` to `-32600`
- JSON-RPC server errors: `-32099` to `-32000`
- MCP errors: `-32113` to `-32000`
- MCP refusal codes: `1001` to `1089`

**Example:**
```erlang
% Valid JSON-RPC error
ok = erlmcp_protocol_validator:validate_error_code(-32700).

% Valid MCP refusal code
ok = erlmcp_protocol_validator:validate_error_code(1001).

% Invalid error code
{error, _} = erlmcp_protocol_validator:validate_error_code(9999).
```

### Field Validation Functions

#### validate_method_name/1

Validate MCP method name.

```erlang
-spec validate_method_name(binary()) -> validation_result().
```

**Valid Methods:**
```erlang
% Core Protocol
<<"initialize">>, <<"ping">>

% Resources
<<"resources/list">>, <<"resources/read">>, <<"resources/templates/list">>
<<"resources/subscribe">>, <<"resources/unsubscribe">>

% Tools
<<"tools/list">>, <<"tools/call">>

% Tasks
<<"tasks/create">>, <<"tasks/list">>, <<"tasks/get">>
<<"tasks/result">>, <<"tasks/cancel">>

% Prompts
<<"prompts/list">>, <<"prompts/get">>

% Completion
<<"completion/complete">>

% Cancellation
<<"requests/cancel">>

% Logging
<<"logging/set_level">>

% Roots
<<"roots/list">>

% Sampling
<<"sampling/create_message">>
```

#### validate_notification_name/1

Validate MCP notification name.

```erlang
-spec validate_notification_name(binary()) -> validation_result().
```

**Valid Notifications:**
```erlang
% Core
<<"notifications/initialized">>

% Progress
<<"notifications/progress">>

% Resources
<<"notifications/resourcesupdated">>
<<"notifications/resources/list_changed">>

% Tools
<<"notifications/tools/list_changed">>

% Prompts
<<"notifications/prompts/list_changed">>

% Roots
<<"notifications/roots/list_changed">>

% Tasks
<<"notifications/tasks/status">>

% Cancellation
<<"notifications/cancelled">>

% Message
<<"notifications/message">>
```

#### validate_field_type/3

Validate field value matches expected type.

```erlang
-spec validate_field_type(binary(), term(), field_type() | [field_type()]) -> validation_result().
```

**Field Types:**
- `binary` - Binary string
- `integer` - Integer number
- `number` - Integer or float
- `boolean` - true or false
- `map` - Map/object
- `array` - List
- `any` - Any type

**Example:**
```erlang
% Single type check
ok = erlmcp_protocol_validator:validate_field_type(
    <<"id">>,
    123,
    integer
).

% Multiple type check
ok = erlmcp_protocol_validator:validate_field_type(
    <<"id">>,
    <<"abc">>,
    [binary, integer]
).

% Type mismatch
{error, #{reason := type_mismatch}} =
    erlmcp_protocol_validator:validate_field_type(
        <<"count">>,
        <<"not a number">>,
        integer
    ).
```

#### validate_required_fields/2

Validate required fields are present in message.

```erlang
-spec validate_required_fields(map(), [binary()]) -> validation_result().
```

**Example:**
```erlang
Message = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"ping">>,
    <<"id">> => 1
},

% Check required fields
ok = erlmcp_protocol_validator:validate_required_fields(
    Message,
    [<<"jsonrpc">>, <<"method">>]
).

% Missing field
{error, #{reason := missing_required_fields}} =
    erlmcp_protocol_validator:validate_required_fields(
        Message,
        [<<"jsonrpc">>, <<"method">>, <<"params">>]
    ).
```

### Utility Functions

#### format_validation_error/1

Format validation error for logging/response.

```erlang
-spec format_validation_error(validation_error()) -> binary().
```

**Example:**
```erlang
Error = #{reason => invalid_method, details => #{method => <<"bad">>}},
Formatted = erlmcp_protocol_validator:format_validation_error(Error),
% Returns: <<"invalid_method: method=bad">>
```

## Usage Examples

### Validating Incoming Messages

```erlang
%% Handle incoming message
handle_incoming(Message) ->
    case erlmcp_protocol_validator:validate_jsonrpc(Message) of
        ok ->
            case maps:get(<<"method">>, Message, undefined) of
                undefined ->
                    handle_response(Message);
                Method ->
                    handle_request(Method, Message)
            end;
        {error, Reason} ->
            logger:error("Invalid message: ~p", [Reason]),
            {error, invalid_message}
    end.

handle_request(Method, Message) ->
    Params = maps:get(<<"params">>, Message, #{}),
    case erlmcp_protocol_validator:validate_request(Method, Params) of
        ok ->
            process_request(Method, Params);
        {error, Reason} ->
            {error, Reason}
    end.
```

### Creating Valid Responses

```erlang
%% Create successful response
create_success_response(RequestId, Result) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => Result
    },
    case erlmcp_protocol_validator:validate_response(RequestId, Result) of
        ok -> {ok, Response};
        {error, _} -> {error, invalid_response}
    end.

%% Create error response
create_error_response(RequestId, Code, Message) ->
    ok = erlmcp_protocol_validator:validate_error_code(Code),
    Error = #{
        <<"code">> => Code,
        <<"message">> => Message
    },
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"error">> => Error
    },
    {ok, Response}.
```

### Validating Tool Calls

```erlang
%% Validate tool call request
validate_tool_call(ToolName, Arguments) ->
    % Check method name
    case erlmcp_protocol_validator:validate_method_name(<<"tools/call">>) of
        ok ->
            % Check tool name is binary
            case is_binary(ToolName) andalso byte_size(ToolName) > 0 of
                true ->
                    % Check arguments is a map
                    case is_map(Arguments) of
                        true -> ok;
                        false -> {error, invalid_arguments}
                    end;
                false -> {error, invalid_tool_name}
            end;
        {error, Reason} -> {error, Reason}
    end.
```

## Testing Guidance

### Unit Tests

```erlang
%% Test valid JSON-RPC request
validate_jsonrpc_valid_request_test() ->
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"ping">>,
        <<"id">> => 1
    },
    ?assertEqual(ok, erlmcp_protocol_validator:validate_jsonrpc(Message)).

%% Test missing jsonrpc field
validate_jsonrpc_missing_version_test() ->
    Message = #{
        <<"method">> => <<"ping">>,
        <<"id">> => 1
    },
    {error, Reason} = erlmcp_protocol_validator:validate_jsonrpc(Message),
    ?assertEqual(invalid_jsonrpc_version, maps:get(reason, Reason)).

%% Test unknown method
validate_unknown_method_test() ->
    {error, Reason} = erlmcp_protocol_validator:validate_request(
        <<"unknown/method">>,
        #{}
    ),
    ?assertEqual(unknown_method, maps:get(reason, Reason)).

%% Test invalid error code
validate_invalid_error_code_test() ->
    {error, Reason} = erlmcp_protocol_validator:validate_error_code(99999),
    ?assertEqual(invalid_error_code, maps:get(reason, Reason)).
```

### Property-Based Tests

```erlang
%% Property: Valid error codes are in valid range
prop_valid_error_codes() ->
    ?FORALL(Code, oneof([-32700, -32600, -32001, 1001, 1089]),
        begin
            ok = erlmcp_protocol_validator:validate_error_code(Code),
            true
        end).

%% Property: ID validation accepts valid types
prop_valid_id_types() ->
    ?FORALL(Id, oneof([null, 1, 12345, <<"abc-123">>]),
        begin
            ok = erlmcp_protocol_validator:validate_response(Id, #{}),
            true
        end).
```

## Troubleshooting

### Common Validation Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `invalid_jsonrpc_version` | Missing or wrong jsonrpc field | Add `"jsonrpc": "2.0"` |
| `missing_method` | No method field in request | Add method field |
| `unknown_method` | Method not in MCP spec | Check method name spelling |
| `unknown_notification` | Notification not in spec | Check notification name |
| `invalid_error_code` | Error code out of range | Use valid error code range |
| `type_mismatch` | Field has wrong type | Fix field type |
| `missing_required_fields` | Required fields absent | Add missing fields |

### Debug Mode

Enable detailed logging:

```erlang
logger:set_application_level(erlmcp_validation, debug).
```

### Validation Flow Diagram

```
Incoming Message
       |
       v
validate_jsonrpc/1
       |
       +--> Is map?
       |    |
       |    +--> No --> {error, not_map}
       |    |
       |    +--> Yes --> Check jsonrpc field
       |                  |
       |                  +--> Missing/wrong --> {error, invalid_jsonrpc_version}
       |                  |
       |                  +--> Present/correct --> Detect message type
       |                                        |
       |    +-----------------------------------+--> Has ID?
       |                                        |  |
       |                                        |  +--> No (Notification)
       |                                        |  |    |
       |                                        |  |    v
       |                                        |  | validate_notification/2
       |                                        |  |
       |                                        |  +--> Has Method?
       |                                        |       |
       |                                        |       +--> Yes (Request)
       |                                        |       |    |
       |                                        |       |    v
       |                                        |       | validate_request/2
       |                                        |       |
       |                                        |       +--> No (Response)
       |                                        |            |
       |                                        |            v
       |                                        |      validate_response/2
```

## Related Documentation

- [MCP_SPEC_VALIDATION.md](MCP_SPEC_VALIDATION.md) - Validation overview
- [erlmcp_spec_parser](../apps/erlmcp_validation/src/erlmcp_spec_parser.erl) - Spec metadata
- [MCP 2025-11-25 Spec](https://spec.modelcontextprotocol.io/)
