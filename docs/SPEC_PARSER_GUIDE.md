# erlmcp Spec Parser Guide

## Overview

The `erlmcp_spec_parser` module is the core specification parser for the Model Context Protocol (MCP) version 2025-11-25. It provides hardcoded specification metadata for reliable, zero-dependency validation of MCP implementations.

**Key Features:**
- Hardcoded MCP 2025-11-25 specification (no external file parsing)
- gen_server-based with caching for performance
- Complete method, error code, transport, and capability definitions
- Validation functions for messages and method calls
- Query functions for all specification elements

## Architecture

### Hardcoded Spec Strategy

The spec parser uses hardcoded Erlang records and functions rather than parsing external specification files. This approach provides:

1. **Reliability**: No file I/O or parsing errors
2. **Performance**: Immediate access to spec data
3. **Type Safety**: Compile-time type checking
4. **Zero Dependencies**: No external parsers or file readers

### Data Flow

```
┌─────────────────────┐
│   Caller Request    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  gen_server Call    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│   Spec Builder      │
│  (build_mcp_spec)   │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Validation &       │
│  Result Return      │
└─────────────────────┘
```

## API Reference

### Starting and Stopping

```erlang
% Start with default options
{ok, Pid} = erlmcp_spec_parser:start_link().

% Start with custom options (cache_ttl in milliseconds)
{ok, Pid} = erlmcp_spec_parser:start_link(#{cache_ttl => 600000}).

% Stop the parser
ok = erlmcp_spec_parser:stop(Pid).
```

### Specification Queries

```erlang
% Get complete specification
{ok, #mcp_spec{}} = erlmcp_spec_parser:parse_specification().

% Get all method requirements
{ok, Methods} = erlmcp_spec_parser:get_method_requirements().

% Get specific method requirements
{ok, #method_req{}} = erlmcp_spec_parser:get_method_requirements(<<"initialize">>).

% Get all error codes
{ok, ErrorCodes} = erlmcp_spec_parser:get_error_requirements().

% Get specific error code
{ok, #error_code_req{}} = erlmcp_spec_parser:get_error_requirements(-32700).

% Get all transports
{ok, Transports} = erlmcp_spec_parser:get_transport_requirements().

% Get specific transport
{ok, #transport_req{}} = erlmcp_spec_parser:get_transport_requirements(<<"stdio">>).

% Get all capabilities
{ok, Capabilities} = erlmcp_spec_parser:get_capability_requirements().

% Get specific capability
{ok, #capability_req{}} = erlmcp_spec_parser:get_capability_requirements(<<"resources">>).
```

### Validation Functions

```erlang
% Validate a complete message
ok = erlmcp_spec_parser:validate_message(#{<<"jsonrpc">> => <<"2.0">>, ...}).

% Validate method call
ok = erlmcp_spec_parser:validate_method_call(<<"tools/list">>, #{}).

% Validate error code
ok = erlmcp_spec_parser:validate_error_code(-32700).

% Check capability support
ok = erlmcp_spec_parser:check_capability_support(<<"resources">>, server).
```

### Utility Functions

```erlang
% Get spec version
<<"2025-11-25">> = erlmcp_spec_parser:get_spec_version().

% Get all method names
{ok, [<<"initialize">>, <<"tools/list">>, ...]} = erlmcp_spec_parser:get_all_methods().

% Get all error codes
{ok, [-32700, -32600, ...]} = erlmcp_spec_parser:get_all_error_codes().

% Get all transports
{ok, [<<"stdio">>, <<"tcp">>, ...]} = erlmcp_spec_parser:get_all_transports().
```

## Data Structures

### #mcp_spec{}

```erlang
-record(mcp_spec, {
    version,              % <<"2025-11-25">>
    specification_date,  % <<"2025-11-25">>
    protocol_type,        % <<"JSON-RPC 2.0">>
    methods,              % #{binary() => #method_req{}}
    error_codes,          % #{integer() => #error_code_req{}}
    transports,           % [binary()]
    capabilities          % #{binary() => #capability_req{}}
}).
```

### #method_req{}

```erlang
-record(method_req, {
    name,                  % Method name (binary)
    method_type,           % request | notification
    direction,             % client_to_server | server_to_client | bidirectional
    required,              % boolean()
    params_spec,           % Parameter specification (map)
    result_spec,           % Result specification (map)
    capability_required,   % Required capability (binary | undefined)
    deprecation_status,    % stable | experimental | deprecated
    documentation          % Description (binary)
}).
```

### #error_code_req{}

```erlang
-record(error_code_req, {
    code,              % Integer error code
    name,              % Error name (binary)
    category,          % json_rpc | mcp_protocol | transport | validation
    description,       % Error description (binary)
    severity,          % error | warning
    retry_strategy     % retry | abort | fallback
}).
```

### #transport_req{}

```erlang
-record(transport_req, {
    name,                  % Transport name (binary)
    transport_type,        % stream_based | message_based
    framing,               % json_delimiter | length_prefixing | custom
    connection_oriented,   % boolean()
    multiplexing_support,  % boolean()
    required_features,     % [binary()]
    optional_features      % [binary()]
}).
```

### #capability_req{}

```erlang
-record(capability_req, {
    name,               % Capability name (binary)
    category,           % client | server
    required,           % boolean()
    dependencies,       % [binary()]
    features,           % [binary()]
    validation_rules    % [binary()]
}).
```

## Usage Examples

### Example 1: Validate a Method Call

```erlang
% Check if tools/call request is valid
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{
        <<"name">> => <<"my_tool">>,
        <<"arguments">> => #{}
    }
},

case erlmcp_spec_parser:validate_message(Request) of
    ok -> io:format("Valid request~n");
    {error, Reason} -> io:format("Invalid: ~p~n", [Reason])
end.
```

### Example 2: Get Method Requirements

```erlang
% Get requirements for resources/read
{ok, MethodReq} = erlmcp_spec_parser:get_method_requirements(<<"resources/read">>),

% Extract required parameters
ParamsSpec = MethodReq#method_req.params_spec,
Required = [K || {K, V} <- maps:to_list(ParamsSpec),
                maps:get(required, V, false) =:= true],

io:format("Required params: ~p~n", [Required]).
% Output: Required params: [<<"uri">>]
```

### Example 3: Validate Error Code

```erlang
% Check if error code is valid
case erlmcp_spec_parser:validate_error_code(-32008) of
    ok ->
        {ok, ErrorReq} = erlmcp_spec_parser:get_error_requirements(-32008),
        io:format("Error: ~s~n", [ErrorReq#error_code_req#name]);
    {error, Reason} ->
        io:format("Invalid error code: ~p~n", [Reason])
end.
% Output: Error: <<Resource not found>>
```

### Example 4: Generate Method Validator

```erlang
% Get validator for a specific method
{ok, Validator} = erlmcp_spec_parser:generate_method_validator(<<"initialize">>),

% Use validator to check parameters
Params = #{
    <<"protocolVersion">> => <<"2025-11-25">>,
    <<"capabilities">> => #{},
    <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
},

% Check required params (example)
Required = Validator#method_validator.required_params,
HasRequired = lists:all(fun(K) -> maps:is_key(K, Params) end, Required).
```

## Validation Rules

### JSON-RPC 2.0 Rules

1. **jsonrpc field**: Must be exactly `"2.0"`
2. **method field**: Required for requests/notifications
3. **id field**: Required for requests, optional for responses, prohibited for notifications
4. **params field**: Optional, must be object or array
5. **result field**: Mutually exclusive with error in responses
6. **error field**: Must have code, message; data is optional

### MCP Protocol Rules

1. **initialize first**: First request must be initialize
2. **protocol version**: Must negotiate `"2025-11-25"`
3. **capability exchange**: Capabilities must be declared in initialize
4. **initialized notification**: Must send after successful initialize
5. **reinitialize rejection**: Duplicate initialize requests rejected

### Method-Specific Rules

Each method has specific validation rules defined in its `#method_req{}`:

- **params_spec**: Required and optional parameters
- **result_spec**: Expected response structure
- **capability_required**: Required server/client capability
- **deprecation_status**: Stability level

## Integration with Validators

The spec parser integrates with all validators:

```erlang
% Protocol validator uses spec parser for method validation
{ok, Spec} = erlmcp_spec_parser:parse_specification(),
Methods = Spec#mcp_spec.methods,

% Transport validator uses spec parser for transport requirements
{ok, Transports} = erlmcp_spec_parser:get_transport_requirements(),

% Security validator uses spec parser for capability checks
ok = erlmcp_spec_parser:check_capability_support(<<"resources">>, server).
```

## Performance

### Caching

The spec parser implements caching with configurable TTL:

- **Default TTL**: 5 minutes (300,000ms)
- **Cache Key**: Parsed specification
- **Cache Invalidate**: Automatic expiration

```erlang
% Manually cache the spec
{ok, cached} = erlmcp_spec_parser:cache_parsed_spec().

% Get cached spec with age
{ok, Spec, AgeMs} = erlmcp_spec_parser:get_cached_spec().
```

### Performance Characteristics

- **Parse time**: < 1ms (hardcoded data)
- **Query time**: < 0.1ms (map lookup)
- **Memory**: ~100KB for full spec
- **Concurrency**: gen_server serializes requests (sufficient for read-heavy workload)

### Optimization Tips

1. **Batch queries**: Get all methods at once instead of individual lookups
2. **Cache locally**: Store frequently-used spec data in process state
3. **Warm cache**: Call `parse_specification()` on startup

## Complete Example

```erlang
-module(my_validator).
-export([validate_tools_call/1]).

validate_tools_call(Request) ->
    % Start spec parser if not running
    case whereis(erlmcp_spec_parser) of
        undefined -> {ok, _} = erlmcp_spec_parser:start_link();
        _ -> ok
    end,

    % Validate message structure
    case erlmcp_spec_parser:validate_message(Request) of
        {error, Reason} -> {error, invalid_message, Reason};
        ok ->
            % Extract method and params
            Method = maps:get(<<"method">>, Request),
            Params = maps:get(<<"params">>, Request, #{}),

            % Validate method call
            case erlmcp_spec_parser:validate_method_call(Method, Params) of
                {error, Reason} -> {error, invalid_params, Reason};
                ok -> {ok, valid}
            end
    end.
```

## See Also

- [Validator Guide](VALIDATOR_GUIDE.md)
- [Spec Compliance Testing](SPEC_COMPLIANCE_TESTING.md)
- [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/)
