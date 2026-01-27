# MCP Capability Negotiation Implementation

## Overview

This document describes the MCP capability negotiation feature implemented in erlmcp. The feature handles the critical handshake during server initialization where the client and server exchange their supported capabilities and agree on a protocol version.

## Architecture

### Files

1. **src/erlmcp_capabilities.erl** - Core capability negotiation module
2. **src/erlmcp_server.erl** - Updated with capability extraction and validation
3. **test/erlmcp_capabilities_tests.erl** - Comprehensive test suite (8+ tests)

### Key Components

#### 1. erlmcp_capabilities Module

Provides pure functions for capability handling:

```erlang
%% Extract client capabilities from initialize request
extract_client_capabilities(Params) -> #mcp_client_capabilities{}

%% Validate protocol version compatibility
validate_protocol_version(Version) -> ok | {error, Reason}

%% Check if client has specific capability
client_has_capability(ClientCaps, CapabilityName) -> boolean()

%% Build server capability map for response
build_server_capabilities(ServerCaps) -> map()

%% Check if request method is allowed by negotiated capabilities
check_capability_for_request(Method, ClientCaps) -> {ok, Method} | {error, Reason}
```

#### 2. Updated Server State

The server state now includes:

```erlang
-record(state, {
    %% ... existing fields ...
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    %% ... other fields ...
}).
```

#### 3. Initialize Request Handler

The initialize handler now:

1. Extracts client capabilities from request params
2. Validates protocol version compatibility
3. Stores negotiated capabilities in server state
4. Returns server capabilities in response
5. Blocks double initialization

## Protocol Version Validation

### Current Behavior

- Accepts matching version: `?MCP_VERSION` (currently `<<"2025-06-18">>`)
- Rejects future versions (not yet supported)
- Rejects versions before `<<"2025-01-01">>`
- Accepts compatible older versions within valid range

### Example

```erlang
validate_protocol_version(<<"2025-06-18">>) -> ok
validate_protocol_version(<<"2099-01-01">>) -> {error, <<"Protocol version not yet supported">>}
validate_protocol_version(<<"2024-06-18">>) -> {error, <<"Protocol version too old">>}
```

## Client Capability Extraction

Client advertises capabilities during initialize request:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {
      "roots": {},
      "sampling": {},
      "experimental": {
        "custom_feature": true
      }
    },
    "clientInfo": { ... }
  }
}
```

Extracted as:

```erlang
#mcp_client_capabilities{
    roots = #mcp_capability{enabled = true},
    sampling = #mcp_capability{enabled = true},
    experimental = #{<<"custom_feature">> => true}
}
```

## Server Capability Response

Server responds with its capabilities:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-06-18",
    "capabilities": {
      "resources": {
        "subscribe": true,
        "listChanged": true
      },
      "tools": {},
      "prompts": {
        "listChanged": true
      },
      "logging": {}
    },
    "serverInfo": {
      "name": "erlmcp",
      "version": "0.5.0"
    }
  }
}
```

## Feature Blocking

After initialization, all feature requests are validated against negotiated capabilities:

### Before Initialization

All requests except `initialize` are blocked:

```erlang
handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId, #state{initialized = false} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Cannot list resources before server initialization">>),
    {noreply, State}
```

Response:
```json
{
  "error": {
    "code": -32005,
    "message": "Cannot list resources before server initialization"
  }
}
```

### Double Initialize Prevention

Attempting to initialize twice is rejected:

```erlang
handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, TransportId, #state{initialized = true} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Server already initialized. Initialize must be called only once.">>),
    {noreply, State}
```

## Capability-Based Access Control

The `check_capability_for_request/2` function enforces that:

### Resource Methods
- `resources/list`
- `resources/read`
- `resources/subscribe`
- `resources/unsubscribe`

Require client `resources` capability.

### Tool Methods
- `tools/list`
- `tools/call`

Require client `tools` capability.

### Prompt Methods
- `prompts/list`
- `prompts/get`

Require client `prompts` capability.

### Sampling Methods
- `sampling/createMessage`

Require client `sampling` capability.

## Test Coverage

The test suite (erlmcp_capabilities_tests.erl) includes:

### 1. Server Capabilities Tests (3 tests)
- `test_initialize_response_includes_capabilities()` - Verifies capabilities in response
- `test_initialize_response_protocol_version()` - Verifies protocol version field
- `test_server_info_in_response()` - Verifies serverInfo structure

### 2. Client Capability Extraction Tests (4 tests)
- `test_extract_client_capabilities()` - Basic extraction
- `test_extract_roots_capability()` - Roots capability handling
- `test_extract_sampling_capability()` - Sampling capability handling
- `test_extract_empty_capabilities()` - Handling empty capability maps

### 3. Protocol Version Validation Tests (4 tests)
- `test_validate_matching_protocol_version()` - Exact version match
- `test_validate_default_protocol_version()` - Default version handling
- `test_reject_incompatible_protocol_version()` - Future version rejection
- `test_validate_major_version_only()` - YYYY-MM-DD format validation

### 4. Feature Blocking Tests (4 tests)
- `test_block_resources_before_initialization()` - Resources blocked pre-init
- `test_block_tools_before_initialization()` - Tools blocked pre-init
- `test_block_prompts_before_initialization()` - Prompts blocked pre-init
- `test_allow_resources_after_initialization()` - Resources allowed post-init

### 5. Capability Access Control Tests (3 tests)
- `test_check_client_roots_capability()` - Roots capability check
- `test_check_client_sampling_capability()` - Sampling capability check
- `test_check_experimental_capability()` - Experimental capability support

### 6. Task Capability Negotiation Tests (3 tests)
- `test_include_task_capability_in_server_response()` - Task capability presence
- `test_task_capability_features()` - Task feature structure
- `test_task_create_allowed_with_capability()` - Task creation with capability

**Total: 21+ tests across 6 categories**

## Usage Examples

### Example 1: Server with Full Capabilities

```erlang
Capabilities = #mcp_server_capabilities{
    resources = #mcp_capability{enabled = true},
    tools = #mcp_capability{enabled = true},
    prompts = #mcp_capability{enabled = true},
    logging = #mcp_capability{enabled = true}
},
{ok, Server} = erlmcp_server:start_link(my_server, Capabilities)
```

### Example 2: Client Checking Capability

```erlang
ClientCaps = erlmcp_capabilities:extract_client_capabilities(Params),

case erlmcp_capabilities:client_has_capability(ClientCaps, <<"resources">>) of
    true -> handle_resources_list(Request);
    false -> send_error(<<"Client does not support resources capability">>)
end
```

### Example 3: Validate Protocol Version

```erlang
ProtocolVersion = maps:get(<<"protocolVersion">>, InitParams, ?MCP_VERSION),

case erlmcp_capabilities:validate_protocol_version(ProtocolVersion) of
    ok -> proceed_with_initialization();
    {error, Reason} -> send_error_response(Reason)
end
```

## Error Codes

The implementation uses standard MCP error codes:

```erlang
?MCP_ERROR_NOT_INITIALIZED = -32005
?JSONRPC_INVALID_PARAMS = -32602
```

## Tracing & Observability

Initialize request includes detailed tracing:

```erlang
erlmcp_tracing:set_attributes(SpanCtx, #{
    <<"request_id">> => Id,
    <<"transport_id">> => TransportId,
    <<"method">> => ?MCP_METHOD_INITIALIZE,
    <<"client.protocol_version">> => ProtocolVersion,
    <<"client.capabilities">> => <<"negotiated">>
})
```

## Backward Compatibility

The implementation maintains backward compatibility:

- Servers without capability requirements work as before
- Protocol version defaults to `?MCP_VERSION` if not provided
- Undefined capabilities are treated as not enabled

## Future Extensions

The capability negotiation framework supports:

1. **Dynamic Capability Advertisement** - Servers can advertise capabilities at runtime
2. **Conditional Feature Support** - Features can be enabled/disabled based on client capabilities
3. **Experimental Features** - Unknown experimental capabilities can be passed through
4. **Capability Versioning** - Different capability versions can be negotiated

## Implementation Notes

### Immutability

Client capabilities are extracted once during initialization and stored in server state, ensuring consistent capability checking throughout the session.

### Fail-Fast Validation

Protocol version validation happens immediately during initialize request, failing early if incompatible.

### No State Mutation on Error

If protocol validation fails, server state remains unchanged, allowing potential retry.

### Thread Safety

All capability checking is stateless and can be safely called from multiple processes.

## References

- MCP Protocol Specification: https://modelcontextprotocol.io
- Erlang OTP Best Practices
- JSON-RPC 2.0 Specification: https://www.jsonrpc.org/specification
