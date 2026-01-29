# Capability Negotiation Implementation

## Overview

This document describes the implementation of proper capability negotiation for the erlmcp MCP SDK, following the MCP 2025-11-25 specification.

## Implementation Summary

### Files Created

1. **`apps/erlmcp_core/src/erlmcp_capabilities.erl`** - Capability negotiation module
   - Extract client capabilities from initialize request
   - Extract server capabilities from initialize response
   - Convert capabilities to/from maps for JSON encoding
   - Validate protocol versions
   - Negotiate capabilities between client and server
   - Helper functions for capability checking and error formatting

2. **`apps/erlmcp_core/test/erlmcp_capability_negotiation_tests.erl`** - Comprehensive test suite
   - 29 tests covering all capability negotiation scenarios
   - Tests for extraction, conversion, validation, and negotiation
   - Integration tests for full initialize handshake
   - Graceful degradation tests
   - Experimental capabilities tests

### Files Modified

1. **`apps/erlmcp_core/src/erlmcp_server.erl`**
   - Initialize handler uses `erlmcp_capabilities` module
   - Protocol version validation
   - Capability negotiation on handshake

2. **`apps/erlmcp_core/src/erlmcp_client.erl`**
   - Initialize sends client capabilities
   - Capability extraction and validation

3. **`apps/erlmcp_core/src/erlmcp_logging.erl`**
   - Fixed type definitions (added `log_entry()` type)

## Key Features

### 1. Capability Extraction

```erlang
%% Extract client capabilities from initialize request
ClientCaps = erlmcp_capabilities:extract_client_capabilities(Params).

%% Extract server capabilities from initialize response
ServerCaps = erlmcp_capabilities:extract_server_capabilities(Response).
```

### 2. Protocol Version Validation

```erlang
%% Validate protocol version
case erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>) of
    ok -> ok;
    {error, Reason} -> {error, Reason}
end.
```

Supported versions:
- `2024-11-05`
- `2025-11-25`

### 3. Capability Negotiation

```erlang
%% Negotiate capabilities between client and server
Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps).
```

### 4. Capability Checking

```erlang
%% Check if a capability is supported
erlmcp_capabilities:has_capability(ServerCaps, resources).  % true | false

%% Check if a capability feature is enabled
erlmcp_capabilities:has_capability_feature(ServerCaps, resources, subscribe).
```

### 5. Graceful Degradation

```erlang
%% Format capability error for client
Error = erlmcp_capabilities:format_capability_error({unsupported_capability, tools}).
%% => <<"Capability not supported: tools">>
```

## Test Results

All 29 tests pass:

- Extract client capabilities (full and minimal)
- Extract server capabilities (full and minimal)
- Convert capabilities to/from maps
- Validate protocol versions
- Negotiate capabilities
- Check capabilities and features
- Format capability errors
- Merge capabilities
- Full initialize handshake
- Graceful degradation
- Experimental capabilities

## Usage Examples

See the module documentation in `erlmcp_capabilities.erl` for detailed usage examples.

## References

- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [erlmcp OTP Patterns](./otp-patterns.md)
- [erlmcp API Reference](./api-reference.md)
