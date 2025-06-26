# Everything Server Example

This directory contains an Erlang implementation of the "everything server" example from the Model Context Protocol (MCP) servers repository, converted to use the erlmcp SDK.

## Overview

The everything server demonstrates a comprehensive MCP server implementation that showcases all major MCP capabilities:

- **8 Tools** for various operations (echo, math, environment, images, etc.)
- **3 Prompt Types** with different complexity levels and content types
- **100 Test Resources** with alternating mime types for testing resource handling
- **Advanced Features** including subscriptions, logging, and notifications

## Architecture

This implementation follows proper Erlang/OTP patterns:

- **gen_server behavior** for robust state management and supervision
- **erlmcp SDK integration** using `#mcp_server_capabilities{}` records
- **Proper resource management** with maps-based storage and handler functions
- **Schema validation** for all tools with JSON schema definitions
- **Memory management** with proper timer cleanup in terminate callbacks

## Features

### Tools Implemented

1. **echo** - Echoes back a message
2. **add** - Adds two numbers together  
3. **longRunningOperation** - Simulates long-running operations with progress steps
4. **printEnv** - Returns environment variables as JSON
5. **sampleLLM** - Mock LLM sampling (requires client support for actual implementation)
6. **getTinyImage** - Returns a tiny PNG image with multiple content types
7. **annotatedMessage** - Demonstrates message annotations with different priorities and audiences
8. **getResourceReference** - Returns references to test resources

### Prompt Types

1. **simple_prompt** - Basic prompt without arguments
2. **complex_prompt** - Prompt with temperature and style arguments, includes image content  
3. **resource_prompt** - Prompt that includes resource references

### Resources

- **100 static resources** numbered 1-100
- **Alternating mime types** (text/plain and application/octet-stream)
- **Resource template support** for dynamic access via URI patterns
- **Base64 encoded data** for binary resources

### Advanced Features

- **Subscription Management** - Periodic resource update notifications every 10 seconds
- **Logging** - Random log messages at different levels every 20 seconds
- **Stderr Notifications** - Timestamped stderr messages every 30 seconds
- **Proper State Management** - Uses gen_server state to track subscriptions and timers

## Usage

### Compilation

From the erlmcp project root:

```bash
rebar3 compile
```

### Running the Server

The everything server is designed to be used as part of the erlmcp project. You can start it programmatically:

```erlang
%% Start the everything server
{ok, Pid} = everything_server:start_link().

%% The server will register its capabilities and be ready to handle MCP requests
```

### Testing

Run the project tests:

```bash
rebar3 eunit
```

## Implementation Details

### State Management

The server maintains state using a record:

```erlang
-record(state, {
    subscriptions = sets:new(),  % Resource subscriptions
    update_timer,                % Timer for periodic updates
    log_timer,                   % Timer for log messages
    stderr_timer                 % Timer for stderr notifications
}).
```

### Resource Handling

Resources are generated dynamically based on the requested URI:

- Resources 1-100 with alternating mime types
- Text resources contain simple string content
- Binary resources contain base64-encoded data
- Resource templates support parameterized access

### Tool Schemas

All tools include comprehensive JSON schema definitions for input validation:

```erlang
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"message">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Message to echo back">>
        }
    },
    <<"required">> => [<<"message">>]
}
```

## Integration with erlmcp SDK

This example demonstrates proper integration with the erlmcp SDK:

- Uses `erlmcp_server:start_link/2` for server initialization
- Implements all required gen_server callbacks
- Uses erlmcp record definitions from `erlmcp.hrl`
- Follows erlmcp patterns for resource, tool, and prompt registration

## Based On

This implementation is based on the TypeScript everything server example from:
https://github.com/modelcontextprotocol/servers/tree/main/src/everything

The conversion maintains feature parity while following Erlang/OTP best practices and erlmcp SDK patterns.
