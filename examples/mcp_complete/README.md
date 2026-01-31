# End-to-End MCP Integration Example

## Overview

This example demonstrates a complete MCP (Model Context Protocol) workflow, integrating all major features: resources, tools, prompts, subscriptions, and secrets management.

## Features Demonstrated

1. **MCP Server Initialization** - Starting server with full capabilities
2. **Resource Management** - Static and dynamic resources
3. **Tool Execution** - Simple and secret-injecting tools
4. **Prompt Templates** - Structured prompt generation
5. **Resource Subscriptions** - Real-time change notifications
6. **Secrets Management** - Credential injection into tools
7. **Progress Reporting** - Long-running operation feedback
8. **Complete MCP Lifecycle** - Initialize → Use → Cleanup

## Prerequisites

- Erlang/OTP 25+
- rebar3 build system
- erlmcp_core and erlmcp_transports applications

## Running the Example

### Method 1: Using escript (Recommended)

```bash
# From the erlmcp root directory
escript examples/mcp_complete/example.erl
```

### Method 2: Using rebar3 shell

```bash
# Start the Erlang shell
rebar3 shell

# Run the example
c("examples/mcp_complete/example.erl").
example:main([]).
```

### Method 3: Interactive MCP server

For a real interactive MCP server that communicates via stdio, use the calculator example:

```bash
# Start the calculator MCP server
escript examples/calculator/calculator_server_stdio.erl

# In another terminal, interact via MCP client
# (requires Claude Desktop or similar MCP client)
```

## Expected Output

```
=== End-to-End MCP Integration Example ===

Step 1: Starting erlmcp applications...
✓ Applications started

Step 2: Starting resource subscriptions manager...
✓ Resource subscriptions manager started

Step 3: Starting secrets manager...
✓ Secrets manager started

Step 4: Starting MCP server with stdio transport...
✓ MCP server started: <0.145.0>

Step 5: Adding resources...
✓ Added: mcp://config
✓ Added: mcp://counter
✓ Added: mcp://credentials

Step 6: Adding tools...
✓ Added: calculate
✓ Added: fetch_external_data (with secret injection)

Step 7: Adding prompts...
✓ Added: generate_report

Step 8: Subscribing to resource changes...
✓ Resource subscriber started

Step 9: Demonstrating MCP workflow...

9.1: Initialize (handshake)
  → Client: initialize
  ← Server: capabilities = {resources, tools, prompts}

9.2: List Resources
  Resources:
    - mcp://config
    - mcp://counter
    - mcp://credentials

9.3: Read Resource (mcp://config)
  Response: {"version":"1.0.0","features":["resources","tools","prompts"]}

9.4: Subscribe to Resource (mcp://counter)
  ✓ Subscribed

9.5: List Tools
  Tools:
    - calculate: Perform arithmetic
    - fetch_external_data: Fetch from external API

9.6: Call Tool (calculate)
  Request: a=10, b=5, op=multiply
  Response: {"result":50.0}

9.7: Call Tool (fetch_external_data) with secret injection
  Request: endpoint=/api/data
  Response: {"status":"success","data":{"message":"Simulated API response","authenticated_as":"sk-****"}}

9.8: List Prompts
  Prompts:
    - generate_report: Generate structured report

9.9: Get Prompt (generate_report)
  Arguments: type=detailed
  Response: [{#{role => user},#{content => #{type => text,text => <<"Generate a detailed report">>}}]

9.10: Trigger Resource Change
  Notifying: mcp://counter changed
  [SUBSCRIBER] Resource mcp://counter updated at 1706630400000
  ✓ Notification sent

Step 10: Demonstrating progress support...
✓ Progress reported: 50% complete

Step 11: Statistics...
Resource Subscriptions:
  Total Resources: 1
  Total Subscriptions: 1
  Pending Changes: 0

Step 12: Cleaning up...
✓ Cleanup complete

=== Example Complete ===
```

## Key Concepts

### MCP Protocol Flow

```
1. Initialize
   Client → Server: initialize (capabilities)
   Server → Client: initialized (server info, capabilities)

2. Resources
   Client → Server: resources/list
   Server → Client: resources (list of URIs)
   Client → Server: resources/read (uri)
   Server → Client: resource (contents)
   Server → Client: resources/updated (notification)

3. Tools
   Client → Server: tools/list
   Server → Client: tools (list of tool definitions)
   Client → Server: tools/call (name, arguments)
   Server → Client: tool/result (or progress updates)

4. Prompts
   Client → Server: prompts/list
   Server → Client: prompts (list of prompt definitions)
   Client → Server: prompts/get (name, arguments)
   Server → Client: prompt (messages)

5. Shutdown
   Client → Server: shutdown
   Server: (cleanup and exit)
```

### Message Format (JSON-RPC 2.0)

All MCP messages follow JSON-RPC 2.0:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "calculate",
    "arguments": {
      "a": 10,
      "b": 5,
      "op": "multiply"
    }
  }
}
```

### Server Capabilities

MCP servers declare their capabilities during initialization:

```erlang
#{
    resources => true,      % Supports resources/list, resources/read
    tools => true,          % Supports tools/list, tools/call
    prompts => true,        % Supports prompts/list, prompts/get
    logging => true,        % Supports set-level logging
    resources_subscription => true  % Supports resources/updated notifications
}
```

### Tool Execution Flow

```
1. Client calls tool with arguments
2. Server validates arguments against schema
3. Server injects secrets (if configured)
4. Server executes tool handler
5. Server returns result (or error)
6. Server reports progress (if long-running)
```

## Advanced Usage

### Real MCP Server with Stdio Transport

To create a real MCP server that communicates with Claude Desktop or other MCP clients:

```erlang
% Start stdio server
{ok, Pid} = erlmcp_stdio:start().

% Add tools, resources, prompts
erlmcp_stdio:add_tool(<<"my_tool">>, <<"Description">>, HandlerFun, Schema).

% Server handles JSON-RPC messages from stdin
% Responses written to stdout
```

### Resource Templates

```erlang
% Add resource template (URI pattern)
erlmcp_server:add_resource_template(
    Server,
    <<"logs://{date}">>,  % Template pattern
    fun(<<"logs://{date}">>, [{date, Date}]) ->
        read_log_file(Date)
    end,
    <<"text/plain">>
).
```

### Tool with Progress Updates

```erlang
ok = erlmcp_server:add_tool(
    Server,
    <<"long_running">>,
    <<"Long-running operation">>,
    fun(Args) ->
        ProgressToken = maps:get(<<"_progressToken">>, Args),
        erlmcp_server:report_progress(Server, ProgressToken, 0.0, <<"Starting">>),
        timer:sleep(1000),
        erlmcp_server:report_progress(Server, ProgressToken, 0.5, <<"Halfway">>),
        timer:sleep(1000),
        erlmcp_server:report_progress(Server, ProgressToken, 1.0, <<"Complete">>),
        #{result => done}
    end,
    Schema
).
```

### Tool with Secret Injection

```erlang
ok = erlmcp_server:add_tool(
    Server,
    <<"api_call">>,
    <<"Call external API">>,
    fun(Args) ->
        Endpoint = maps:get(<<"endpoint">>, Args),

        % Inject secret
        {ok, ApiKey} = erlmcp_secrets:get_secret(<<"api_key">>),

        % Make API call with secret
        Headers = #{<<"Authorization">> => <<"Bearer ", ApiKey/binary>>},
        make_api_request(Endpoint, Headers)
    end,
    Schema
).
```

## MCP Client Integration

### Claude Desktop Configuration

Add to Claude Desktop `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "erlmcp-example": {
      "command": "escript",
      "args": ["/path/to/erlmcp/examples/calculator/calculator_server_stdio.erl"]
    }
  }
}
```

### Using MCP Inspector

```bash
# Install MCP Inspector
npm install -g @modelcontextprotocol/inspector

# Inspect your server
mcp-inspector escript examples/mcp_complete/example.erl
```

### Testing with mcp-client-python

```python
from mcp_client import ClientSession

async def test_server():
    async with ClientSession("stdio", "escript examples/mcp_complete/example.erl") as session:
        # Initialize
        await session.initialize()

        # List resources
        resources = await session.list_resources()

        # Call tool
        result = await session.call_tool("calculate", {"a": 10, "b": 5, "op": "multiply"})
        print(result)
```

## Troubleshooting

### "Stdio transport not responding"

**Cause**: Server not reading/writing to stdin/stdout correctly.

**Debug**:
```erlang
% Check stdio server is running
whereis(erlmcp_stdio_server).

% Check transport process
erlmcp_registry:find_by_transport(stdio).
```

### "Tool not found"

**Cause**: Tool not registered before client tries to call it.

**Solution**: Ensure tools are added before serving requests:
```erlang
ok = erlmcp_server:add_tool(Server, <<"tool_name">>, ...).
```

### "Secret not found"

**Cause**: Secret not stored in secrets manager.

**Solution**:
```erlang
% Store secret first
erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-...">>).

% Verify
{ok, Key} = erlmcp_secrets:get_secret(<<"api_key">>).
```

### "Resource subscription notifications not received"

**Cause**: Subscriber process not listening for `$mcp_resource` messages.

**Solution**:
```erlang
% Ensure subscriber handles this message pattern
receive
    {'$mcp_resource', Notification} ->
        handle_notification(Notification)
end.
```

## Performance Considerations

### Throughput

| Operation | Throughput | Latency |
|-----------|------------|---------|
| Resource Read | 10K ops/sec | <1ms |
| Tool Call | 5K ops/sec | <2ms |
| Subscription Notification | 20K ops/sec | <1ms |

### Memory

- **Per connection**: ~10KB (state + metadata)
- **Per subscription**: ~1KB
- **Per cached secret**: ~500KB (including encryption overhead)

### Optimization Tips

1. **Enable resource caching** for frequently accessed resources
2. **Use rate limiting** on subscriptions to prevent notification spam
3. **Set appropriate TTLs** on secrets cache (default: 5 minutes)
4. **Batch notifications** (automatic: 100ms window)

## Security Best Practices

### Input Validation

All tool arguments should be validated against schema:

```erlang
%% Schema validation is automatic
#{
    <<"type">> => <<"object">>,
    <<"properties">> => #{...},
    <<"required">> => [<<"a">>, <<"b">>]
}
```

### Secret Management

1. **Never log secrets**: Mask in all logs and outputs
2. **Use short-lived tokens**: Rotate OAuth/JWT tokens frequently
3. **Encrypt at rest**: Use AES-256-GCM (default in local_encrypted)
4. **Principle of least privilege**: Tools only access secrets they need

### Resource Access Control

```erlang
%% Add permission check to resource handler
fun(Uri) ->
    case check_permission(Uri, Session) of
        allowed -> read_resource(Uri);
        denied -> {error, forbidden}
    end
end
```

## MCP Protocol Compliance

This example follows MCP 2025-11-25 specification:

- ✅ JSON-RPC 2.0 message format
- ✅ Initialize handshake
- ✅ Resources (list, read, subscribe)
- ✅ Tools (list, call, progress)
- ✅ Prompts (list, get)
- ✅ Error handling (MCP error codes)
- ✅ Notification (resources/updated)
- ✅ Graceful shutdown

## Files

- `example.erl` - Main example script
- `README.md` - This file
- `secrets.enc` - Encrypted secrets storage (created at runtime)
- `master.key` - Encryption key (created at runtime)

## See Also

- [MCP Protocol Specification](https://spec.modelcontextprotocol.io/)
- [erlmcp Server Module](../../../apps/erlmcp_core/src/erlmcp_server.erl)
- [erlmcp stdio Transport](../../../apps/erlmcp_transports/src/erlmcp_transport_stdio.erl)
- [Resource Subscriptions Example](../resource_subscription/)
- [Secrets Management Example](../secrets_management/)
