# Running erlmcp Locally - Complete Walkthrough

## Tiered Workflow

Before running any server, verify your environment:

### Health Check
```bash
make doctor
```

### Test Tiers (choose one)

**Smoke** (≤2 min): Minimal validation
```bash
make smoke
```

**Quick** (≤10 min): Core validation + integration
```bash
make quick
```

**Full** (≤30 min): All tests
```bash
make test
```

### Then Run a Server

Once tests pass, use one of the one-command starts:

```bash
make run-stdio        # STDIO server
make run-http         # HTTP server (port 3000)
make run-http-sse     # HTTP + SSE server (port 3000)
```

Or use manual approaches (below):

---

This guide shows you how to run erlmcp examples and learn all major features in one end-to-end walkthrough.

## Quick Start (2 minutes)

```bash
# From the erlmcp root directory
make example-mcp-complete
```

This single command:
1. Compiles the entire erlmcp system
2. Runs a complete end-to-end example
3. Demonstrates resources, tools, prompts, secrets, and HTTP transport
4. Shows you exactly how to use erlmcp

**Expected duration**: ~30 seconds of actual execution after compilation

## What You'll Learn

The full surface example (`examples/mcp_complete/`) demonstrates:

### Core Concepts
- **Resources**: How to register and read data sources
- **Tools**: How to register and invoke functions
- **Prompts**: How to create dynamic prompt templates
- **Secrets**: How to safely inject credentials into tools

### Advanced Features
- **Progress Tracking**: Report progress on long-running operations
- **Subscriptions**: Subscribe to resource changes
- **Multiple Transports**: HTTP (and extensible to TCP, WebSocket, etc.)
- **Error Handling**: Proper MCP error codes and responses

### Transport Types Covered
- **HTTP Server**: Built-in HTTP transport on port 8765
- **JSON-RPC 2.0**: Standard message format for all communication

## Architecture

The example demonstrates this flow:

```
┌─────────────────┐
│  MCP Server     │  (HTTP transport on :8765)
│  ┌───────────┐  │
│  │ Resources │  │  3 resources (config, counter, credentials)
│  ├───────────┤  │
│  │ Tools     │  │  2 tools (calculate, fetch_with_secrets)
│  ├───────────┤  │
│  │ Prompts   │  │  1 prompt template (generate_report)
│  └───────────┘  │
└────────┬────────┘
         │ JSON-RPC 2.0
         │
┌────────▼────────┐
│  Test Client    │  (demonstrates each operation)
│  ┌───────────┐  │
│  │ Initialize│  │  Handshake
│  ├───────────┤  │
│  │ List Ops  │  │  List resources/tools/prompts
│  ├───────────┤  │
│  │ Call Ops  │  │  Invoke tools, read resources
│  ├───────────┤  │
│  │ Progress  │  │  Track long operations
│  └───────────┘  │
└─────────────────┘
```

## Step-by-Step Walkthrough

### 1. Build the System

```bash
cd erlmcp
make compile
```

Output:
```
Compiling all apps...
✓ Compilation complete
```

### 2. Run the Full Example

```bash
make example-mcp-complete
```

You'll see the full surface example output showing all major features.

## Learning Paths

### I want to understand Resources

1. Read: `examples/mcp_complete/example.erl` lines 83-118
2. See: `apps/erlmcp_core/src/erlmcp_server.erl` - `add_resource/5`
3. Test: Modify the example to add your own resource

### I want to understand Tools

1. Read: `examples/mcp_complete/example.erl` lines 120-199
2. See: `apps/erlmcp_core/src/erlmcp_server.erl` - `add_tool/5`
3. Test: Add your own tool

### I want to understand Prompts

1. Read: `examples/mcp_complete/example.erl` lines 201-228
2. See: `apps/erlmcp_core/src/erlmcp_server.erl` - `add_prompt/5`

### I want to understand Secrets

1. Read: `examples/mcp_complete/example.erl` lines 165-199 (tool with injection)
2. See: `apps/erlmcp_core/src/erlmcp_secrets.erl`

## Building Your Own MCP Server

After understanding the full surface example, you can:

### Minimal Server (stdio transport)

```erlang
-module(my_mcp_server).

main([]) ->
    % Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    % Create server
    {ok, Server} = erlmcp_server:start_link(
        <<"my-server">>,
        #{
            transport => stdio,
            capabilities => #{resources => true, tools => true}
        }
    ),

    % Add a resource
    erlmcp_server:add_resource(
        Server,
        <<"mcp://hello">>,
        <<"Hello Resource">>,
        fun(_) -> <<"Hello, World!">> end,
        <<"text/plain">>
    ),

    % Keep running
    receive _ -> ok end.
```

## Troubleshooting

### Port Already in Use

```bash
lsof -i :8765 | grep -v PID | awk '{print $2}' | xargs kill -9
```

### Can't Find erlmcp_core

```bash
make distclean
make compile
make example-mcp-complete
```

## Quick Reference

### Running Examples
```bash
make example-mcp-complete          # Full surface example
make example-help                  # Show all examples
```

### Development
```bash
make compile                       # Compile all apps
make test                          # Run all tests
make console                       # Interactive shell
```

## Files Reference

| File | Purpose |
|------|---------|
| `examples/mcp_complete/example.erl` | Full surface example (start here) |
| `examples/mcp_complete/README.md` | Detailed technical documentation |
| `apps/erlmcp_core/src/erlmcp_server.erl` | Server implementation |
| `apps/erlmcp_core/src/erlmcp_client.erl` | Client implementation |
| `apps/erlmcp_transports/src/erlmcp_transport_*.erl` | Transport implementations |

Happy coding!
