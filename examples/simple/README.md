# MCP stdio Server Guide

## Overview

This guide explains how to set up and use the erlmcp stdio server with Claude Desktop, including configuration and usage instructions.

## What is the stdio MCP Server?

The stdio (Standard Input/Output) MCP server is a lightweight implementation of the Model Context Protocol that communicates with Claude Desktop through stdin/stdout pipes. This server provides:

- **Tools**: Functions Claude can call (echo, add, system_info)
- **Resources**: Data sources Claude can read (example.txt file)
- **Prompts**: Reusable templates for common tasks (essay writing prompt)

### stdio vs TCP: Why stdio is Preferred

| Transport | stdio | TCP |
|-----------|-------|-----|
| **Setup** | Simple - just specify a command | Complex - requires host/port configuration |
| **Security** | Runs locally, no network exposure | Opens network port, requires firewall rules |
| **Performance** | Direct process communication | Network overhead |
| **Debugging** | Easy to test with command line | Requires network debugging tools |
| **Deployment** | Single executable | Requires server management |

**stdio is the recommended transport for local MCP servers** because it's simpler, more secure, and performs better for local integrations.

## Building the Server (v2.0 Umbrella Structure)

erlmcp v2.0 uses an umbrella project structure with separate applications. First, compile the erlmcp project:

```bash
# Clone and build the project
git clone <repository-url>
cd erlmcp
rebar3 compile

# Then compile the simple example
cd examples/simple
rebar3 compile
```

The v2.0 structure separates concerns into apps:
- `erlmcp_core` - Core protocol, client/server, JSON-RPC
- `erlmcp_transports` - STDIO, TCP, HTTP transports
- `erlmcp_observability` - Metrics, traces, receipts
- `tcps_erlmcp` - Toyota Code Production System (optional)

See `../v2_migration_guide.md` for details.

## Configuring Claude Desktop

### 1. Locate the Configuration File

Claude Desktop uses an `claud_desktop_config.json` configuration file. The location depends on your operating system:

- **macOS**: `~/Library/Application Support/Claude/claud_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claud_desktop_config.json`
- **Linux**: `~/.config/Claude/claud_desktop_config.json`

### 2. Create or Edit claud_desktop_config.json

Create or edit the `claud_desktop_config.json` file with the following content:

```json
{
  "mcpServers": {
    "erlmcp-simple": {
        "command": "erl",
        "args": [
            "-pa",
            "<ABS PATH>/erlmcp/_build/default/lib/*/ebin",
            "-pa",
            "<ABS PATH>/erlmcp/apps/erlmcp_core/ebin",
            "-pa",
            "<ABS PATH>/erlmcp/apps/erlmcp_transports/ebin",
            "-pa",
            "<ABS PATH>/erlmcp/examples/simple",
            "-eval",
            "code:load_file(simple_server_stdio), simple_server_stdio:start()",
            "-noshell"
        ],
        "cwd": "<ABS PATH>/erlmcp"
    }
  }
}
```

**Note**: v2.0 uses umbrella apps structure. The `-pa` flags now reference:
1. All compiled dependencies in `_build/default/lib/*/ebin`
2. Core app: `apps/erlmcp_core/ebin`
3. Transports app: `apps/erlmcp_transports/ebin`
4. Example directory: `examples/simple`

### 3. Restart Claude Desktop

**⚠️ Important**: After editing the configuration file, you **must restart Claude Desktop** for the changes to take effect. The MCP servers are only loaded during application startup.

1. Quit Claude Desktop completely
2. Restart Claude Desktop
3. Wait for the application to fully load

## Verifying the Server is Running

Once Claude Desktop restarts, the MCP server should be automatically connected. You can verify this by:

1. Looking for any error messages in Claude Desktop
2. Testing the server with the prompts below

## How to Use the Server

### Available Tools

The stdio server provides three tools:

#### 1. Echo Tool

Returns the input message with "Echo: " prefix.

**Example prompt:**

```
Use the echo tool to repeat "Hello, World!"
```

#### 2. Add Tool

Adds two numbers together.

**Example prompt:**

```
Use the add tool to calculate 15 + 27
```

#### 3. System Info Tool

Returns system information including hostname and Erlang version.

**Example prompt:**

```
Use the system_info tool to show me information about this system
```

### Available Resources

#### Example Text File

A sample text resource that demonstrates resource access.

**Example prompt:**

```
Read the example.txt resource and show me its contents
```

### Available Prompts

#### Essay Writing Prompt

A template for generating essay writing prompts.

**Example prompt:**

```
Use the write_essay prompt to create a persuasive essay prompt about climate change
```

## Sample Conversation

Here's a complete example of how to interact with the MCP server:

```
User: Can you use the system_info tool to show me information about this system?

Claude: I'll use the system_info tool to get information about your system.

[Claude calls the system_info tool]

The system information shows:
- Hostname: BNYN-J9MN6KPQG5
- Erlang version: 28
- Status: stdio working!

User: Now use the add tool to calculate 42 + 38

Claude: I'll use the add tool to calculate 42 + 38.

[Claude calls the add tool with arguments {"a": 42, "b": 38}]

The result is 80.

User: Can you read the example.txt resource?

Claude: I'll read the example.txt resource for you.

[Claude reads the example.txt resource]

The example.txt resource contains:
"This is example content from a stdio MCP resource."
```

## Troubleshooting

### Common Issues

1. **Server not appearing**: Ensure you restarted Claude Desktop after editing `mcp.json`
2. **Path errors**: Verify the absolute path to the `ebin` directory is correct
3. **Permission errors**: Make sure the `erl` command is in your PATH
4. **Compilation errors**: Run `rebar3 compile` to ensure the project builds successfully

### Debug Steps

1. Test the server manually:

   ```bash
   cd /path/to/erlmcp
   erl -noshell -pa ebin -eval "simple_server_stdio:start()."
   ```

2. Send a test message:

   ```json
   {"jsonrpc": "2.0", "id": 1, "method": "tools/list"}
   ```

3. Check Claude Desktop logs (if available) for error messages

### Getting Help

If you encounter issues:

1. Verify the erlmcp project compiles successfully
2. Test the server manually from the command line
3. Check that the path in `mcp.json` is correct and absolute
4. Ensure Claude Desktop was restarted after configuration changes

## Next Steps

Once you have the basic server working, you can:

1. Modify the server to add your own tools and resources
2. Explore the erlmcp library for more advanced features
3. Create additional MCP servers for different use cases

The stdio server provides a solid foundation for building more sophisticated MCP integrations with Claude Desktop.
