# Run Scripts - Quick Start MCP Servers

This directory contains helper scripts to quickly start erlmcp servers with different transport configurations.

## Available Scripts

### 1. STDIO Server (`stdio-server.sh`)

**Purpose**: Start MCP server with standard input/output transport.

**Usage**:
```bash
./scripts/run/stdio-server.sh
# OR
make run-stdio
```

**Transport**: stdin/stdout (JSON-RPC 2.0)

**Use Cases**:
- Claude Desktop integration
- MCP Inspector testing
- Child process communication

**Example Integration** (Claude Desktop config):
```json
{
  "mcpServers": {
    "erlmcp": {
      "command": "/path/to/erlmcp/scripts/run/stdio-server.sh"
    }
  }
}
```

---

### 2. HTTP Server (`http-server.sh`)

**Purpose**: Start MCP server with HTTP POST transport.

**Usage**:
```bash
./scripts/run/http-server.sh [PORT]
# OR
make run-http
```

**Arguments**:
- `PORT` - HTTP port (default: 3000)
- `HTTP_PORT` env var - Override port

**Transport**: HTTP POST to `http://localhost:3000/mcp`

**Use Cases**:
- Web applications
- REST API integration
- Testing with curl/Postman

**Example Request**:
```bash
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "clientInfo": {
        "name": "test-client",
        "version": "1.0"
      }
    }
  }'
```

---

### 3. HTTP + SSE Server (`http-sse-server.sh`)

**Purpose**: Start MCP server with HTTP + Server-Sent Events.

**Usage**:
```bash
./scripts/run/http-sse-server.sh [PORT]
# OR
make run-http-sse
```

**Arguments**:
- `PORT` - HTTP port (default: 3000)
- `HTTP_PORT` env var - Override port

**Transports**:
- HTTP POST: `http://localhost:3000/mcp`
- SSE Stream: `http://localhost:3000/events`

**Use Cases**:
- Real-time notifications
- Progress updates
- Resource change subscriptions
- Long-running operations

**Example SSE Subscription**:
```bash
# Subscribe to events
curl -N http://localhost:3000/events

# In another terminal, trigger updates
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"long_running_task","arguments":{"duration":5}}}'
```

---

## Common Features

All scripts include:
- ✅ Automatic compilation check (compiles if needed)
- ✅ Clear startup messages with configuration
- ✅ Example tools, resources, and prompts
- ✅ Secrets management with demo credentials
- ✅ Graceful Ctrl+C shutdown
- ✅ Usage documentation in script headers

## Quick Reference

| Script | Transport | Port | Real-time | Use Case |
|--------|-----------|------|-----------|----------|
| `stdio-server.sh` | stdin/stdout | N/A | No | Claude Desktop, MCP Inspector |
| `http-server.sh` | HTTP POST | 3000 | No | Web apps, REST APIs |
| `http-sse-server.sh` | HTTP + SSE | 3000 | Yes | Real-time updates, progress |

## Server Capabilities

### STDIO Server
- **Resources**: `file://example.txt`
- **Tools**: `echo`, `add`, `system_info`
- **Prompts**: `write_essay`

### HTTP Server
- **Resources**: `mcp://config`, `mcp://counter`, `mcp://credentials`
- **Tools**: `calculate`, `fetch_external_data`
- **Prompts**: `generate_report`
- **Features**: Secrets management

### HTTP + SSE Server
- **Resources**: `mcp://config`, `mcp://counter`, `mcp://status`
- **Tools**: `calculate`, `long_running_task`
- **Prompts**: `generate_report`
- **Features**: Secrets management, real-time notifications, progress updates

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `HTTP_PORT` | Override HTTP port | 3000 |
| `HTTP_HOST` | Bind address | localhost |

## Troubleshooting

### Port Already in Use
```bash
# Change port
HTTP_PORT=8080 ./scripts/run/http-server.sh

# Or kill process on port
lsof -i :3000 | grep -v PID | awk '{print $2}' | xargs kill -9
```

### Compilation Errors
```bash
# Clean and rebuild
make distclean
make compile

# Then retry
./scripts/run/stdio-server.sh
```

### Script Not Executable
```bash
chmod +x scripts/run/*.sh
```

## Example Workflows

### Test with MCP Inspector
```bash
# Install MCP Inspector
npm install -g @modelcontextprotocol/inspector

# Run STDIO server
mcp-inspector ./scripts/run/stdio-server.sh
```

### Test HTTP Server with curl
```bash
# Start server
./scripts/run/http-server.sh 3000 &
SERVER_PID=$!

# Initialize
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"test","version":"1.0"}}}'

# List tools
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'

# Call tool
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"calculate","arguments":{"a":10,"b":5,"op":"multiply"}}}'

# Cleanup
kill $SERVER_PID
```

### Monitor SSE Events
```bash
# Terminal 1: Start server
./scripts/run/http-sse-server.sh

# Terminal 2: Subscribe to events
curl -N http://localhost:3000/events

# Terminal 3: Trigger long-running task
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"long_running_task","arguments":{"duration":10}}}'

# Watch progress events stream in Terminal 2
```

## See Also

- [examples/mcp_complete/README.md](../../examples/mcp_complete/README.md) - Comprehensive MCP example
- [examples/simple/](../../examples/simple/) - Simple server examples
- [docs/transports.md](../../docs/transports.md) - Transport documentation
- [CLAUDE.md](../../CLAUDE.md) - Development guide

## Integration with Makefile

These scripts are integrated with the Makefile run targets:

```makefile
run-stdio: compile
	@./scripts/run/stdio-server.sh

run-http: compile
	@./scripts/run/http-server.sh

run-http-sse: compile
	@./scripts/run/http-sse-server.sh
```

This allows you to use either:
- Direct script execution: `./scripts/run/stdio-server.sh`
- Make target: `make run-stdio`

Both approaches produce the same result.
